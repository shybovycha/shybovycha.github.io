---
layout: post
title: 'Data compression'
tags: [programming, algorithms]
---

I have been curious about how data compression works for a very long time - back in the day there was an archiver race, with WinRar and KGB archiver being insanely good at it. Bear in mind, those were the days where you had to use 1.44MB 3.5" magnetic "floppy" disks to transfer data - that was the era before DVD and even CD disks. And what WinRar could offer was no joke - you could compress 14MB Doom 2 and split it into 5 _tomes_, write each of them to a separate disk and then unarchive them on another computer - archiver would prompt you to insert the next disk with the next tome when it is done decompressing the current one.

But I digress. I was curious how WinRar can do that and how the size of the data is actually reduced.

And recently I got back to this topic. What I found was actually quite interesting.

Back in the day I had an idea to transform the data like so: for each character, replace it with a short code followed by the number of times it is repeated in a row:

```
AAAAABBB (8 characters)

  |
  v

A5B3 (4 characters - 50% compression!)
```

This is exactly what [run-length encoding](https://en.wikipedia.org/wiki/Run-length_encoding) is doing.
The problem with this solution was that even this very text does not contain a whole lot of repeated characters in a row, so it is quite inefficient:

```
Hello world (11 characters)

     |
     v

H1e1l2o1 1w1o1r1l1d1 (20 characters - negative 100% compression)
```

The next idea I had was to represent each character's original binary code with a shorter one:

```
Hello world

H (72 = 01001000 => 000000001)
e (101 = 01100101 => 00000010)
l (108 = 01101100 => 00000011) <------\
l (108 = 01101100 => 00000011) <-----/|
o (111 = 01101111 => 00000100) <-\    |
  (32 = 00100000 => 00000101)    |    |
w (119 = 01110111 => 00000110)   |    |
o (111 = 01101111 => 00000100) <-/    |
r (114 = 01110010 => 00000101)        |
l (108 = 01101100 => 00000011) <------/
d (100 = 01100100 => 00000111)

Original:

Hello world
01001000 01100101 01101100 01101100 01101111 00100000 01110111 01101111 01110010 01101100 01100100

New:

00000001 00000010 00000011 00000011 00000100 00000101 00000110 00000100 00000111 00000011 00001000
```

On itself this does not present any compression, but what if characters were packed into half-bytes? The longest short code is only 4 bits, so we could get rid of most of zeros:

```
Old:

0000'0001 0000'0010 0000'0011 0000'0011 0000'0100 0000'0101 0000'0110 0000'0100 0000'0111 0000'0011 0000'1000

(11 bytes)

New:

0001'0010 0011'0011 0100'0101 0110'0100 0111'0011 1000'0000

(6 bytes)
```

This is a reduction of about 50%! But this method does not preserve the mapping of characters to short codes.
If we were to add this mapping, the message size would be actually much larger:

```
01001000 00000001 01100101 00000010 01101100 00000011 01101111 00000100 00100000 00000101 01110111 00000110 01110010 00000111 01100100 00001000
H        code     e        code     l        code     o        code     <space>  code     w        code     r        code     d        code
```

This is already `16` bytes. Then add the `6` bytes for the encoded message and you get `22` bytes - a negative 100% compression again.

If the initial text was much, **much** longer, this could actually be an improvement, but here's the challenge: once the number of unique letters in the initial text is larger than `15` (the maximum number of variants that could be represented by `4` bits), the compression rate drops below zero - simply because we can not represent every character in less than a full byte anymore.

Literal decades of break and I learned about [Huffman encoding](https://en.wikipedia.org/wiki/Huffman_coding).
The idea is somewhat similar - represent each letter of the input text with a short(er) code.
The algorithm looks like this: get all the unique letters of the input text, order them by the number of their occurrences in the text, in decreasing order:

```
l => 3
o => 2
d => 1
r => 1
w => 1
" " => 1
e => 1
H => 1
```

Within this list, order the pairs with the same number of occurrences by the character code, in ascending order:

```
l => 3
o => 2
" " => 1
H => 1
d => 1
e => 1
r => 1
w => 1
```

Imagine each of these entries as a node:

```
(l, 3) (o, 2) (" ", 1) (H, 1) (d, 1) (e, 1) (r, 1) (w, 1)
```

Then, take a pair of nodes with the smallest number of occurrences each and merge them into a new node with the number of occurrences as a sum of two original nodes:

```
                                                 (2)
                                               /     \
(l, 3) (o, 2) (" ", 1) (H, 1) (d, 1) (e, 1) (r, 1) (w, 1)

becomes

(l, 3) (o, 2) (" ", 1) (H, 1) (d, 1) (e, 1)      (2)
                                               /     \
                                            (r, 1) (w, 1)
```

Repeat this process until there is just one node left:

```
                                   (2)
                                 /     \
(l, 3) (o, 2) (" ", 1) (H, 1) (d, 1) (e, 1)      (2)
                                               /     \
                                            (r, 1) (w, 1)

becomes

(l, 3) (o, 2) (" ", 1) (H, 1)  (2)              (2)
                             /     \          /     \
                         (d, 1)  (e, 1)    (r, 1) (w, 1)
```

And again:

```
                     (2)
                   /     \
(l, 3) (o, 2) (" ", 1) (H, 1)  (2)              (2)
                             /     \          /     \
                         (d, 1)  (e, 1)    (r, 1) (w, 1)

becomes

(l, 3)  (2)   (o, 2)      (2)               (2)
      /     \           /     \           /     \
 (" ", 1) (H, 1)    (d, 1)  (e, 1)     (r, 1) (w, 1)
```

There are four candidates for the smallest amongst the `(2)` nodes: `(o, 2)` and three internal nodes; in this case the `(o, 2)` node will become the left branch (because the ordering of the characters - none of the internal nodes has the character assigned, but the `(o, 2)` node has it; its code is lexicographically larger than the empty character, assuming empty character has code of `0`):

```
            (4)
          /     \
(l, 3)  (2)   (o, 2)      (2)               (2)
      /     \           /     \           /     \
 (" ", 1) (H, 1)    (d, 1)  (e, 1)     (r, 1) (w, 1)

becomes

            (4)      (l, 3)      (2)            (2)
          /     \              /     \        /     \
        (2)   (o, 2)        (r, 1) (w, 1)  (d, 1)  (e, 1)
      /     \
 (" ", 1) (H, 1)
```

Note how the node `(l, 3)` has moved to the middle of the list, between `(4)` and `(2)` nodes - this is needed to preserve the ordering by the number of occurrences.

There are two more nodes `(2)`, join them:

```
                                        (4)
                                         |
                                    /----------\
                                   /            \
            (4)      (l, 3)      (2)            (2)
          /     \              /     \        /     \
        (2)   (o, 2)        (r, 1) (w, 1)  (d, 1)  (e, 1)
      /     \
 (" ", 1) (H, 1)

becomes

            (4)                   (4)        (l, 3)
          /     \                  |
        (2)   (o, 2)          /----------\
      /     \                /            \
 (" ", 1) (H, 1)           (2)            (2)
                         /     \        /     \
                      (r, 1) (w, 1)  (d, 1)  (e, 1)
```

Now the smallest two nodes are `(l, 3)` and `(4)`:

```
                                        (7)
                                         |
                                    /---------\
                                   /           \
            (4)                   (4)        (l, 3)
          /     \                  |
        (2)   (o, 2)          /----------\
      /     \                /            \
 (" ", 1) (H, 1)           (2)            (2)
                         /     \        /     \
                      (r, 1) (w, 1)  (d, 1)  (e, 1)

becomes

                  (7)                      (4)
                   |                     /     \
              /---------\              (2)   (o, 2)
             /           \           /     \
            (4)        (l, 3)   (" ", 1) (H, 1)
             |
        /----------\
       /            \
     (2)            (2)
   /     \        /     \
(r, 1) (w, 1)  (d, 1)  (e, 1)
```

And lastly there are `(7)` and `(4)` nodes to merge:

```
                              (11)
                               |
                     /--------------------\
                    /                      \
                  (7)                      (4)
                   |                     /     \
              /---------\              (2)   (o, 2)
             /           \           /     \
            (4)        (l, 3)   (" ", 1) (H, 1)
             |
        /----------\
       /            \
     (2)            (2)
   /     \        /     \
(r, 1) (w, 1)  (d, 1)  (e, 1)
```

Then, following the rule _"left branch -> '0', right branch -> '1'"_, mark each branches of this tree:

```
                              (11)
                               |
                      /------------------\
                    <0>                  <1>
                    /                      \
                  (7)                      (4)
                 /   \                    /   \
               <0>   <1>                <0>   <1>
               /       \                /       \
             (4)     (l, 3)          (2)     (o, 2)
            /   \                    /   \
          <0>   <1>                <0>   <1>
          /       \                /       \
         /         \          (" ", 1)    (H, 1)
       (2)         (2)
      /   \        /  \
    <0>   <1>    <0>   <1>
     |     |      |     |
  (r, 1) (w, 1) (d, 1) (e, 1)
```

Lastly, by following the branches from the root node of this tree, gather the branch labels into the binary code for each of the leaf nodes:

```
(l, 3, 0b01)  (o, 2, 0b11)  (" ", 1, 0b100)  (H, 1, 0b0101)  (d, 1, 0b0010)  (e, 1, 0b0011)  (r, 1, 0b0000)  (w, 1, 0b0001)
```

This is called Huffman encoding or Huffman tree.
Using these codes, the initial text `Hello world` is encoded as following:

```
H   e    l  l  o      w    o  r    l  d
101 0011 01 01 11 100 0001 11 0000 01 0010

joining into bytes

10100110 10111100 00011100 00010010
```

This could be programmed in Ruby like so:

```rb
s = 'Hello world'
a = s.chars.uniq.map {|c| {char: c, count: s.chars.count(c)}}.sort {|a, b| [b[:count], a[:char]] <=> [a[:count], b[:char]]}
pq = a.map { |c| {char: c[:char], count: c[:count], left: nil, right: nil} }

while pq.size > 1 do
  left, right = pq.pop(2)

  if left[:count] < right[:count]
    left, right = right, left
  end

  pq << {char: nil, count: left[:count] + right[:count], left: left, right: right}

  pq.sort! {|a,b| [b[:count], a[:char] || ''] <=> [a[:count], b[:char] || '']}
end

q = [{node: pq[0], code: ''}]

codes = {}

while q.size > 0 do
  e = q.shift

  if e[:node][:left].nil? && e[:node][:right].nil?
    codes[e[:node][:char]] = e[:code]
    next
  end

  q.push({node: e[:node][:left], code: e[:code] + '0'}) if !e[:node][:left].nil?

  q.push({node: e[:node][:right], code: e[:code] + '1'}) if !e[:node][:right].nil?
end
```

or in C++:

```cpp
#include <string>
#include <map>
#include <queue>
#include <iostream>

struct Node {
    char c;
    int count;
    Node* left;
    Node* right;
};

struct CodeNode {
    Node* node;
    std::string code;
};

int main() {
    const std::string s = "Hello world";

    std::map<char, int> freq;

    for (auto i : s) {
        if (freq.find(i) == freq.end()) {
            freq[i] = 1;
        } else {
            freq[i]++;
        }
    }

    auto cmpNode = [](Node* a, Node* b) {
        if (a->count != b->count) {
            return a->count > b->count;
        }

        return a->c < b->c;
    };

    std::priority_queue<Node*, std::vector<Node*>, decltype(cmpNode)> pq(cmpNode);

    for (auto i : freq) {
        pq.push(new Node{.c = i.first, .count = i.second, .left = nullptr, .right = nullptr});
    }

    while (pq.size() > 1) {
        auto* left = pq.top();
        pq.pop();

        auto* right = pq.top();
        pq.pop();

        if (left->count < right->count) {
            std::swap(left, right);
        }

        pq.push(new Node{ .c = '\0', .count = left->count + right->count, .left = left, .right = right });
    }

    std::queue<CodeNode*> q;

    q.push(new CodeNode{ .node = pq.top(), .code = std::string() });

    std::map<char, std::string> codes;

    while (!q.empty()) {
        auto* e = q.front();
        q.pop();

        if (!e->node->left && !e->node->right) {
            codes[e->node->c] = e->code;
            continue;
        }

        if (e->node->left) {
            q.push(new CodeNode{ .node = e->node->left, .code = e->code + "0" });
        }

        if (e->node->right) {
            q.push(new CodeNode{ .node = e->node->right, .code = e->code + "1" });
        }
    }

    for (auto i : codes) {
        std::cout << "'" << i.first << "'" << ": " << i.second << std::endl;
    }

    return 0;
}
```

If the Huffman table is known, the source could be decoded by traversing from the root: if the next _bit_ in the input sequence is `0` - traverse the left child of the current node, if the next bit is `1` - traverse the right child of the current node; if the leaf node is reached - emit a corresponding character:

```
======= TBD =======
```

If the initial message (`Hello world`) is encoded with this algorith (to four bytes: `0xA6 0xBC 0x1C 0x12`), in order to properly decode it, the tree or the code table has to be given alongside with the encoded message:

```
message: 'Hello world' (11 bytes)
encoded: 0xA6 0xBC 0x1C 0x12 (4 bytes)
table:   {"l"=>"01", "o"=>"11", " "=>"100", "H"=>"101", "r"=>"0000", "w"=>"0001", "d"=>"0010", "e"=>"0011"} (8 keys + 8 values = 16 bytes)
compression = 11 bytes -> 20 bytes = negative 100%
```

This is not entirely a compression algorith, though. Algorithms such as ZIP (DEFLATE) and JPEG (although slightly different in details, the general approach still applicable) go further to compress the _table_ itself.
To understand how they work, one needs to understand the tricks behind them.

## Canonical Huffman codes

There is one issue with just general Huffman encoding: there are multiple ways to build a _different_ tree for the same input.
Refer to the early stage of encoding from above:

```
(l, 3)  (2)   (o, 2)      (2)               (2)
      /     \           /     \           /     \
 (" ", 1) (H, 1)    (d, 1)  (e, 1)     (r, 1) (w, 1)
```

Here's the non-deterministic part: which pair of nodes to merge next? Depending on the choice, the code for each character would be different (although the _code length_ would remain the same).

That's where [canonical Huffman codes](https://en.wikipedia.org/wiki/Canonical_Huffman_code) come into play: this simple algorithm unifies the logic, transforming any valid Huffman tree for a given input into the same tree (so the codes remain the same).

To obtain canonical codes from _any_ valid Huffman tree for a given input, the codes are transformed following two simple rules:

1. sort the codes by code length (ascending) and then by character (also ascendig)
2. starting with `current_code = 0` and `current_length = 0`, iterate over each _code length_ (assume iterator is `length_i`):
  1. if the length is greater than `current_length`, left shift `current_code` by the difference `length_i - current_length`
  2. increment `current_code`, filling the unused left-most bits with zeroes
  3. update `current_length` to `length_i`

In the example above, the codes are:

```
l:   01
o:   11
" ": 100
H:   101
r:   0000
w:   0001
d:   0010
e:   0011
```

The _lengths_ of those codes are:

```
l:   2 (Huffman code: '01', length of '01' = 2)
o:   2 (Huffman code: '11', length of '11' = 2)
" ": 3 (Huffman code: '100', length of '100' = 3)
H:   3 (Huffman code: '101', length of '101' = 3)
r:   4 (Huffman code: '0000', length of '0000' = 4)
w:   4 (Huffman code: '0001', length of '0001' = 4)
d:   4 (Huffman code: '0010', length of '0010' = 4)
e:   4 (Huffman code: '0011', length of '0011' = 4)
```

Making these codes canonical requires sorting by _Huffman code's length_ first and then by _character_ (I have left old codes in braces for reference):

```
l:   2 (old code: 01, its length = 2)
o:   2 (old code: 11, its length = 2)
" ": 3 (old code: 100, its length = 3)
H:   3 (old code: 101, its length = 3)
d:   4 (old code: 0010, its length = 4)
e:   4 (old code: 0011, its length = 4)
r:   4 (old code: 0000, its length = 4)
w:   4 (old code: 0001, its length = 4)
```

Then, re-assign codes, starting with `0` and padding to the length of old code:

```
l:   00 (old code length = 2, next code = 00 + 1 = 01)
o:   01 (old code length = 2, next code = 01 + 1 = 10)
" ": 100 (old code length = 3; but previous entry ('o' = 01) has length 2, so shift left by 1 bit ('10' << 1 == '100'); next code = 100 + 1 = 101)
H:   101 (old code length = 3; next code = 101 + 1 = 110)
d:   1100 (old code length = 4; but previous entry ('H' = 101) has length 3, so shift left by 1 bit ('110' << 1 == '1100'); next code = 1100 + 1 = 1101)
e:   1101 (old code length = 4; next code = 1101 + 1 = 1110)
r:   1110 (old code length = 4; next code = 1110 + 1 = 1111)
w:   1111 (old code length = 4)
```

Interestingly, this code did not use the previous codes themselves.
This implies the codes could be calculated automatically if only the _lengths_ of the codes are given.

For the example above, the original table is:

```
```

...

## ZIP, DEFLATE

DEFLATE algorithm compresses the list of codes lengths using a constant table, defined by the standard (or algorithm description, if you will).

The constant table is defined in the archiver code itself, so it remains the same for all archives and does not need to be a part of archive itself.

The archive, on the other hand, stores a list of 256 values, each representing the length of the code of the corresponding character.

For example, archive may store a list like this:

```
0, 0, 2, 2, 4, 0, 0, 0, 0, ... (total 256 elements, all the rest are zeros)
```

But this is highly inefficient, so this list is _also_ comressed using Huffman encoding. The difference is that this list is first compressed using run-length encoding (from the very top of this blog) and the result is compressed using the pre-defined Huffman table.

Step-by-step this looks like this: first, the original content is compressed. This produces a Huffman table.
Assume this table of canonical Huffman codes for input `Hello world`:

```
l  : "00"
o  : "01"
" ": "100"
H  : "101"
d  : "1100"
e  : "1101"
r  : "1110"
w  : "1111"
```

As described above, for *canonical* Huffman codes, storing only codes' lengths is enough to reconstruct the _codes_:

```
"00"   : 2
"01"   : 2
"100"  : 3
"101"  : 3
"1100" : 4
"1101" : 4
"1110" : 4
"1111" : 4
```

So the table of _codes' lengths_ looks like this:

```
1: 0 (codes of length 1: none)
2: 2 (codes of length 2: two codes)
3: 2 (codes of length 3: two codes)
4: 4 (codes of length 4: four codes)
```

This table looks very much like an indexed array (1-based, though - indexes start at `1` instead of `0` like in C), so it could be represented as an array.

Given this data, at any point Huffman codes could be re-created:

```
code = 0

// lengths: [0, 2, 2, 4]

length_i = 1, codes_length_i = 0;
  no codes of length 1 => skip

length_i = 2, codes_length_i = 2;
  code (0) << 1 => code = 00;
  create <codes_length_i (2)> codes:
    emit code (00), code += 1 (code = 01);
    emit code (01), code += 1 (code = 10);

length_i = 3, codes_length_i = 2;
  code (10) << 1 => code = 100;
  create <codes_length_i (2)> codes:
    emit code (100), code += 1 (code = 101)
    emit code (101), code += 1 (code = 110);

length_i = 4, codes_length_i = 4;
  code (110) << 1 => code = 1100;
  create <codes_length_i (4)> codes:
    emit code (1100), code += 1 (code = 1101);
    emit code (1101), code += 1 (code = 1110);
    emit code (1110), code += 1 (code = 1111);
    emit code (1111), code += 1 (code = 1100);
```

So for the list of codes lengths `[0, 2, 2, 4]` the re-created codes will be

```
00
01
100
101
1100
1101
1110
1111
```

For the second phase, the _original symbols_ have to be correlated with these codes somehow.
Referring back to the compressed content's table of codes:

```
l  : "00" (code length: 2)
o  : "01" (code length: 2)
" ": "100" (code length: 3)
H  : "101" (code length: 3)
d  : "1100" (code length: 4)
e  : "1101" (code length: 4)
r  : "1110" (code length: 4)
w  : "1111" (code length: 4)
```

In a similar manner to codes lengths' table, a table of all possible `256` byte values (ASCII table, effectively) is created and the corresponding byte's code length is written to this table:

```
0x00: 0
0x01: 0
0x02: 0
...
0x20 (" "): 3
0x48 (H)  : 3
0x64 (d)  : 4
0x65 (e)  : 4
0x6c (l)  : 2
0x6f (o)  : 2
0x72 (r)  : 4
0x77 (w)  : 4
...
```

Instead of hexadecimal values, here's _decimal_ representation:

```
32  (" ") : 3
72  (H)   : 3
100 (d)   : 4
101 (e)   : 4
108 (l)   : 2
111 (o)   : 2
114 (r)   : 4
119 (w)   : 4
```

This list could be written as follows:

```
index: [0, 1, 2, 3, 4, ..., 31, 32, 33, 34, 35, ..., 71, 72, 73, ..., 99, 100, 101, 102, ..., 108, ..., 111, 112, 113, 114, ..., 119, ..., 255 ]
value: [0, 0, 0, 0, 0, ..., 0,  3,  0,  0,  0,  ...,  0,  3,  0, ...,  0,   4,   4,   0, ...,   2, ...,   2,   0,   0,   4, ...,   4, ..., 0 ]
```

This list contains a lot of duplications and a lot of zeroes. It is then compressed using the run-length algorithm (from the very top of this blog), which **now** makes sense:

```
(0, 31), (3, 1), (0, 39), (3, 1), (0, 27), (4, 1), (4, 1), (0, 6), (2, 1), (0, 2), (2, 1), (0, 2), (4, 1), (0, 4), (4, 1), (0, 136)
```

But DEFLATE does it smarter - it uses pre-defined table of symbols, again:

- for zero value, use zero itself
- for values 1..15, use the value itself as a code
- to repeat previous value 3..6 times, use code `16` and `3` extra bits to represent how many repetitions (`000` - repeat previous value 3 times, `001` - repeat previous value 4 times, `010` - repeat previous value 5 times, `011` - repeat previous value 6 times)
- to repeat value `0` 3..10 times, use code `17` and extra bits
- to repeat value `0` 11..138 times, use code `18` and extra bits

For example, the number `0` repeated `31` times would be represented as `0 (18, extra bits: 30 - 11)`:

- `0` - raw value, no previous value to repeat
- `(18, extra bits: 30 - 11)` - repeat previous value `0` 30 times; code `18` represents 1..138 repetitions, so extra bits `0000000` would translate to '11 repetitions', `0000001` would translate to '12 repetitions' etc, `0010011` (`30 - 11 = 19`, `19` binary = `0010011`) translates to '30 repetitions', `01111111` translates to '138 repetitions'

The trick here is that all of these values fit nicely into one byte each:

- `0` -> `0b00000000`
- `1`..`15` -> `0b00000001` .. `0b00001111`
- `16` -> `0b00010000`, extra 3 values:  `0b00010001`, `0b00010010`, `0b00010011`
- `17` -> `0b00010001`, extra bits 
- `18` -> `0b00010010`, extra bits


In case of the array of codes' lengths above:

```
(0, 31)
(3, 1)
(0, 39)
(3, 1)
(0, 27)
(4, 1)
(4, 1)
(0, 6)
(2, 1)
(0, 2)
(2, 1)
(0, 2)
(4, 1)
(0, 4)
(4, 1)
(0, 136)

becomes encoded:

(0, 31)  -> [0, (18, extra bits: 30 - 11)]
(3, 1)   -> 3
(0, 39)  -> [0, (18, extra bits: 38 - 11)]
(3, 1)   -> 3
(0, 27)  -> [0, (18, extra bits: 26 - 11)]
(4, 1)   -> [4, 4] - previous value (4) is repeated just once, emit the value itself
(0, 6)   -> [0, (16, extra bits: 5 - 3)]
(2, 1)   -> 2
(0, 2)   -> [0, 0]
(2, 1)   -> 2
(0, 2)   -> [0, 0]
(4, 1)   -> 4
(0, 4)   -> [0, (16, extra bits: 3 - 3)]
(4, 1)   -> 4
(0, 136) -> [0, (18, extra bits: 135 - 11)]

in raw bytes:

[0, (18, extra bits: 30 - 11)]
3
[0, (18, extra bits: 38 - 11)]
3
[0, (18, extra bits: 26 - 11)]
[4, 4] - previous value is repeated just once, emit the value itself
[0, (16, extra bits: 5 - 3)]
2
[0, 0]
2
[0, 0]
4
[0, (16, extra bits: 3 - 3)]
4
[0, (18, extra bits: 135 - 11)]
```

## JPEG, DHT

Define Huffman Table stores the list of codes lengths using an index-as-value approach - a list of `16` values effectively translated as _"element at index 'i' tells how many codes of length 'i' are there in the table"_.

For example, an array like this

```
[0, 0, 2, 2, 4, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0]
```

is expanded like so:

```
index: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
value: [0, 0, 2, 2, 4, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0]

codes of length 0: 0
codes of length 1: 0
codes of length 2: 2
codes of length 3: 2
codes of length 4: 4
codes of length 5: 0
codes of length 6: 0
codes of length 7: 0
codes of length 8: 0
codes of length 9: 0
codes of length 10: 0
codes of length 11: 0
codes of length 12: 0
codes of length 13: 0
codes of length 14: 0
codes of length 15: 0
```