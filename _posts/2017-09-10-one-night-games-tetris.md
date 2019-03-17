---
layout: post
title: 'One-night games: Tetris'
date: '2018-06-01T16:00:00+10:00'
---

Did you ever think of how old games like tetris or snake _(hail the Nokia fans!)_ work?

I always wanted to write a few relatively simple games just to practice my coding skills.
Back when I was at the uni, I've made [peg solitaire](https://en.wikipedia.org/wiki/Peg_solitaire) and [three-in-a-row](https://en.wikipedia.org/wiki/Bejeweled) games in C/C++ with SFML. I even wrote a _(pretty shitty)_ server-client [chess game](https://github.com/shybovycha/moo-chess) _(be warned: this code was written in 2011 and only contains fixes to make it run on a new version of SFML, so it might look... smelly... on each and every line...)_. My latest achievements were the [Entanglement](https://github.com/shybovycha/entanglement-ios) game and a well-designed _(at least from my perspective)_ [arcade space shooter](https://github.com/shybovycha/python-shooter).

But I've never implemented more well-known games like tetris or snake. And they are interesting from the algorithmic perspective.

And recently I've came to the point when I really wanted to create something interesting. And that was the chance to try myself as a early-2000-ies developer!

As I wanted to accomplish my goal as soon as possible, I've decided to not pick up any libraries and just implement everything with HTML5 Canvas API. And this is what I ended up with:

<img src="" alt="" data-src="/images/one-night-games/snake-screenshot.png" class="medium">

In this post I'll talk about Tetris. Under the cut you'll find the interesting algorithmic solutions I've used for it.

<!--more-->

## Tetris

[Source code](https://github.com/shybovycha/moo-tetris)

The most interesting part about tetris was to implement piece rotation. I've represented all the possible figures as a list of matrices.

```js
const FIGURES = [
    [[1, 1, 1, 1]],
    [[1, 1], [1, 1]],
    [[1, 1, 1], [0, 0, 1]],
    [[1, 1, 1], [1, 0, 0]],
    [[1, 1, 0], [0, 1, 1]],
    [[0, 1, 1], [1, 1, 0]],
    [[0, 1, 0], [1, 1, 1]]
];
```

The problem however is that all figures are different, hence all the matrices have different dimensions - a "stick" is 4x1, whilst a "cube" is 2x2. Other pieces are 3x2. Rotation algorithm might not be simple. And it implies one limitation - pieces should preserve their position - not to move to the sides when rotating them.

Pieces are best represented as matrices, but in order to operate on matrices we need to have the corresponding methods. I decided to just implement the class `Piece` with the essential methods:

```js
class Piece {
    constructor(row, col, shape) {
        this.piece = [].slice.apply(shape);
        this.rows = shape.length;
        this.cols = shape[0].length;
        this.col = col;
        this.row = row;
    }
}
```

Here `row` and `col` describe the position of a piece on a screen whilst `rows` and `cols` are the dimensions of a piece. `piece` contains the matrix data of a piece.

The rotation algorithm I've implemented tries to transpose piece's matrix in a very simple manner:

```js
class Piece {
    rotate() {
        let result = new Array(this.cols);

        for (let i = 0; i < this.cols; i++) {
            if (!result[i]) {
                result[i] = [];
            }

            for (let t = 0; t < this.rows; t++) {
                result[i][t] = this.figure[t][this.cols - 1 - i];
            }
        }

        this.figure = result;
        this.rows = result.length;
        this.cols = result[0].length;
    }
}
```

What it does is actually creating a totally new matrix, where the dimensions are swapped and the data is the opposite data to what was in the original matrix. In other words, we iterate the original matrix over its normal indices, but write the data to the new matrix in the reversed direction. So if we read the row with index `0` and column with index `2`, this means we need to write the data from the source matrix at `data[0][2]` to the destination matrix at `data[2][0]`. Sounds correct, right?

But if we do that with the array, we will end up having the new matrix mirrored:

```
           given:

      0   1   2   3
    +---+---+---+---+
 0  | 1 | 2 | 3 | 4 |
    +---+---+-------+
 1  | 5 | 6 | 7 | 8 |
    +---+---+---+---+


            expectation:

      0   1                0    1
    +---+---+             +---+---+
 0  | 4 | 8 |          0  | 5 | 1 |
    +---+---+             +---+---+
 1  | 3 | 7 |          1  | 6 | 2 |
    +---+---+    or       +---+---+
 2  | 2 | 6 |          2  | 7 | 3 |
    +---+---+             +---+---+
 3  | 1 | 5 |          3  | 8 | 4 |
    +---+---+             +---+---+



    reality:

      0   1
    +---+---+
 0  | 1 | 5 |
    +---+---+
 1  | 2 | 6 |
    +---+---+
 2  | 3 | 7 |
    +---+---+
 3  | 4 | 8 |
    +---+---+
```

Thus we need to copy the data to the new matrix in the reverse order along one axis.

In order to allow for a two-way rotation I've introduced a numeric argument to the `Piece#rotate` method, defining which way _(clockwise or counter-clockwise)_ the piece will be rotated:

```js
rotate(direction) {
    // ...

    for (let t = 0; t < this.rows; t++) {
        result[i][t] = direction > 0 ? this.figure[t][this.cols - 1 - i] : this.figure[this.rows - 1 - t][i];
    }

    // ...
}
```

The other interesting algorithm is collision detection. This is slightly more complex than just checking boundariesof a figure. Since we can not think of a regular (circle, rectangle, line) shape for the field formed by the previous pieces, we will need to check each cell of a field against each cell of a currently falling piece. But if we decide to do that - we should either do this before rendering the falling piece (to check whether we are allowed to render it) and then, in the case of collision we will be made to "cancel" the piece's move down one cell. Alternatively, we could check each cell of a field agains the data of a falling piece, but with a one row shift down:

```
what we draw:

+-------+
|   xx  |
|    x  |
|    x  |
|   *   |
| ****  |
| ***** |
+-------+

what we check:

+-------+
|       |
|   ..  |
|    .  |
|   *.  |
| ****  |
| ***** |
+-------+

same example,
but with the
collision occuring:

+-------+
|       |
|       |
|   ..  |
|   *.  |
| ***!  |
| ***** |
+-------+
```

The check is trivial: if both field cell at the checked position and any of the piece data at the piece position plus one row shift down are non-zero - then we have detected a collision and we should not move the piece down, but just render it and generate the next piece.
