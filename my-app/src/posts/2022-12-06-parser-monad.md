---
layout: post
title: "Parser monad"
date: "06-12-2022T00:00:00+10:00"
---

Few years ago I did a course project for the "Functional programming" course at Jagiellonian University.
The project was to implement a program for solving systems of equations... in Haskell.

For me back then, the entire concept of monads was quite foreign and scary, so I implemented a very crude
state machine to parse the equations as strings from STDIN:

```hs
parseRow :: String -> Integer -> [Integer] -> [String] -> ([Integer], [String])
parseRow [] state coefficients var_names
	| state == 7 = (reverse coefficients, reverse var_names)
	| otherwise = error ("Invalid equation (state: " ++ show state ++ "; coefficients: " ++ show coefficients ++ "; var_names: " ++ show var_names ++ ")")

parseRow (c:cs) state coefficients var_names
	| (state == 1) && (c == '-') = parseRow cs 2 ((-1) : coefficients) var_names
	| state == 1 && isNum c = parseRow cs 3 (c_int : coefficients) var_names
	| state == 1 && isAlpha c = parseRow cs 4 (1 : coefficients) ([c] : var_names)
	| state == 2 && isNum c = parseRow cs 3 (repl_k : drop 1 coefficients) var_names
	| state == 2 && isAlpha c = parseRow cs 4 coefficients ([c] : var_names)
	| (state == 3) && (c == '=') = parseRow cs 5 coefficients var_names
	| state == 3 && isAlpha c = parseRow cs 4 coefficients ([c] : var_names)
	| state == 3 && isNum c = parseRow cs 3 (new_k : drop 1 coefficients) var_names
	| (state == 4) && (c == '-') = parseRow cs 2 ((-1) : coefficients) var_names
	| (state == 4) && (c == '+') = parseRow cs 2 (1 : coefficients) var_names
	| state == 4 && isAlphaNum c = parseRow cs 4 coefficients (new_v : drop 1 var_names)
	| (state == 4) && (c == '=') = parseRow cs 5 coefficients var_names
	| (state == 5) && (c == '-') = parseRow cs 6 ((-1) : coefficients) var_names
	| state == 5 && isNum c = parseRow cs 7 (c_int : coefficients) var_names
	| state == 6 && isNum c = parseRow cs 7 (repl_k : drop 1 coefficients) var_names
	| state == 7 && isNum c = parseRow cs 7 (new_k : drop 1 coefficients) var_names
	| otherwise = parseRow cs state coefficients var_names
	where
		k = abs $ head coefficients
		k_sign = sign (head coefficients)
		c_int = atoi c
		new_k = k_sign * ((k * 10) + c_int)
		repl_k = k_sign * c_int
		v = if not (null var_names) then head var_names else []
		new_v = v ++ [c]

parseLine :: String -> ([Integer], [String])
parseLine s = parseRow s 1 [] []
```

However, recently I (hopefully) got a grip of monads, so I decided to modify my old project in Haskell
to make it prettier.

One of the improvements I made was the parsing part. Since one of the requirements for the course project
was to not use any third-party libraries, I decided to stick to this and implement a parser monad.

This proved to be an interesting task and made the entire parsing part quite neat:

```hs
equationFactor :: Parser Fraction
equationFactor = do
    _sign <- factorSign

    zeroOrMore (sat isSpace)

    factor <- fmap (fromMaybe (1%1)) (zeroOrOne rationalFactor)

    return (_sign * factor)

equationMember :: Parser (Fraction, String)
equationMember = do
    factor <- equationFactor

    zeroOrMore (sat isSpace)
    zeroOrOne (sat (== '*'))
    zeroOrMore (sat isSpace)

    nameFirst <- oneOrMore (sat isAlpha)
    nameRest <- zeroOrMore (sat isAlphaNum)

    zeroOrMore (sat isSpace)

    return (factor, nameFirst ++ nameRest)

-- An equation consists of a list of pairs (factor, variable name) and a free member
equation :: Parser ([(Fraction, String)], Fraction)
equation = do
    members <- oneOrMore equationMember

    zeroOrMore (sat isSpace)
    sat (== '=')
    zeroOrMore (sat isSpace)

    freeMember <- rationalFactor

    return (members, freeMember)
```

This is all possible thanks to the `Parser` monad. Under the cut - the implementation details.

<!--more-->

<div class="content-read-marker" data-fraction="25"></div>

This material is heavily based on the [Dave Sands' lectures](https://www.youtube.com/watch?v=H7aYfGP76AI) about parsing in Haskell.

The main idea is that a parser is an annotated function from an input string to _maybe_ something (parsing result) and a string (the unparsed part):

```hs
newtype Parser a = P (String -> Maybe (a, String))
```

Here, `P` is just the type constructor.

So the function wrapped by the `Parser` monad takes a `String` as an input, parses as much as possible and returns what it managed to parse
together with the rest of the string, which was not parser. The two possible results hence are `Just (a, String)` representing the parsed data (`a` part of `Maybe (a, String)`)
and the unparsed remainder of a string (`String` part of `Maybe (a, String)`). The other possible result is `Nothing`, meaning the input string can not be parsed
with a given parser function.

Based on this explanation, one can build combinations of the parsers using the chaining of the `Maybe` monad and the boolean logic (`AND` and `OR` operations, predominantly).

## Helper functions

There are few trivial parsers which to help on this endeavour:

* `success a` - always succeeds, returning a `Just (a, str)` (some default value and an entire input string) for any input string
* `failure` - always fails, returning `Nothing` for any input string
* `item` - parses any one arbitrary character, returning this character wrapped in `Just (chr, str)`
  (first character and the rest of the string) if the input string is non-empty or `Nothing` if the input string is empty
* `parser1 <|> parser2` - tries the first parser, `parser1` and if it succeeds - returns the result,
  but if it fails - returns whatever running the second parser, `parser2` returns
* `parser1 >>= (\x -> parser2)` - chains two parsers together, runs `parser1` and passes its output string to the second parser, `parser2`;
  but since a parser result is a tuple, not just a string that can be passed to the second parser, this function takes a _function_
  from the output of the first parser to the second parser
* `parserFn2 <*> parser1` - chains two parsers together, runs `parser1` (yes, in the reverse order) and applies `parserFn2` to the result;
  where `parserFn2` is a function wrapped in a parser (`Parser (a -> a)`)

Since a parser is just a function `String -> Maybe (a, String)`, in order to actually __parse__ something, one needs to __run__ it.
Running a parser is as easy as executing a function `parse`, passing it a parser and an input string:

```hs
>>> parse (success 1) "abc"
Just (1,"abc")

>>> parse (failure) "abc"
Nothing

>>> parse item "abc"
Just ('a',"bc")

>>> parse (item <|> success '!') "abc"
Just ('a',"bc")

>>> parse (item <|> success '!') ""
Just ('!',"")

>>> parse (item >>= (\_ -> item)) "abc"
Just ('b',"c")

>>> parse (item >>= (\_ -> item)) "abc"
Just ('b',"c")

>>> parse (success (\x -> if x == '-' then (-1) else 1) <*> item) "-123"
Just (-1,"123")

>>> parse (success (\x -> if x == '-' then (-1) else 1) <*> item) "123"
Just (1,"23")
```

The aforementioned helper functions are defined as following:

```hs
success :: a -> Parser a
success a = P (\str -> Just (a, str))

failure :: Parser a
failure = P (\_ -> Nothing)

item :: Parser Char
item = P $ \str -> case str of
  "" -> Nothing
  (ch:chs) -> Just (ch, chs)

instance Alternative Parser where
  empty = failure

  p1 <|> p2 = P $ \str -> case parse p1 str of
    Nothing -> parse p2 str
    result -> result

instance Monad Parser where
  p1 >>= p2 = P $ \str -> case parse p1 str of
    Just (a, str') -> parse (p2 a) str'
    Nothing -> Nothing

  return = pure
```

<div class="content-read-marker" data-fraction="50"></div>

## Basic parsers

These basic building blocks are then extended with a bit more complex building blocks, which are a little bit more handy for building
complex parsers:

* `sat (ch -> Bool)` - returns a parser which only succeeds if the first character of a (non-empty) input string satisfies the function (`ch -> Bool`)
* `zeroOrMore p` - returns a parser which succeeds if the parser `p` (passed as a param) succeeds on the input string (potentially multiple times)
or fails - regardless, the resulting parser will succeed
* `zeroOrOne p` - returns a parser which succeeds if the parser `p` (passed as a param) succeeds exactly once on an input string or if it fails
* `oneOrOne p` - returns a parser which succeeds if the parser `p` (passed as a param) succeeds once or more on an input string

With these building blocks, one can build more complex parsers:

### Parsing a digit

```hs
>>> digit = sat isDigit
>>> parse digit "123abc"
Just ('1',"23abc")
```

### Parsing a number (as a string)

```hs
>>> numberStr = oneOrMore (sat isDigit)
>>> parse numberStr "123abc"
Just ("123","abc")
```

Since `Parser` is an instance of `Monad`, you can use `fmap` or `<$>` to combine it with other functions:

### Parsing a number (as a non-negative number)

```hs
>>> naturalNumber :: Parser Integer; naturalNumber = read <$> oneOrMore (sat isDigit)
>>> parse naturalNumber "123abc"
Just (123,"abc")
```

### Parsiung a potentially negative number

```hs
>>> sign = fmap (maybe 1 (\_ -> -1)) (zeroOrOne (sat (== '-')))
>>> intNumber = ((*) <$> sign) <*> naturalNumber
>>> parse intNumber "-123abc"
Just (-123,"abc")
```

The code above requires a bit of an explanation, I guess.
The first part,

```hs
sign = fmap (maybe 1 (\_ -> -1)) (zeroOrOne (sat (== '-')))
```

parses a potential `-` sign at the beginning of a string and returns either `1` or `-1`, aka the sign multiplier.
The tricky part is `fmap (maybe 1 (\_ -> -1)) ...` - the `zeroOrOne` parser will return `Maybe a` -- that is, `Maybe (Maybe a, String)`.
So if the wrapped part (in this case - `sat (== '-')`) is present in the string, it will return `Just a` (in this case - `Just '-'`).
Hence we need to cast this `Maybe Char` to something reasonable - a number would do. We call the `maybe` helper with two params - the first
one is what would be returned if it is applied to `Nothing` and the second one is a function which will be called on the value wrapped in `Just`
the thing is applied to:

```hs
>>> maybe 1 (\_ -> -1) (Just '-')
-1
>>> maybe 1 (\_ -> -1) Nothing
-1
>>> maybe False (\ch -> ch == '-') (Just '-')
True
>>> maybe True (\ch -> ch == '-') Nothing
True
```

The `fmap` bit then applies this function (returned by `maybe 1 (\_ -> -1)`) to the value wrapped by the next argument:

```hs
>>> fmap (maybe 1 (\_ -> -1)) (Just (Just '-'))
Just (-1)
>>> fmap (maybe 1 (\_ -> -1)) (Just Nothing)
Just 1
```

This very same code could be rewritten as follows:

```hs
(maybe 1 (\_ -> -1)) <$> (Just Nothing)
```

Finally, the second part:

```hs
((*) <$> sign) <*> number
```

The left part of it is just like the previously rewritten function from `fmap` to `<$>`, so this entire line can be written down as follows:

```hs
(fmap (*) sign) <*> number
```

What it does is applies the `(*)` function (multiplication) to the value wrapped in the second argument.
In this case it is `sign`, which is a `Parser Integer`. This is the tricky bit: the type of this expression is __not__ `Parser Integer`.
It is `Parser (a -> a)`, a parser of a function. This function, wrapped in a `Parser` type, can be applied to whatever other parser returns
and hence chained together.
The `<*>` operator, as mentioned before, chains the two parsers. So the entire expression applies the multiplication operation to the value
returned by the `sign` parser and the value returned by the `number` parser.

TL;DR: the whole thing _analyzes_ the first character of a string (without consuming it) and returns either `1` or `-1`; it then multiplies this value by the number
returned by the `number` parser.

```hs
>>> parse intNumber "-123"
Just (-123,"")
>>> parse intNumber "123"
Just 123
```

Using a `<|>` operator, one can parse integer (both negative and non-negative) numbers in this weird manner:

```hs
negativeNumber = (sat (== '-')) >> (* (-1)) <$> read <$> oneOrMore (sat isDigit)
positiveNumber = read <$> oneOrMore (sat isDigit)
number2 = negativeNumber <|> positiveNumber
```

```hs
>>> parse number2 "-42"
Just (42,"")
>>> parse number2 "123"
Just (123,"")
```

<div class="content-read-marker" data-fraction="75"></div>

## Combining parsers

To give a few practical examples of combining the parsers into something meaningful, consider the parser of an integer equation:

```
-3x1 + 4*x2 = 5
```

There are few helpful smaller parsers mentioned above, like `intNumber`.

But we would need a bit more than that: an equation is a series of members (`-3x1`, `4*x2`), where each member is a combination of a factor (`-3`, `4`) and a variable (`x1`, `x2`).
There might be either an addition or subtration between members.
Lastly, equation must end with an equal sign followed by a free-standing number.

Let's see if we can come up with a plan for this.

We can start at the bottom and define parsers for each bit of an equation or start from the top and define the parser for an entire equation and go down to every member and factor.
Let's start with the small bits, the members:

```hs
equationMember :: Parser (Integer, String)
equationMember = do
    -- first, the numeric factor
    factor <- equationFactor -- TODO

    -- then, optionally, a multiplication sign, potentially surrounded by any number of whitespace
    zeroOrMore (sat isSpace)
    zeroOrOne (sat (== '*'))
    zeroOrMore (sat isSpace)

    -- followed by the variable name - only letters first, then either letters or numbers
    nameFirst <- oneOrMore (sat isAlpha)
    nameRest <- zeroOrMore (sat isAlphaNum)

    -- potentially surrounded by any number of whitespace
    zeroOrMore (sat isSpace)

    -- lastly, return the tuple - numeric factor and the name of a variable
    return (factor, nameFirst ++ nameRest)
```

This should be relatively straightforward to follow.
We do not have the `equationFactor` defined which is used in this parser, so here it is:

```hs
equationFactor :: Parser Integer
equationFactor = do
    -- optionally, a sign (+ or -)
    _sign <- factorSign -- TODO

    -- surrounded by any number of whitespace
    zeroOrMore (sat isSpace)

    -- followed by a number
    factor <- fmap (fromMaybe 1) (zeroOrOne intNumber)

    -- combine the numeric factor and a member sign (+ or -) for a final factor number
    return (_sign * factor)
```

The `factorSign` is yet to be implemented. The reason why I extract it into a separate parser is because one-liner would look ugly:

```hs
factorSign :: Parser Integer
factorSign = positiveSign <|> negativeSign <|> (success 1)

positiveSign = fmap (\_ -> 1) (sat (== '+'))

negativeSign = fmap (\_ -> -1) (sat (== '-'))
```

This works by either matching the `+` sign and converting it to `1` or matching the `-` sign and converting it to `-1` or succeeding with `1` (by default, when there is no sign).

Lastly, an entire equation parser would be a rather simple combination of the above parsers:

```hs
equation :: Parser ([(Integer, String)], Integer)
equation = do
    -- first, match all the members as a list of tuples (factor, variable name)
    members <- oneOrMore equationMember

    -- potentially surrounded by any number of whitespace, the '=' sign
    zeroOrMore (sat isSpace)
    sat (== '=')
    zeroOrMore (sat isSpace)

    -- the last, free-standing member
    freeMember <- intNumber

    -- finally, return the tuple of members and a free-standing member
    return (members, freeMember)
```

You can try it on the equation from the example above:

```hs
>> parse equation "-3x1 + 4*x2 = 5"
Just (([(-3,"x1"),(4,"x2")],5),"")
```

The finished project contains a little bit more - it implements rational calculus (rational factors instead of just pure integers).
The entire code is also hosted on [GitHub](https://github.com/shybovycha/gauss-elimination).

<div class="content-read-marker" data-fraction="100"></div>
