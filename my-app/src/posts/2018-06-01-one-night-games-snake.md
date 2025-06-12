---
layout: post
title: 'One-night games: snake'
---

[source code](https://github.com/shybovycha/moo-snake)

<img src="" alt="" data-src="/images/one-night-games/snake-screenshot.webp" class="medium">

Under the cut you can find the interesting algorithmic solutions I've mentioned.

<!--more-->

## Snake

[Source code](https://github.com/shybovycha/moo-snake)

Snake is a very simple game to implement. The core is the field, which is a binary matrix of whether there is something to draw in a specific position or there is nothing. The game is tightly bound to the timer, since it is almost a turn-based game and the movement speed should increase as the snake grows. The only things which might be interesting are: snake movement _(in other words, how do we update the game matrix on each timer tick)_, snake growth _(or what happens when a snake collides with the food)_ and the collision test _(or how to determine what the snake collided with)_.

The snake movement is quite simple - try thinking of a snake as of a stack of `Vector2` objects, having just the coordinates of each body part of a snake:

```js
class Point {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }

    equals(other) {
        return ((this.x === other.x) && (this.y === other.y));
    }
}

class Snake {
    constructor(points, direction, maxX, maxY) {
        this.points = points;
        this.direction = direction;
        this.maxX = maxX;
        this.maxY = maxY;
    }

    advance() {
        this.points.shift();
        this.points.push(this.nextPoint());
    }

    nextPoint() {
        let { x, y } = this.points.last();

        if (this.direction === 'up') {
            y--;
        } else if (this.direction === 'right') {
            x++;
        } else if (this.direction === 'down') {
            y++;
        } else if (this.direction === 'left') {
            x--;
        }

        if (x < 0) {
            x = this.maxX;
        } else if (x > this.maxX) {
            x = 0;
        }

        if (y < 0) {
            y = this.maxY;
        } else if (y > this.maxY) {
            y = 0;
        }

        return new Point(x, y);
    }
}
```

On every timer tick we should "advance" the snake in the direction of movement by simply adding a new element to the top of a stack and remove the last one, representing snake's tail. When a player hits any of the arrow keys, we simply set the movement direction for a snake, which will be considered on a next timer tick:

```js
class Game {
    constructor(rows, cols) {
        this.rows = rows;
        this.cols = cols;
        this.score = 0;
        this.snake = this._createSnake();
        this.food = this._generateFood();
    }

    updateState() {
        const nextSnakePoint = this.snake.nextPoint();

        if (this.food.equals(nextSnakePoint)) {
            this.score++;
            this.snake.points.push(nextSnakePoint);
            this.food = this._generateFood();
        }

        this.snake.advance();
    }
}
```

There is a need for one more bounds check: when a snake is about to cross the boundaries of a game field, we need to change the next point a snake is about to advance to the opposite "wall" _(if we want such a feature)_.

The game ends when a snake hits one of its body parts. The check for that is really simple: we just need to check whether the next point is contained in the stack of a snake:

```js
isGameOver() {
    const p = this.snake.nextPoint();
    return this.snake.points.find(e => e.equals(p));
}
```

To render a field, we need to render the food and each of snake's body parts:

```js
class Renderer {
    _render() {
        this._clearCanvas();

        for (let i = 0; i < this.game.snake.points.length; i++) {
            const { x, y } = this.game.snake.points[i];
            this._drawRectangle(x * this.cellSize, y * this.cellSize, this.cellSize, this.cellSize);
        }

        this._drawRectangle(this.game.food.x * this.cellSize, this.game.food.y * this.cellSize, this.cellSize, this.cellSize);
    }
}
```

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
