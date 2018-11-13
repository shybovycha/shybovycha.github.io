---
layout: post
title: Entanglement2
date: '2018-11-11T11:42:24+10:00'
---

In order to generate valid connections for a tile, we need to have twelve pairs of integers between 0 and 11 (inclusively), where none of the pairs contains same number as any other pair and there are no duplications. There are two ways of doing this:

1. we either keep track of numbers already used in the generated results and use only numbers which were not yet used
2. alternatively, we take a list of numbers between 0 and 11, shuffle it and split into chunks of two elements

The second approach is less memory and time consuming, so let's stick to that one.

JavaScript does not provide a way to get a random number of range, so we'll need to implement a helper function for that purpose:

```js
function randomInt(max) {
  return Math.floor(Math.random() * Math.floor(max));
}
```

A function to shuffle array items _(mutating the source array)_:

```js
function shuffle(a) {
  for (let i = 0; i < a.length - 2; ++i) {
    const lastElementIndex = a.length - 1 - i;

    const t = randomInt(lastElementIndex);

    const tmp = a[t];
    a[t] = a[lastElementIndex];
    a[lastElementIndex] = tmp;
  }

  return a;
}
```

What happening here is: we iterate the list indices from the pre-last to the first one, take the random index from the rest and swap those two elements. This way the iterated element will always contain a new random element, which is different from itself.

And splitting array into chunks of two elements could be implemented like this:

```js
function splitIntoChunks(a, n) {
  let acc = [];

  for (let i = 0; i < a.length; ++i) {
    const chunkIndex = Math.floor(i / n);

    if (!acc[chunkIndex]) {
      acc[chunkIndex] = [];
    }

    acc[chunkIndex].push(a[i]);
  }

  return acc;
}
```

Let's say we have these permutations:

```
[3, 5]
[6, 7]
[8, 10]
[0, 2]
[9, 1]
[11, 4]
```

In GeoGebra I've used these three functions to help me with _(with the help of [this article](https://archive.geogebra.org/en/upload/files/steve_arnold/beziers.html))_:

```
Bezier(a,b,t)=a*(1-t)+b*t
Bezier2(a,b,c,t)=Bezier(a,b,t)*(1-t)+Bezier(b,c,t)*t
```

And then, to show the connections:

```
connections1={
Curve[Bezier2(x(C3),x(O),x(C5),t),Bezier2(y(C3),y(O),y(C5),t),t,0,1],
Curve[Bezier2(x(C6),x(O),x(C7),t),Bezier2(y(C6),y(O),y(C7),t),t,0,1],
Curve[Bezier2(x(C8),x(O),x(C10),t),Bezier2(y(C8),y(O),y(C10),t),t,0,1],
Curve[Bezier2(x(C0),x(O),x(C2),t),Bezier2(y(C0),y(O),y(C2),t),t,0,1],
Curve[Bezier2(x(C9),x(O),x(C1),t),Bezier2(y(C9),y(O),y(C1),t),t,0,1],
Curve[Bezier2(x(C11),x(O),x(C4),t),Bezier2(y(C11),y(O),y(C4),t),t,0,1]
}
```

Rotating this tile left by one sixth of it will result in changed connections:

```
[3, 5] -> [1, 3]
[6, 7] -> [4, 5]
[8, 10] -> [6, 8]
[0, 2] -> [10, 0]
[9, 1] -> [7, 11]
[11, 4] -> [9, 2]
```

Which results in new curves:

```
connections2={
Curve[Bezier2(x(C1),x(O),x(C3),t),Bezier2(y(C1),y(O),y(C3),t),t,0,1],
Curve[Bezier2(x(C4),x(O),x(C5),t),Bezier2(y(C4),y(O),y(C5),t),t,0,1],
Curve[Bezier2(x(C6),x(O),x(C8),t),Bezier2(y(C6),y(O),y(C8),t),t,0,1],
Curve[Bezier2(x(C10),x(O),x(C0),t),Bezier2(y(C10),y(O),y(C0),t),t,0,1],
Curve[Bezier2(x(C7),x(O),x(C11),t),Bezier2(y(C7),y(O),y(C11),t),t,0,1],
Curve[Bezier2(x(C9),x(O),x(C2),t),Bezier2(y(C9),y(O),y(C2),t),t,0,1]
}
```

The formulae looks like this: `nextIndex = ((prevIdex + 12) - 2) % 12` and the function rotating the tile' connections could be implemented like this:

```js
function rotateLeft(a) {
  for (let i = 0; i < a.length; ++i) {
    a[i][0] = (a[i][0] + 12 - 2) % 12;
    a[i][1] = (a[i][1] + 12 - 2) % 12;
  }

  return a;
}
```

A very small change is required to implement `rotateRight`:

```js
function rotateRight(a) {
  for (let i = 0; i < a.length; ++i) {
    a[i][0] = (a[i][0] + 2) % 12;
    a[i][1] = (a[i][1] + 2) % 12;
  }

  return a;
}
```

Assume we have an existing path coming from `C6' -> C2'`:

```
path' = Curve[Bezier2(x(C6'),x(O'),x(C2'),t),Bezier2(y(C6'),y(O'),y(C2'),t),t,0,1]
```

In first case, the path will be continued at `C9 -> C1`. If we rotate the tile to the left one time, it will become `C9 -> C2`.

## Drawing tiles

HTML layout:

```html
<canvas></canvas>
```

JavaScript skeleton:

```js
const canvas = document.querySelector('canvas');
const ctx = canvas.getContext('2d');

function drawTile(ctx, tileSize, u, v) {
    ctx.beginPath();
    // ...
    ctx.closePath();

    ctx.fillStyle = 'rgba(255, 255, 255, 1.0)';
    ctx.strokeStyle = "rgba(0, 0, 0, 1.0)";
    ctx.lineWidth = 4;

    ctx.fill();
}
```

Now, according to these calculations <IMAGE GOES HERE>, fill the missing code:

```js
function drawTile(ctx, tileSize, u, v) {
  ctx.beginPath();

  const cx = u; // no conversion just for now
  const cy = v;
  const a = tileSize;
  const c = (a * Math.sqrt(3)) / 2;
  const a2 = a / 2;

  ctx.moveTo(cx, cy - a); // A
  ctx.lineTo(cx - c, cy - a2); // B
  ctx.lineTo(cx - c, cy + a2); // C
  ctx.lineTo(cx, cy + a); // D
  ctx.lineTo(cx + c, cy + a2); // E
  ctx.lineTo(cx + c, cy - a2); // F

  ctx.closePath();

  ctx.fillStyle = '#bebebe';
  ctx.strokeStyle = '#000000';
  ctx.lineWidth = 4;

  ctx.fill();
  ctx.stroke();
}
```

Connection points are somewhat tricky though. Draw a few guiding lines on a tile diagram and you'll see that the X coordinate of every connection point is translated by one third of a tile width _(which is equal to a tile size, since a tile is a regular hexagon)_, whilst the Y coordinate has a step of twice as less.

To put it simply, here are the formulae:

```latex
C_0(c_x - \frac{a\sqrt{3}}{6}, c_y + \frac{a}{2} + \frac{a}{3})\\
C_1(c_x - \frac{a\sqrt{3}}{3}, c_y + \frac{a}{2} + \frac{a}{6})\\

C_2(c_x - \frac{a\sqrt{3}}{2}, c_y + \frac{a}{6})\\
C_3(c_x - \frac{a\sqrt{3}}{2}, c_y - \frac{a}{6})\\

C_4(c_x - \frac{a\sqrt{3}}{3}, c_y - \frac{a}{2} - \frac{a}{6})\\
C_5(c_x - \frac{a\sqrt{3}}{6}, c_y - \frac{a}{2} - \frac{a}{3})\\

C_6(c_x + \frac{a\sqrt{3}}{6}, c_y - \frac{a}{2} - \frac{a}{3})\\
C_7(c_x + \frac{a\sqrt{3}}{3}, c_y - \frac{a}{2} - \frac{a}{6})\\

C_8(c_x + \frac{a\sqrt{3}}{2}, c_y - \frac{a}{6})\\
C_9(c_x + \frac{a\sqrt{3}}{2}, c_y + \frac{a}{6})\\

C_{10}(c_x + \frac{a\sqrt{3}}{3}, c_y + \frac{a}{2} + \frac{a}{6})\\
C_{11}(c_x + \frac{a\sqrt{3}}{6}, c_y + \frac{a}{2} + \frac{a}{3})\\
```

Or in JS language:

```js
[cx - (a * (Math.sqrt(3) / 6)), cy + (a / 2) + (a / 3)],
[cx - (a * (Math.sqrt(3) / 3)), cy + (a / 2) + (a / 6)],

[cx - (a * (Math.sqrt(3) / 2)), cy + (a / 6)],
[cx - (a * (Math.sqrt(3) / 2)), cy - (a / 6)],

[cx - (a * (Math.sqrt(3) / 3)), cy - (a / 2) - (a / 6)],
[cx - (a * (Math.sqrt(3) / 6)), cy - (a / 2) - (a / 3)],

[cx + (a * (Math.sqrt(3) / 6)), cy - (a / 2) - (a / 3)],
[cx + (a * (Math.sqrt(3) / 3)), cy - (a / 2) - (a / 6)],

[cx + (a * (Math.sqrt(3) / 2)), cy - (a / 6)],
[cx + (a * (Math.sqrt(3) / 2)), cy + (a / 6)],

[cx + (a * (Math.sqrt(3) / 3)), cy + (a / 2) + (a / 6)],
[cx + (a * (Math.sqrt(3) / 6)), cy + (a / 2) + (a / 3)],
```

Drawing function for now:

```js
function drawTile(ctx, tileSize, u, v, connections) {
  const cx = u; // no conversion just for now
  const cy = v;
  const a = tileSize;
  const c = (a * Math.sqrt(3)) / 2;
  const a2 = a / 2;

  // the tile background, z-index is the lowest of three, thus rendering BEFORE everything else
  {
    ctx.beginPath();

    ctx.moveTo(cx, cy - a); // A
    ctx.lineTo(cx - c, cy - a2); // B
    ctx.lineTo(cx - c, cy + a2); // C
    ctx.lineTo(cx, cy + a); // D
    ctx.lineTo(cx + c, cy + a2); // E
    ctx.lineTo(cx + c, cy - a2); // F

    ctx.closePath();

    ctx.fillStyle = '#bebebe';

    // stroking has no effect on the resulting image here
    // ctx.stroke();
    ctx.fill();
  }

  const connectionPoints = [
    [cx - (a * (Math.sqrt(3) / 6)), cy + (a / 2) + (a / 3)],
    [cx - (a * (Math.sqrt(3) / 3)), cy + (a / 2) + (a / 6)],

    [cx - (a * (Math.sqrt(3) / 2)), cy + (a / 6)],
    [cx - (a * (Math.sqrt(3) / 2)), cy - (a / 6)],

    [cx - (a * (Math.sqrt(3) / 3)), cy - (a / 2) - (a / 6)],
    [cx - (a * (Math.sqrt(3) / 6)), cy - (a / 2) - (a / 3)],

    [cx + (a * (Math.sqrt(3) / 6)), cy - (a / 2) - (a / 3)],
    [cx + (a * (Math.sqrt(3) / 3)), cy - (a / 2) - (a / 6)],

    [cx + (a * (Math.sqrt(3) / 2)), cy - (a / 6)],
    [cx + (a * (Math.sqrt(3) / 2)), cy + (a / 6)],

    [cx + (a * (Math.sqrt(3) / 3)), cy + (a / 2) + (a / 6)],
    [cx + (a * (Math.sqrt(3) / 6)), cy + (a / 2) + (a / 3)],
  ];

  for (const [startIdx, endIdx] of connections) {
    const startPoint = connectionPoints[startIdx];
    const endPoint = connectionPoints[endIdx];

    // black stroke, 2px thick
    {
      ctx.beginPath();

      ctx.moveTo(startPoint[0], startPoint[1]);

      // bezier with two control points, being same point, the center of a tile
      ctx.bezierCurveTo(cx, cy, cx, cy, endPoint[0], endPoint[1]);

      ctx.strokeStyle = '#000';
      ctx.lineWidth = 8;

      ctx.stroke();
    }

        // the white insides of a line
    {
      ctx.beginPath();

      ctx.moveTo(startPoint[0], startPoint[1]);

      ctx.bezierCurveTo(cx, cy, cx, cy, endPoint[0], endPoint[1]);

      // white line, 4px thick
      ctx.strokeStyle = '#fff';
      ctx.lineWidth = 4;

      ctx.stroke();
    }
  }

  // the tile frame, z-index is higher, thus rendering AFTER all the connection lines
  {
    ctx.beginPath();

    ctx.moveTo(cx, cy - a); // A
    ctx.lineTo(cx - c, cy - a2); // B
    ctx.lineTo(cx - c, cy + a2); // C
    ctx.lineTo(cx, cy + a); // D
    ctx.lineTo(cx + c, cy + a2); // E
    ctx.lineTo(cx + c, cy - a2); // F

    ctx.closePath();

    // ctx.fillStyle = '#bebebe';
    ctx.strokeStyle = '#000000';
    ctx.lineWidth = 6;

    // DO NOT FILL POLYGON - this way all the connection lines will be covered by the fill color
    // ctx.fill();
    ctx.stroke();
  }
}

const canvas = document.querySelector('canvas');
const ctx = canvas.getContext('2d');

drawTile(ctx, 50, 80, 80, [[10, 11]]);
```

Putting everything together:

```js
function randomInt(max) {
  return Math.floor(Math.random() * Math.floor(max));
}

function shuffle(a) {
  for (let i = 0; i < a.length - 2; ++i) {
    const lastElementIndex = a.length - 1 - i;

    const t = randomInt(lastElementIndex);

    const tmp = a[t];
    a[t] = a[lastElementIndex];
    a[lastElementIndex] = tmp;
  }

  return a;
}

function splitIntoChunks(a, n) {
  let acc = [];

  for (let i = 0; i < a.length; ++i) {
    const chunkIndex = Math.floor(i / n);

    if (!acc[chunkIndex]) {
      acc[chunkIndex] = [];
    }

    acc[chunkIndex].push(a[i]);
  }

  return acc;
}

function generateTile() {
  let connections = [];

  for (let i = 0; i < 12; ++i) {
    connections.push(i);
  }

  shuffle(connections);

  return splitIntoChunks(connections, 2);
}

function rotateTileLeft(connections) {
  const result = [];

    for (const [from, to] of connections) {
    result.push([ (from + 10) % 12, (to + 10) % 12 ]);
  }

  return result;
}

function rotateTileRight(connections) {
  const result = [];

    for (const [from, to] of connections) {
    result.push([ (from + 2) % 12, (to + 2) % 12 ]);
  }

  return result;
}

function drawTile(ctx, tileSize, u, v, connections) {
  const cx = u; // no conversion just for now
  const cy = v;
  const a = tileSize;
  const c = (a * Math.sqrt(3)) / 2;
  const a2 = a / 2;

  // the tile background, z-index is the lowest of three, thus rendering BEFORE everything else
  {
    ctx.beginPath();

    ctx.moveTo(cx, cy - a); // A
    ctx.lineTo(cx - c, cy - a2); // B
    ctx.lineTo(cx - c, cy + a2); // C
    ctx.lineTo(cx, cy + a); // D
    ctx.lineTo(cx + c, cy + a2); // E
    ctx.lineTo(cx + c, cy - a2); // F

    ctx.closePath();

    ctx.fillStyle = '#bebebe';

    // stroking has no effect on the resulting image here
    // ctx.stroke();
    ctx.fill();
  }

  const connectionPoints = [
    [cx - (a * (Math.sqrt(3) / 6)), cy + (a / 2) + (a / 3)],
    [cx - (a * (Math.sqrt(3) / 3)), cy + (a / 2) + (a / 6)],

    [cx - (a * (Math.sqrt(3) / 2)), cy + (a / 6)],
    [cx - (a * (Math.sqrt(3) / 2)), cy - (a / 6)],

    [cx - (a * (Math.sqrt(3) / 3)), cy - (a / 2) - (a / 6)],
    [cx - (a * (Math.sqrt(3) / 6)), cy - (a / 2) - (a / 3)],

    [cx + (a * (Math.sqrt(3) / 6)), cy - (a / 2) - (a / 3)],
    [cx + (a * (Math.sqrt(3) / 3)), cy - (a / 2) - (a / 6)],

    [cx + (a * (Math.sqrt(3) / 2)), cy - (a / 6)],
    [cx + (a * (Math.sqrt(3) / 2)), cy + (a / 6)],

    [cx + (a * (Math.sqrt(3) / 3)), cy + (a / 2) + (a / 6)],
    [cx + (a * (Math.sqrt(3) / 6)), cy + (a / 2) + (a / 3)],
  ];

  for (const [startIdx, endIdx] of connections) {
      const startPoint = connectionPoints[startIdx];
    const endPoint = connectionPoints[endIdx];

    // black stroke, 2px thick
    {
      ctx.beginPath();

      ctx.moveTo(startPoint[0], startPoint[1]);

      // bezier with two control points, being same point, the center of a tile
      ctx.bezierCurveTo(cx, cy, cx, cy, endPoint[0], endPoint[1]);

      ctx.strokeStyle = '#000';
      ctx.lineWidth = 8;

      ctx.stroke();
    }

        // the white insides of a line
    {
      ctx.beginPath();

      ctx.moveTo(startPoint[0], startPoint[1]);

      ctx.bezierCurveTo(cx, cy, cx, cy, endPoint[0], endPoint[1]);

      // white line, 4px thick
      ctx.strokeStyle = '#fff';
      ctx.lineWidth = 4;

      ctx.stroke();
    }
  }

  // the tile frame, z-index is higher, thus rendering AFTER all the connection lines
  {
    ctx.beginPath();

    ctx.moveTo(cx, cy - a); // A
    ctx.lineTo(cx - c, cy - a2); // B
    ctx.lineTo(cx - c, cy + a2); // C
    ctx.lineTo(cx, cy + a); // D
    ctx.lineTo(cx + c, cy + a2); // E
    ctx.lineTo(cx + c, cy - a2); // F

    ctx.closePath();

    // ctx.fillStyle = '#bebebe';
    ctx.strokeStyle = '#000000';
    ctx.lineWidth = 6;

    // DO NOT FILL POLYGON - this way all the connection lines will be covered by the fill color
    // ctx.fill();
    ctx.stroke();
  }
}

const canvas = document.querySelector('canvas');
const ctx = canvas.getContext('2d');

drawTile(ctx, 50, 80, 80, generateTile());
```
