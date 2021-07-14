---
layout: post
title: "Note transposer"
---

This thing allows one to transpose any note in a given tuning to any other tuning (or rather string / fret on a guitar).

```js
const NOTES = ['A', 'A#', 'B', 'C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#'];

const noteOnStringAtFret = (str, fret) => NOTES[(NOTES.indexOf(str) + fret) % NOTES.length];

const chordInTuning = (frets, tuning) => frets.map((fret, stringIdx) => (fret >= 0 && fret <= 24) ? noteOnStringAtFret(tuning[stringIdx], fret) : fret);

const tuning = ['D', 'G', 'C', 'F', 'A', 'D'];

const DREAMSHADE__YOUR_VOICE__INTRO__TAB = [
  [0, x, x, x, x, x],
  [0, x, x, x, x, x],
  [1, 3, 3, x, x, x],
  [0, 2, 2, x, x, x],
  [0, x, x, x, x, x],
  [0, x, x, x, x, x],
  [3, 2, 5, x, x, x],
// part 2
  [x, 2, x, x, x, x],
  [x, 7, x, x, x, x],
  [x, x, 10, x, x, x],
  [x, 0, x, x, x, x],
  [x, x, 7, x, x, x],
  [x, x, 10, x, x, x],
  [x, 0, x, x, x, x],
  [x, x, 9, x, x, x],
  [x, 8, x, x, x, x],
  [x, 10, x, x, x, x],
  [x, 8, x, x, x, x],
  [x, 8, x, x, x, x],
  [x, x, 9, x, x, x],
  [x, 8, x, x, x, x],
  [x, x, x, 9, x, x],
// part 3
  [8, x, x, x, x, x],
  [x, x, 10, x, x, x],
  [8, x, x, x, x, x],
  [x, x, x, 12, x, x],
  [x, x, x, 9, x, x],
  [8, x, x, x, x, x],
  [x, x, 10, x, x, x],
  [8, x, x, x, x, x],
  [x, x, x, 12, x, x],
  [x, x, x, 9, x, x],
  [7, x, x, x, x, x],
  [x, x, 9, x, x, x],
  [7, x, x, x, x, x],
  [x, x, x, 12, x, x],
  [7, x, x, x, x, x],
  [x, x, 10, x, x, x],
// part 4
  [x, 0, x, x, x, x],
  [x, x, 7, x, x, x],
  [x, x, 10, x, x, x],
  [x, 0, x, x, x, x],
  [x, x, 7, x, x, x],
  [x, x, 10, x, x, x],
  [x, 0, x, x, x, x],
  [x, x, 9, x, x, x],
  [x, 8, x, x, x, x],
  [x, 10, x, x, x, x],
  [x, 8, x, x, x, x],
  [x, 8, x, x, x, x],
  [x, x, 9, x, x, x],
  [x, 8, x, x, x, x],
  [x, x, x, 9, x, x],
  [x, x, 10, x, x, x],
// part 5
  [8, x, x, x, x, x],
  [x, x, 10, x, x, x],
];
```
