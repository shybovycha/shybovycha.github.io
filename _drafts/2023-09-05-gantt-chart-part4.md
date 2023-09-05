---
layout: post
title: "Gantt chart. Part 4"
date: '2023-09-05T14:01:24+10:00'
---

Seems like every two years or so I hop on [my Gantt chart implementation](https://github.com/shybovycha/gantt-chart/) and rework it completely.

Last few attempts were alright, but I was never quite satisfied with the implementation - be it SVG, which has a toll on a browser and has quite limited customization functionality or Canvas API, with same limited customization but being fast.

<img src="/images/gantt_chart_part3/gantt-chart-v1.webp" loading="lazy" alt="First revision of Gantt chart">

<img src="/images/gantt_chart_part3/gantt-chart-v2.webp" loading="lazy" alt="Second revision of Gantt chart">

<img src="/images/gantt_chart_part3/screenshot.webp" loading="lazy" alt="Third revision of Gantt chart">

With the recent introduction of grid layouts in CSS, now supported [in all browsers](https://caniuse.com/css-grid), now seems like a perfect time to revisit the old implementations once again.

<!--more-->

The data for the tests is going to look like this:

```js
export const data = [
  {
    id: 1,
    name: "epic 1"
  },
  {
    id: 2,
    name: "epic 2"
  },
  {
    id: 3,
    name: "epic 3"
  },
  {
    id: 4,
    name: "story 1",
    parent: 1
  },
  {
    id: 5,
    name: "story 2",
    parent: 1
  },
  {
    id: 6,
    name: "story 3",
    parent: 1
  },
  {
    id: 7,
    name: "story 4",
    parent: 2
  },
  {
    id: 8,
    name: "story 5",
    parent: 2
  },
  {
    id: 9,
    name: "lorem ipsum dolor atata",
    parent: 5
  },
  {
    id: 10,
    name: "task 2",
    parent: 5
  }
];
```

The main component, `<Gantt>`, initially was implementated as follows:

```jsx
import React, { useMemo } from "react";

import style from "./gantt.module.css";

const LeftPaneRow = ({ id, name }) => {
  return <div className={style.row}>{name}</div>;
};

const LeftPane = ({ items }) => {
  return (
    <div className={style.left_pane}>
      <div className={style.left_pane_header}>/</div>

      <div className={style.left_pane_rows}>
        {items.map((item) => (
          <LeftPaneRow key={item.id} {...item} />
        ))}
      </div>
    </div>
  );
};

const RightPaneRow = ({ id, name }) => {
  return (
    <div className={style.row}>
      <div className={style.entry} style={{ left: 0 }}>
        {id}
      </div>
    </div>
  );
};

const RightPane = ({ items }) => {
  return (
    <div className={style.right_pane}>
      <div className={style.right_pane_header}>...scale...</div>
      <div className={style.right_pane_rows}>
        {items.map((item) => (
          <RightPaneRow key={item.id} {...item} />
        ))}
      </div>
    </div>
  );
};

export const flattenTree = (items) => {
  const queue = [];

  items.filter(({ parent }) => !parent).forEach((item) => queue.push(item));

  const result = [];
  const visited = new Set();

  while (queue.length > 0) {
    const item = queue.shift();

    if (visited.has(item.id)) {
      continue;
    }

    result.push(item);
    visited.add(item.id);

    items
      .filter((child) => child.parent === item.id)
      .forEach((child) => queue.unshift(child));
  }

  return result;
};

export const Gantt = ({ items }) => {
  const itemList = useMemo(() => flattenTree(items), [items]);

  return (
    <div className={style.gantt}>
      <LeftPane items={itemList} />
      <RightPane items={itemList} />
    </div>
  );
};
```

The core of the proper representation of this diagram is the CSS:

```css
.gantt {
  display: grid;
  grid-template: 1fr / auto 1fr;
  grid-template-areas: "left right";
  width: 100%;
}

.gantt .left_pane {
  display: grid;
  grid-area: left;
  border-right: 1px solid #bbb;
  grid-template: auto 1fr / 1fr;
  grid-template-areas: "corner" "rows";
}

.gantt .left_pane .left_pane_rows {
  display: grid;
  grid-area: rows;
}

.gantt .left_pane .left_pane_header {
  display: grid;
  grid-area: corner;
}

.gantt .right_pane {
  display: grid;
  grid-template: auto 1fr / 1fr;
  grid-template-areas: "scale" "rows";
  grid-area: right;
  overflow: auto;
}

.gantt .right_pane .right_pane_rows {
  width: 10000px; /*temp*/
  display: grid;
  grid-area: rows;
}

.gantt .right_pane .right_pane_header {
  display: flex;
  grid-area: scale;
}

.gantt .row {
  height: 40px;
  align-items: center;
  display: flex;
}

.gantt .right_pane .row {
  position: relative;
}

.gantt .right_pane .row .entry {
  position: absolute;
  background: #eeeeee;
  padding: 0.1rem 0.5rem;
  border-radius: 0.4rem;
}
```
