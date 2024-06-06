---
layout: post
title: "Gantt chart. Part 4"
date: '2024-06-25T14:01:24+10:00'
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

<img src="/images/gantt_chart_part4/rework2.png" loading="lazy" alt="Split into two panels, right is scrollable">

Good, we now have two panels with items aligned in rows and the right panel being scrollable if it gets really long.
Next thing, `position: absolute` is absolutely disgusting - we use grid layout already!
Instead, split each row into the same number of columns using grid and position the elements in there:

```jsx
const RightPaneRow = ({ id, name, columns, start, end }) => {
  const gridTemplate = `auto / repeat(${columns}, 1fr)`;
  const gridArea = `1 / ${start} / 1 / ${end}`;

  return (
    <div
      className={style.row}
      style={{
        gridTemplate,
      }}
    >
      <div
        className={style.entry}
        style={{
          gridArea,
        }}
      >
        {id}
      </div>
    </div>
  );
};
```

and clean up the CSS a bit (like removing the `position: absolute` and reducing the width from `10000px` down to `1000px`):

```css
.gantt .right_pane .right_pane_rows {
  width: 1000px; /*temp*/
  display: grid;
  grid-area: rows;
}

.gantt .row {
  height: 40px;
  align-items: center;
  display: grid;
}

.gantt .right_pane .row {
  position: relative;
}

.gantt .right_pane .row .entry {
  background: #eeeeee;
  padding: 0.1rem 0.5rem;
  border-radius: 0.4rem;
}
```

Now, let's position the elements in each row using the column index:

```jsx
const RightPanelRowEntry = ({ id, start, end, children }) => {
  const gridArea = `1 / ${start} / 1 / ${end}`;

  return (
    <div
      className={style.entry}
      style={{
        gridArea,
      }}
    >
      {children}
    </div>
  );
};

const RightPaneRow = ({ id, name, columns, start, end }) => {
  const gridTemplate = `auto / repeat(${columns}, 1fr)`;
  const gridArea = `1 / ${start} / 1 / ${end}`;

  return (
    <div
      className={style.row}
      style={{
        gridTemplate,
      }}
    >
      <div
        className={style.entry}
        style={{
          gridArea,
        }}
      >
        {id}
      </div>
    </div>
  );
};

const RightPaneHeaderRow = ({ columns, children }) => {
  const gridTemplate = `auto / repeat(${columns}, 1fr)`;

  return (
    <div
      className={style.right_pane_header_row}
      style={{
        gridTemplate,
      }}
    >
      {children}
    </div>
  );
};

const RightPaneHeader = ({ children }) => {
  return <div className={style.right_pane_header}>{children}</div>;
};

const RightPane = ({ items, columns }) => {
  const columnHeaders = [...Array(columns)].map((_, idx) => (
    <RightPaneHeader>{idx + 1}</RightPaneHeader>
  ));

  const rows = items.map((item) => (
    <RightPaneRow key={item.id} columns={columns}>
      <RightPanelRowEntry {...item}>{item.id}</RightPanelRowEntry>
    </RightPaneRow>
  ));

  return (
    <div className={style.right_pane}>
      <RightPaneHeaderRow columns={columns}>{columnHeaders}</RightPaneHeaderRow>
      <div className={style.right_pane_rows}>{rows}</div>
    </div>
  );
};
```

And add corresponding new CSS styles:

```css
.gantt .right_pane .right_pane_header_row {
  display: grid;
  grid-area: scale;
}

.gantt .right_pane .right_pane_header_row .right_pane_header {
  display: grid;
  align-items: center;
  text-align: center;
}
```

This requires `start` and `end` defined for each entry:

```js
export const data = [
  {
    id: 1,
    name: "epic 1",
    start: 1,
    end: 12,
  },
  {
    id: 2,
    name: "epic 2",
    start: 2,
    end: 4,
  },
  {
    id: 3,
    name: "epic 3",
    start: 9,
    end: 11,
  },
  {
    id: 4,
    name: "story 1",
    parent: 1,
    start: 6,
    end: 7,
  },
  // ...
};
```

<img src="/images/gantt_chart_part4/rework3.png" loading="lazy" alt="Aligning items in a grid">

And, to make it not repeat a dozen of inline CSS styles, we can utilize CSS variables:

```jsx
const RightPaneRow = ({ id, columns, children }) => {
  return (
    <div className={style.row}>
      {children}
    </div>
  );
};

const RightPanelRowEntry = ({ id, start, end, children }) => {
  return (
    <div
      className={style.entry}
      style={{
        "--col-start": start,
        "--col-end": end,
      }}
    >
      {children}
    </div>
  );
};

const RightPane = ({ items, columns }) => {
  const columnHeaders = [...Array(columns)].map((_, idx) => (
    <RightPaneHeader>{idx + 1}</RightPaneHeader>
  ));

  const rows = items.map((item) => (
    <RightPaneRow key={item.id} columns={columns}>
      <RightPanelRowEntry {...item}>{item.id}</RightPanelRowEntry>
    </RightPaneRow>
  ));

  return (
    <div className={style.right_pane} style={{ "--columns": columns }}>
      <RightPaneHeaderRow>{columnHeaders}</RightPaneHeaderRow>
      <div className={style.right_pane_rows}>{rows}</div>
    </div>
  );
};
```

We can also re-use the same `row` for header:

```jsx
const RightPaneHeaderRow = ({ children }) => {
  return <div className={style.right_pane_header_row}>{children}</div>;
};
```

And corresponding CSS:

```css
.gantt .right_pane .right_pane_header_row {
  display: grid;
  grid-area: scale;

  grid-template: auto / repeat(var(--columns, 1), 1fr);
}

.gantt .right_pane .row {
  position: relative;

  grid-template: auto / repeat(var(--columns, 1), 1fr);
}

.gantt .right_pane .row .entry {
  background: #eeeeee;
  padding: 0.1rem 0.5rem;
  border-radius: 0.5rem;
  align-items: center;
  text-align: center;

  grid-area: 1 / var(--col-start, 1) / 1 / var(--col-end, 1);
}
```

I like to also change the fonts, since the default `sans-serif` just looks terrible:

```css
@import url("https://fonts.googleapis.com/css2?family=Assistant:wght@200..800&display=swap");

:root {
  font-family: "Assistant", sans-serif;
  font-optical-sizing: auto;
  font-weight: 300;
  font-style: normal;
  font-variation-settings: "wdth" 100;
}
```

<img src="/images/gantt_chart_part4/rework4.png" loading="lazy" alt="Changing the font">

And maybe add some grid lines for the rows:

```css
.gantt .row:first-child {
  border-top: 1px solid var(--border-color, #eee);
}

.gantt .row {
  padding: 0 0.75rem;
  border-bottom: 1px solid var(--border-color, #eee);
}
```

<img src="/images/gantt_chart_part4/rework5.png" loading="lazy" alt="Adding grid lines">

The last bit for a this prototype would be to have a scale for the columns.