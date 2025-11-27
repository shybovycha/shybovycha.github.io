---
layout: post
title: 'Strongly-typed front-end: experiment 3, server communication'
date: '2021-04-19T12:58:15+0700'
tags: [programming, code-examples, tutorial]
---

The idea of this experiment is to assess how server communication works in these technologies.

**package.json**

```json
{
  "name": "ajax-server",
  "version": "1.0.0",
  "description": "",
  "main": "index.js",
  "author": "Artem Shubovych",
  "license": "ISC",
  "dependencies": {
    "express": "^4.17.1"
  }
}
```

**index.js**

```js
const express = require('express');

const app = express();

const PORT = 3000;

app.get('/', (req, res) => {
  res.json({
    translations: [
      {
        language: 'pl',
        locale: 'pl',
        strings: {
          'hello': 'cześć',
          'world': 'świat'
        }
      },
      {
        language: 'en',
        locale: 'us',
        strings: {
          'hello': 'hello',
          'world': 'world'
        }
      },
      {
        language: 'en',
        locale: 'au',
        strings: {
          'hello': `g'day`,
          'world': 'world'
        }
      }
    ]
  });
});

app.listen(port, () => {
  console.log(`Example app listening at http://localhost:${PORT}`);
});
```
