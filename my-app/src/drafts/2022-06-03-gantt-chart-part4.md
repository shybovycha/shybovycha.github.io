---
layout: post
title: 'Gantt chart. Part 4'
---

Gantt chart could be used in two different scenarios:

1. the general dependency and timeline diagram (like the order of certain processes)
2. data management & analysis - program / project management, where the additional details really make the difference

Given the above, the implementation might vary wildly.

For simple data visualization purposes (like process ordering and timelines) Canvas would be the preferred way,
since the performance of that technology is incredible whilst there is no need to re-render the visualization
more than once (or rather very few times) and maintain the content sizes in relation to the other content
(extra details, tables, etc.).

For the data analysis and data management the main functionality is altering the data and showing it in
relation to other data (extra details, tables, etc.). Despite Canvas allows for those beautiful arrows and curve lines,
it is not as valuable for this use case as the extra data that needs to be rendered next to the visualization.
Hence it might be preferable to use tables and absolutely positioned HTML elements.
