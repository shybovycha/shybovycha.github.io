---
layout: post
title: "Gantt chart. Part 3"
date: '2021-03-04T19:46:24+11:00'
---

I have been writing about and improving on <a href="https://github.com/shybovycha/gantt-chart/">my Gantt chart implementation</a> for quite some time now.

In the <a href="{% post_url 2020-08-02-gantt-chart-part2 %}">previous blog</a> I have promised to re-write the implementation in Canvas. And so I did.

And there are few functional improvements:

* scrolling and zooming in & out is now a thing
* you can drag the milestones around and stretch & shrink them
* an event will be fired whenever a milestone is changed (moved / stretched / shrinked)
* dependencies are now typed: start-to-start, end-to-end or end-to-start are the only supported types

This is a short show-off blog, it won't contain any technical details.
I shall blog about the implementation in the future.

Here, you can even play around with it now!

<img data-src="/images/gantt_chart_part3/screenshot.webp" id="gantt-chart-screenshot">

<div class="chart-container" style="display:none;">
  <div class="controls">
    <button id="zoom-out">-</button>
    <button id="zoom-in">+</button>
  </div>

  <div id="gantt-chart"></div>
</div>

<script>
window.addEventListener('DOMContentLoaded', () => {
  const chartScreenshot = document.querySelector('#gantt-chart-screenshot');
  const chartContainer = document.querySelector('.chart-container');

  const bundle = document.createElement('script');

  bundle.onload = () => {
    chartContainer.style.display = 'block';
    chartScreenshot.parentElement.removeChild(chartScreenshot);
  };

  bundle.src = '/js/gantt-chart.bundle.js';

  document.body.appendChild(bundle);
});
</script>
