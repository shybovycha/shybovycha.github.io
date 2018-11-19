---
layout: post
title: Gantt chart with D3.js
date: '2017-04-09T11:04:24+02:00'
---

At work, I've had a task to implement a Gantt chart diagram to show dependencies and order of some... let's say, milestones.
Given this feature is in a very unstable beta in Google Charts, I thought to myself: *"Why don't I implement it on my own?"*.
And tried to recall my D3 knowledge.

I've also found a minimalistic, but helpful example / screenshot of some Gantt chart implementation:

<img data-src="{{ '/images/gantt_chart_with_d3/gantt-sample_optimized.png' | prepend: site.baseurl }}" alt="">

The challenges I've faced were:

1. order milestones on a timeline
2. scale milestones to fit in a viewport
3. create pretty connection lines
4. center text inside each milestone

And since D3 is a data-driven library, I've used map/reduce where possible.

Here's how the result looked like:

<img data-src="{{ '/images/gantt_chart_with_d3/d3-gantt-chart_optimized.png' | prepend: site.baseurl }}" alt="">

The full implementation code is under the cut.

<!--more-->

```js
var createGanttChart = function (placeholder, data, {
  itemHeight,
  svgOptions
}) {
  // prepare data
  let minStartDate, maxEndDate;

  let margin = (svgOptions && svgOptions.margin) || {
    top: itemHeight * 2,
    left: itemHeight * 2
  };

  let scaleWidth = ((svgOptions && svgOptions.width) || 600);
  let scaleHeight = Math.max((svgOptions && svgOptions.height) || 200, data.length * itemHeight * 2);

  scaleWidth -= margin.left * 2;
  scaleHeight -= margin.top * 2;

  let svgWidth = scaleWidth + (margin.left * 2);
  let svgHeight = scaleHeight + (margin.top * 2);

  let fontSize = (svgOptions && svgOptions.fontSize) || 12;

  data = data.map(function(e) {
    if ((!e.startDate || !e.endDate) && !e.duration) {
      throw new Exception('Wrong element format: should contain either startDate and duration, or endDate and duration or startDate and endDate');
    }

    if (e.startDate)
      e.startDate = moment(e.startDate);

    if (e.endDate)
      e.endDate = moment(e.endDate);

    if (e.startDate && !e.endDate && e.duration) {
      e.endDate = moment(e.startDate);
      e.endDate.add(e.duration[0], e.duration[1]);
    }

    if (!e.startDate && e.endDate && e.duration) {
      e.startDate = moment(e.endDate);
      e.startDate.subtract(e.duration[0], e.duration[1]);
    }

    if (!minStartDate || e.startDate.isBefore(minStartDate)) minStartDate = moment(e.startDate);

    if (!minStartDate || e.endDate.isBefore(minStartDate)) minStartDate = moment(e.endDate);

    if (!maxEndDate || e.endDate.isAfter(maxEndDate)) maxEndDate = moment(e.endDate);

    if (!maxEndDate || e.startDate.isAfter(maxEndDate)) maxEndDate = moment(e.startDate);

    if (!e.dependsOn)
      e.dependsOn = [];

    return e;
  });

  // add some padding to axes
  minStartDate.subtract(2, 'days');
  maxEndDate.add(2, 'days');

  let dataCache = data.reduce(function (acc, e) {
      acc[e.id] = e;
      return acc;
  }, {});

  let fillParents = function (eltId, result) {
      dataCache[eltId].dependsOn.forEach(function (parentId) {
          if (!result[parentId])
            result[parentId] = [];

          if (result[parentId].indexOf(eltId) < 0)
            result[parentId].push(eltId);

          fillParents(parentId, result);
      });
  };

  let childrenCache = data.reduce(function (acc, e) {
      if (!acc[e.id])
        acc[e.id] = [];

      fillParents(e.id, acc);
      return acc;
  }, {});

  data = data.sort(function(e1, e2) {
    if (childrenCache[e1.id] && childrenCache[e2.id] && childrenCache[e1.id].length > childrenCache[e2.id].length)
    // if (moment(e1.endDate).isBefore(moment(e2.endDate)))
      return -1;
    else
      return 1;
  });

  // create container element
  let svg = d3.select(placeholder).append('svg').attr('width', svgWidth).attr('height', svgHeight);

  const xScale = d3.scaleTime()
    .domain([minStartDate.toDate(), maxEndDate.toDate()])
    .range([0, scaleWidth]);

  const xAxis = d3.axisBottom(xScale);

  const g1 = svg.append('g').attr('transform', `translate(${margin.left},${margin.top})`);

  const linesContainer = g1.append('g').attr('transform', `translate(0,${margin.top})`);
  const barsContainer = g1.append('g').attr('transform', `translate(0,${margin.top})`);

  g1.append('g').call(xAxis);

  let rectangleData = data.map(function (d, i) {
    let x = xScale(d.startDate.toDate());
    let xEnd = xScale(d.endDate.toDate());
    let y = i * itemHeight * 1.5;
    let width = xEnd - x;
    let height = itemHeight;

    let label = d.label;
    let charWidth = (width / fontSize);
    let dependsOn = d.dependsOn;
    let id = d.id;

    let tooltip = d.label;

    let singleCharWidth = fontSize * 0.5;
    let singleCharHeight = fontSize * 0.45;

    if (label.length > charWidth) {
      label = label.split('').slice(0, charWidth - 3).join('') + '...';
    }

    let labelX = x + ((width / 2) - ((label.length / 2) * singleCharWidth));
    let labelY = y + ((height / 2) + (singleCharHeight));

    return {
      x,
      y,
      xEnd,
      width,
      height,
      id,
      dependsOn,
      label,
      labelX,
      labelY,
      tooltip
    };
  });

  // create axes
  let bars = barsContainer
    .selectAll('g')
    .data(rectangleData)
    .enter()
    .append('g');

  // prepare dependencies polyline data
  let cachedData = rectangleData.reduce((acc, e) => {
    acc[e.id] = e;
    return acc;
  }, {});

  let cachedIds = rectangleData.map(e => e.id);

  let storedConnections = rectangleData.reduce((acc, e) => { acc[e.id] = 0; return acc }, {});

  let polylineData = rectangleData.reduce(function(acc, d) {
    return acc.concat(
      d.dependsOn
      .map(parentId => cachedData[parentId])
      .map(function (parent) {
        let points = [],
            color = '#' + (Math.max(0.1, Math.min(0.9, Math.random())) * 0xFFF << 0).toString(16);

        storedConnections[parent.id]++;
        storedConnections[d.id]++;

        let deltaParentConnections = storedConnections[parent.id] * (itemHeight / 4);
        let deltaChildConnections = storedConnections[d.id] * (itemHeight / 4);

        if (true) { // cachedIds.indexOf(parent.id) < cachedIds.indexOf(d.id)) {
          // if parent is right above the current bar - put four points at different heights
          points = [
            d.x, (d.y + (itemHeight / 2)),
            d.x - deltaChildConnections, (d.y + (itemHeight / 2)),
            d.x - deltaChildConnections, (d.y - (itemHeight * 0.25)),
            parent.xEnd + deltaParentConnections, (d.y - (itemHeight * 0.25)),
            parent.xEnd + deltaParentConnections, (parent.y + (itemHeight / 2)),
            parent.xEnd, (parent.y + (itemHeight / 2))
          ];
        } else {
          // otherwise - use three points
          points = [
            d.x, (d.y + (itemHeight / 2)),
            d.x - deltaChildConnections, (d.y + (itemHeight / 2)),
            parent.xEnd + deltaParentConnections, (d.y + (itemHeight / 2)),
            parent.xEnd + deltaParentConnections, (parent.y + (itemHeight / 2)),
            parent.xEnd, (parent.y + (itemHeight / 2))
          ];
        }

        return {
          points: points.join(','),
          color: color
        };
      })
    );
  }, []);

  let lines = linesContainer
    .selectAll('polyline')
    .data(polylineData)
    .enter()
    .append('polyline')
    .style('fill', 'none')
    .style('stroke', d => d.color)
    .attr('points', d => d.points);

  bars
    .append('rect')
    .attr('rx', itemHeight / 2)
    .attr('ry', itemHeight / 2)
    .attr('x', d => d.x)
    .attr('y', d => d.y)
    .attr('width', d => d.width)
    .attr('height', d => d.height)
    .style('fill', '#ddd')
    .style('stroke', 'black');

  bars
    .append('text')
    .style('stroke', 'black')
    .attr('x', d => d.labelX)
    .attr('y', d => d.labelY)
    .text(d => d.label);

  bars
    .append('title')
    .text(d => d.tooltip);
};
```

And here's the usage example:

```js
var data = [{
  startDate: '2017-02-27',
  endDate: '2017-03-04',
  label: 'milestone 01',
  id: 'm01',
  dependsOn: []
}, {
  startDate: '2017-02-23',
  endDate: '2017-03-01',
  label: 'milestone 01',
  id: 'm06',
  dependsOn: ['m01']
}, {
  duration: [7, 'days'],
  endDate: '2017-03-24',
  label: 'milestone 02',
  id: 'm02',
  dependsOn: ['m04']
}, {
  startDate: '2017-02-27',
  duration: [12, 'days'],
  label: 'milestone 03',
  id: 'm03',
  dependsOn: ['m01']
}, {
  endDate: '2017-03-17',
  duration: [5, 'days'],
  label: 'milestone 04',
  id: 'm04',
  dependsOn: ['m01']
}];

createGanttChart(document.querySelector('body'), data, {
  itemHeight: 20,
  svgOptions: {
    width: 1200,
    height: 400,
    fontSize: 12
  }
});
```
