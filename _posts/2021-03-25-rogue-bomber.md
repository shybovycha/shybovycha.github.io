---
layout: post
title: "Rogue bomber"
date: '2021-03-25T20:26:24+11:00'
---

This is a yet another show-off blog.

<img data-src="/images/rogue_bomber/screenshot.webp">

Yet another one-day-build, made specifically for not-so-exciting ShipIt-51 hackathon we are having at work right now.

This is a terrible code written in JS in matter of some 8 hrs.

Click the button below to start playing.

<div id="rogue-bomber-placeholder"></div>

<button id="start-rogue-bomber" class="btn btn-md btn-primary read-more">Play!</button>

<script>
window.addEventListener('DOMContentLoaded', () => {
  const bundle = document.createElement('script');

  bundle.onload = () => {
    document.querySelector('#start-rogue-bomber').onclick = () => {
      const canvas = document.createElement('canvas');
      const parent = document.querySelector('#rogue-bomber-placeholder').appendChild(canvas);
      window.__startRogueBomber(canvas);
    };
  };

  bundle.src = '/js/rogue-bomber.js';

  document.body.appendChild(bundle);
});
</script>
