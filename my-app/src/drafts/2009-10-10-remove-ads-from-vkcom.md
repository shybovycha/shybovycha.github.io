---
title: 'Remove ads from vk.com'
layout: post
tags: [programming]
---

Starting on from today, vk.com inserts and updates ads on the page periodically, since page has loaded.
This is the way to remove it on-the-fly:

* open Developer Tools in your browser (<kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>I</kbd> or <kbd>Cmd</kbd> + <kbd>Shift</kbd> + <kbd>I</kbd>)
* go to the "Console" tab
* enter this text into the console:

```js
setInterval(function () {
    var elts = document.querySelectorAll('#ads_left');
    for (var i = 0; i < elts.length; i++) { elts[i].parentElement.removeChild(elts[i]); }
}, 5000);
```

* press <kbd>Enter</kbd>
* perform these steps each time you enter vk.com or reload the page =(
