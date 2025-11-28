---
layout: post
title: "How to adjust widget size in Qt"
date: '2015-03-24T08:56:37+01:00'
tags:
- qt
tumblr_url: http://shybovycha.tumblr.com/post/114480543411/how-to-adjust-widget-size-in-qt
---

Have you ever thought on how this:

<img src="/images/tumblr/qt_adjust_widget/tumblr_inline_nlphu7Lp7Q1qh5oee_500.webp" loading="lazy" />

could be turned to this:

<img src="/images/tumblr/qt_adjust_widget/tumblr_inline_nlphuiqjyZ1qh5oee_500.webp" loading="lazy" />

without manually setting per-pixel sizes?

It's simple as 1-2! Just take a look at the top of the UI editor and you'll find a set of buttons, dedicated to do this adjustment for you:

<img src="/images/tumblr/qt_adjust_widget/tumblr_inline_nlpi06oySm1qh5oee_500.webp" loading="lazy" />

Yet, there is just one small tip: first you should select a containing widget, whose children will be rearranged.

Like this:

<img src="/images/tumblr/qt_adjust_widget/tumblr_inline_nlpi0pvXoe1qh5oee_500.webp" loading="lazy" />

Note the selected parent widget for *vertical_layout*.
