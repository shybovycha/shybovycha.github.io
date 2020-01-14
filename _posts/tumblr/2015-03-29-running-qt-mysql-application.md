---
layout: post
title: Running Qt + MySQL application
date: '2015-03-29T10:47:22+02:00'
tags:
- Qt
- rtfm
- linux
tumblr_url: http://shybovycha.tumblr.com/post/114918907501/running-qt-mysql-application
---

Yesterday I tried to run my university project made with Qt and MySQL. But all I got was strange error message, saying `QMYSQL driver is not loaded` whilst loaded driver list actually included that one.

Searching all over the internet up 'til 2 AM and recompiling the whole Qt gave no result except time being wasted. Yeah, and 90% of search results were tutorials on how to recompile Qt MySQL plugin under Windows.

Yet in the morning I found solution and it was beautifully simple! I just performed one step from <a href="http://doc.qt.io/qt-5/deployment.html">Deploying Qt applications</a>, actually just copied the `plugins/` directory to the dir where the application executable lives (`build-debug/` or `build-release/` for my project; I hate the default `build-#{ProjectName}-Desktop_5_4_1-Debug/` paths); included the `sqldrivers/` directory there (just copied) and created (a bit tuned although) the `qt.conf` file. That file just pointed the plugins path to the custom one:

```
[Paths]
Prefix=.
Plugins=./plugins
```

To sum everything up:

1. create a `plugins/` directory in your build directory
2. copy `Qt/5.4/Src/qtbase/plugins/platforms/` there
3. copy `Qt/5.4/gcc_64/plugins/sqldrivers/` there too
4. create `qt.conf` file in build directory and fill it as mentioned above

And one more hint before the end: to debug why your Qt plugins fail use this application startup switch: <code>QT_DEBUG_PLUGINS=1 ./app_name</code> - this will display plugins debug information.
