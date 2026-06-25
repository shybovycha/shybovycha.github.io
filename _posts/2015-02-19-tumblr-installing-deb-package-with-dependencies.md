---
layout: post
title: Installing deb package with dependencies
date: '2015-02-19T10:54:53+01:00'
tags:
- ubuntu
tumblr_url: http://shybovycha.tumblr.com/post/111457648576/installing-deb-package-with-dependencies
---

Often, the command

```bash
$ sudo dpkg -i package_file.deb
```

fails with messages like `dependency not satisfied`.

There are two ways to fix this:

1. `sudo dpkg -i package_file.deb && sudo apt-get -f install`
2. `sudo dpkg -i --force-depends package_file.deb`

Obviously, the second one is better because it is shorter 😁
