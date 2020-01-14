---
layout: post
title: Installing deb package with dependencies
date: '2015-02-19T10:54:53+01:00'
tags:
- ubuntu
- debian
- rtfm
tumblr_url: http://shybovycha.tumblr.com/post/111457648576/installing-deb-package-with-dependencies
---

Often, installing packages on Debian-based systems like so

```bash
sudo dpkg -i package_file.deb
```

fails with messages like `dependency not satisfied`.

To fix this, there are two ways:

1. `sudo dpkg -i package_file.deb && sudo apt-get -f install`
2. `sudo dpkg -i --force-depends package_file.deb`

Obviously, the second one is shorter =)
