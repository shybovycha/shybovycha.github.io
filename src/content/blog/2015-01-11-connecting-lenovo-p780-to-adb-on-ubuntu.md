---
layout: post
title: Connecting Lenovo P780 to ADB on Ubuntu
date: '2015-01-11T17:38:00+01:00'
tags:
- rtfm
- tutorial
- android
- ubuntu
tumblr_url: http://shybovycha.tumblr.com/post/107798726991/connecting-lenovo-p780-to-adb-on-ubuntu
---
<p>Ohhh&hellip; Today I&rsquo;ve faced one great trouble: recently I reinstalled my Ubuntu, so I lost all my configurations. And when I tried to connect my <em>Lenovo P780</em> to debug my Android application, I saw horrible error:</p>

<pre><code>$ adb devices
* daemon not running. starting it now on port 5037 *
* daemon started successfully *
List of devices attached
????????????    no permissions
</code></pre>

<p>Hey! Where did my smartphone gone?!</p>

<p>Fooling around in the internet, I found two simple steps to fix this:</p>

<ol><li><p>find the <em>VendorID</em> and <em>ProductID</em> for your device running <code>lsusb</code> two times <em>(just find the difference line)</em>:
a. when your device is disconnected
b. when your device is connected</p>

<p>This will give you two outputs:</p>

<pre><code>$ # disconnected device

$ lsusb
Bus 002 Device 002: ID 8087:0024 Intel Corp. Integrated Rate Matching Hub
Bus 002 Device 001: ID 1d6b:0002 Linux Foundation 2.0 root hub
Bus 001 Device 004: ID 064e:d213 Suyin Corp.
Bus 001 Device 002: ID 8087:0024 Intel Corp. Integrated Rate Matching Hub
Bus 001 Device 001: ID 1d6b:0002 Linux Foundation 2.0 root hub
Bus 004 Device 001: ID 1d6b:0003 Linux Foundation 3.0 root hub
Bus 003 Device 001: ID 1d6b:0002 Linux Foundation 2.0 root hub

$ # connected device (via USB)

$ lsusb
Bus 002 Device 002: ID 8087:0024 Intel Corp. Integrated Rate Matching Hub
Bus 002 Device 001: ID 1d6b:0002 Linux Foundation 2.0 root hub
Bus 001 Device 004: ID 064e:d213 Suyin Corp.
Bus 001 Device 002: ID 8087:0024 Intel Corp. Integrated Rate Matching Hub
Bus 001 Device 001: ID 1d6b:0002 Linux Foundation 2.0 root hub
Bus 004 Device 001: ID 1d6b:0003 Linux Foundation 3.0 root hub
Bus 003 Device 006: ID 0bb4:0c03 HTC (High Tech Computer Corp.)
Bus 003 Device 001: ID 1d6b:0002 Linux Foundation 2.0 root hub
</code></pre>

<p>Note the row, which is present in the second output block and is absent in the first one:</p>

<pre><code>Bus 003 Device 006: ID 0bb4:0c03 HTC (High Tech Computer Corp.)
</code></pre>

<p>For some reason, my phone is recognized as a HTC, but that does not bother me so much.
We will need only two parts of that row:</p>

<pre><code>0bb4:0c03
</code></pre>

<p>The <code>0bb4</code> is a <strong>VendorID</strong> and the <code>0c03</code> is the <strong>ProductID</strong> for my phone.</p></li>
<li><p>Add the phone attributes to the system. Sudo-edit the file <code>/lib/udev/rules.d/69-libmtp.rules</code> and point it to your device. Add a line like this (without any newlines):</p>

<pre><code>ATTR{idVendor}=="0bb4", ATTR{idProduct}=="0c03", SYMLINK+="libmtp-%k", MODE="0666", GROUP="audio", ENV{ID_MTP_DEVICE}="1", ENV{ID_MEDIA_PLAYER}="1"
</code></pre>

<p>That should enable your system to see the device later.</p></li>
<li><p>Enable write permissions for your device. Sudo-edit the file <code>/etc/udev/rules.d/51-android.rules</code> <em>(you may need to create it)</em> and add one line there:</p>

<pre><code>SUBSYSTEM=="usb", ATTRS{idVendor}=="0bb4", ATTRS{idProduct} =="0c03", MODE="0666", GROUP="plugdev"
</code></pre></li>
<li><p>To check if your phone is recognized by <code>adb</code>, restart ABD server and check its device list:</p>

<pre><code>$ adb kill-server
$ adb devices
* daemon not running. starting it now on port 5037 *
* daemon started successfully *
List of devices attached
0123456789ABCDEF    device
</code></pre></li>
</ol>
