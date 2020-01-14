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

Ohhh... Today I've faced one great trouble: recently I reinstalled my Ubuntu, so I lost all my configurations. And when I tried to connect my _Lenovo P780_ to debug my Android application, I saw horrible error:

```
$ adb devices
* daemon not running. starting it now on port 5037 *
* daemon started successfully *
List of devices attached
????????????    no permissions
```

Hey! Where did my smartphone gone?!

Fooling around in the internet, I found two simple steps to fix this:

### 1. find the `VendorID` and `ProductID` of your device

Run `lsusb` two times _(just find the difference line)_:

1. when your device is disconnected
2. when your device is connected

This will give you two outputs:

```bash
$ # disconnected device

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
```

Note the row, which is present in the second output block and is absent in the first one:

```
Bus 003 Device 006: ID 0bb4:0c03 HTC (High Tech Computer Corp.)
```

For some reason, my phone is recognized as a HTC, but that does not bother me so much.
We will need only two parts of that row:

```
0bb4:0c03
```

The `0bb4` is a **VendorID** and the `0c03` is the **ProductID** for my phone.

### 2. add the phone attributes to the system

Sudo-edit the file `/lib/udev/rules.d/69-libmtp.rules` and point it to your device. Add a line like this (without any newlines):

```
ATTR{idVendor}=="0bb4", ATTR{idProduct}=="0c03", SYMLINK+="libmtp-%k", MODE="0666", GROUP="audio", ENV{ID_MTP_DEVICE}="1", ENV{ID_MEDIA_PLAYER}="1"
```

That should enable your system to see the device later.</li>

### 3. enable write permissions for your device

Sudo-edit the file `/etc/udev/rules.d/51-android.rules _(you may need to create it)_ and add one line there:

```
SUBSYSTEM=="usb", ATTRS{idVendor}=="0bb4", ATTRS{idProduct} =="0c03", MODE="0666", GROUP="plugdev"
```

### 4. final step

Check if your phone is recognized by `adb` - restart ABD server and check its device list:

```bash
$ adb kill-server
$ adb devices
* daemon not running. starting it now on port 5037 *
* daemon started successfully *
List of devices attached
0123456789ABCDEF    device
```
