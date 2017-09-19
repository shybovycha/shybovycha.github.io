---
layout: post
title: "IntelliJ productivity tips"
date: '2017-09-18T18:00:00+11:00'
---

This post is based on a [speech](https://www.youtube.com/watch?v=eq3KiAH4IBI) and contains some tips'n'tricks I found handy in my everyday work.

* **say "no!" to tabs** - that's it - disable them. Forever. IntellJ has many different way to navigate in the project, just check them out: <kbd>Cmd+E</kbd>, <kbd>Cmd+Alt+&larr;</kbd> and <kbd>Cmd+Alt+&rarr;</kbd>

* **stop using mouse** - just as with navigation in open files, IntelliJ offers awesome code navigation: want to go to the method definition? <kbd>Cmd+B</kbd>, want to find a file or class or method? <kbd>Shift, Shift</kbd> or <kbd>Ctrl+Shift+N</kbd> or <kbd>Ctrl+N</kbd>, want to move around in Project Manager? <kbd>Cmd+1</kbd> (and then <kbd>Esc</kbd> to switch between editor and Manager)

<!--more-->

* **use IntelliJ's code intelligence** - if you forgot what params method takes, use <kbd>Cmp+P</kbd>, if you want to find varoable or class or method usage - use <kbd>Alt+F7</kbd>, use the power of *"Refactor -> Extract Constant/Field"* function

* **use action finder popup** - that one can save you lot of time when you do not remember where the function needed is located or what key sjortcut to use - <kbd>Cmd+Shift+A</kbd> is your friend

## Summary

<table class="table table-bordered">
    <thead>
        <tr>
            <th>Shortcut</th>
            <th>Action</th>
            <th>Comments</th>
        </tr>
    </thead>

    <tbody>
        <tr>
            <td>Open/hide tool window (Project, Structure, Version Control, etc.)</td>
            <td><kbd>Cmd+1</kbd> .. <kbd>Cmd+9</kbd></td>
            <td>To go back to editor - just hit <kbd>Esc</kbd></td>
        </tr>

        <tr>
            <td>Switch between open files</td>
            <td><kbd>Cmd+E</kbd></td>
            <td></td>
        </tr>

        <tr>
            <td>Go back/forward</td>
            <td><kbd>Cmd+Alt+&larr;</kbd> and <kbd>Cmd+Alt+&rarr;</kbd></td>
            <td></td>
        </tr>

        <tr>
            <td>Search for class, method, variable</td>
            <td><kbd>Shift, Shift</kbd></td>
            <td>Use <kbd>Cmd+N</kbd> for class search</td>
        </tr>

        <tr>
            <td>Search for file</td>
            <td><kbd>Cmd+Shift+N</kbd></td>
            <td></td>
        </tr>

        <tr>
            <td>Switch between open files</td>
            <td><kbd>Cmd+E</kbd></td>
            <td></td>
        </tr>

        <tr>
            <td>Go to variable/method definition</td>
            <td><kbd>Cmd+B</kbd></td>
            <td>To go to implementations/overrides, use <kbd>Cmd+Alt+B</kbd></td>
        </tr>

        <tr>
            <td>Search for actions available</td>
            <td><kbd>Cmd+Shift+A</kbd></td>
            <td></td>
        </tr>

        <tr>
            <td>Disable tabs</td>
            <td><kbd>Cmd+Shift+A</kbd> and search for <em>tabs placement</em></td>
            <td><img data-src="{{ '/images/intellij-performance-tips/intellij-disable-tabs-optimized.png' | prepend: site.baseurl }}" alt="" /></td>
        </tr>

        <tr>
            <td>Show method parameters hint</td>
            <td><kbd>Cmd+Shift+P</kbd></td>
            <td></td>
        </tr>

        <tr>
            <td>Find method/class/variable usages</td>
            <td><kbd>Alt+F7</kbd></td>
            <td></td>
        </tr>
    </tbody>
</table>