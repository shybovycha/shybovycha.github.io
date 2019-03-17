---
layout: post
title: "IntelliJ productivity tips"
date: '2017-09-18T18:00:00+11:00'
---

This post is based on a [talk on IntelliJ productivity](https://www.youtube.com/watch?v=eq3KiAH4IBI) and contains some tips and tricks I found handy in my everyday work.

You may disagree with me or find these recommendations useless for yourself, but I and some of my teammates have found out these tips have made us more efficient when dealing with lots of code in a big project.

* **say "no!" to tabs** -- that's it -- disable them. Forever. IntellJ has many ways to navigate the project, just check them out: <kbd>Cmd+E</kbd>, <kbd>Cmd+Alt+&larr;</kbd> and <kbd>Cmd+Alt+&rarr;</kbd>

* **stop using mouse** -- just as with navigation in open files, IntelliJ offers awesome code navigation: want to go to the method definition? <kbd>Cmd+B</kbd>, want to find a file or class or method? <kbd>Shift, Shift</kbd> or <kbd>Ctrl+Shift+N</kbd> or <kbd>Ctrl+N</kbd>, want to move around in Project Manager? <kbd>Cmd+1</kbd> (and then <kbd>Esc</kbd> to switch between editor and Manager)

* **use IntelliJ's code intelligence** -- if you forgot what params method takes, use <kbd>Cmp+P</kbd>, if you want to find variable or class or method usage -- use <kbd>Alt+F7</kbd>, use the power of *"Refactor -> Extract Constant/Field"* function

* **use action finder popup** -- that one can save you lot of time when you do not remember where the function needed is located or what key shortcut to use -- <kbd>Cmd+Shift+A</kbd> is your friend

**WARNING: lots of video/gifs under the cut!**

<!--more-->

## Summary

<section>
    <div>Open/hide tool window (Project, Structure, Version Control, etc.)</div>

    <div><kbd>Cmd+1</kbd> .. <kbd>Cmd+9</kbd></div>

    <div>
        To go back to editor - just hit <kbd>Esc</kbd> <br />
        Project Tree: <br />
        <video preload autoplay loop muted>
            <source src="/images/intellij-performance-tips/cmd_1_esc.webm" />
            <source src="/images/intellij-performance-tips/cmd_1_esc.gif" />
        </video>
        <br />
        Search results:
        <br />
        <video preload autoplay loop muted>
            <source src="/images/intellij-performance-tips/cmd_3_esc.webm" />
            <source src="/images/intellij-performance-tips/cmd_3_esc.gif" />
        </video>
        <br />
        Version Control:
        <br />
        <video preload autoplay loop muted>
            <source src="/images/intellij-performance-tips/cmd_9_esc.webm" />
            <source src="/images/intellij-performance-tips/cmd_9_esc.gif" />
        </video>
    </div>
</section>

<section>
    <div>Switch between open files</div>
    <div><kbd>Cmd+E</kbd></div>
    <div>
        <video preload autoplay loop muted>
            <source src="/images/intellij-performance-tips/cmd_e.webm" />
            <source src="/images/intellij-performance-tips/cmd_e.gif" />
        </video>
    </div>
</section>

<section>
    <div>Go back/forward</div>
    <div><kbd>Cmd+Alt+&larr;</kbd> and <kbd>Cmd+Alt+&rarr;</kbd></div>
    <div>
        <video preload autoplay loop muted>
            <source src="/images/intellij-performance-tips/cmd_alt_arrows.webm" />
            <source src="/images/intellij-performance-tips/cmd_alt_arrows.gif" />
        </video>
    </div>
</section>

<section>
    <div>Search for class, method, variable</div>
    <div><kbd>Shift, Shift</kbd></div>
    <div>
        <video preload autoplay loop muted>
            <source src="/images/intellij-performance-tips/shift_shift.webm" />
            <source src="/images/intellij-performance-tips/shift_shift.gif" />
        </video>
        <br />
        Use <kbd>Cmd+N</kbd> for class search:
        <br />
        <video preload autoplay loop muted>
            <source src="/images/intellij-performance-tips/cmd_n.webm" />
            <source src="/images/intellij-performance-tips/cmd_n.gif" />
        </video>
    </div>
</section>

<section>
    <div>Search for file</div>
    <div><kbd>Cmd+Shift+N</kbd></div>
    <div>
        <video preload autoplay loop muted>
            <source src="/images/intellij-performance-tips/cmd_shift_n.webm" />
            <source src="/images/intellij-performance-tips/cmd_shift_n.gif" />
        </video>
    </div>
</section>

<section>
    <div>Go to variable/method definition</div>
    <div><kbd>Cmd+B</kbd></div>
    <div>
        <video preload autoplay loop muted>
            <source src="/images/intellij-performance-tips/cmd_b.webm" />
            <source src="/images/intellij-performance-tips/cmd_b.gif" />
        </video>
        <br />
        To go to implementations/overrides, use <kbd>Cmd+Alt+B</kbd>:
        <br />
        <video preload autoplay loop muted>
            <source src="/images/intellij-performance-tips/cmd_alt_b.webm" />
            <source src="/images/intellij-performance-tips/cmd_alt_b.gif" />
        </video>
    </div>
</section>

<section>
    <div>Search for actions available</div>
    <div><kbd>Cmd+Shift+A</kbd></div>
    <div>
        <video preload autoplay loop muted>
            <source src="/images/intellij-performance-tips/cmd_shift_a.webm" />
            <source src="/images/intellij-performance-tips/cmd_shift_a.gif" />
        </video>
    </div>
</section>

<section>
    <div>Disable tabs</div>
    <div><kbd>Cmd+Shift+A</kbd> and search for <em>tabs placement</em></div>
    <div>
        <img data-src="/images/intellij-performance-tips/intellij-disable-tabs-optimized.png" alt="" />
    </div>
</section>

<section>
    <div>Show method parameters hint</div>
    <div><kbd>Cmd+P</kbd></div>
    <div>
        <video preload autoplay loop muted>
            <source src="/images/intellij-performance-tips/cmd_p.webm" />
            <source src="/images/intellij-performance-tips/cmd_p.gif" />
        </video>
    </div>
</section>

<section>
    <div>Find method/class/variable usages</div>
    <div><kbd>Alt+F7</kbd></div>
    <div>
        <video preload autoplay loop muted>
            <source src="/images/intellij-performance-tips/alt_f7.webm" />
            <source src="/images/intellij-performance-tips/alt_f7.gif" />
        </video>
    </div>
</section>