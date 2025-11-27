---
layout: post
title: Vim keystrokes cheatsheet
date: '2020-08-04T11:04:24+10:00'
tags: [vim, text-editors, productivity, keyboard-shortcuts, cheatsheet, development-tools, command-line, workflow, coding-tips, reference]
---

The time has come for me to list few of the commands and keystrokes that I use (and the ones that I don't but would like to start) in Vim.

I am actually running a Vim plugin for Visual Studio Code (this and the previous blogs are actually written with this plugin ON) at this very moment.

Bear 🐻 in mind: this blog is aboout _keystrokes_ only, it is not about plugins or configuration I use - I shall cover that one day.

## Things I know by heart

* <kbd>h</kbd>, <kbd>j</kbd>, <kbd>k</kbd>, <kbd>l</kbd> for slow but precise character-wise movement
* <kbd>w</kbd>, <kbd>b</kbd> for faster forward (and, correspondingly) backward word-by-word movement
* <kbd>$</kbd> and <kbd>^</kbd> to go to the last and first word character of the line
* <kbd>gg</kbd> and <kbd>G</kbd> to go to the beginning and the end of the file
* <kbd>O</kbd> (capital `o`) to create a blank line and go to INSERT mode above and below (lower-case `o`) the cursor
* <kbd>f</kbd>, <kbd>F</kbd> and <kbd>t</kbd> and <kbd>T</kbd> for single character lookup within the current line
* <kbd>/</kbd> and <kbd>?</kbd> to search forwards and backwards
* <kbd>c</kbd> (followed by the object) to change the _object_; this expands to the following few commands:
  * <kbd>cw</kbd> to change the current word under cursor
  * <kbd>cit</kbd> to change the text within the current XML tag
  * <kbd>ca'</kbd> to change the text surrounded by single quote
  * <kbd>ci&lt;</kbd> to change the text surrounded with `&lt;` and `&gt;`
  * <kbd>ci{symbol}</kbd> to change the text surrounded by _symbol_, which could be `'`, `"`, `&#96;`; you can also use <kbd>b(</kbd> for block of text, surrounded by braces, <kbd>B{</kbd> for block of text surrounded by curly braces or <kbd>p</kbd> for paragraph, all instead of _symbol_
* <kbd>v</kbd> to enter the VISUAL mode, followed by the command:
  * <kbd>v{select}c</kbd> immediately change the selection
  * <kbd>v{select}d</kbd> cut the selected text
* <kbd>y</kbd> and <kbd>p</kbd> copy and paste the selected text (lower-case `p` pastes above the current line, capital-case `P` pastes below; capital-case `Y` copies the entire line, so the duplicate line command in Vim is <kbd>Y, P</kbd>)
* <kbd>{number}{command}</kbd> repeat the _command_ _number_ times
* <kbd>.</kbd> (the period or dot symbol) repeats the last command
* <kbd>x</kbd> to remove the character under cursor
* <kbd>r{char}</kbd> to replace the character under cursor with _char_
* <kbd>A</kbd> to go to the end of the line and enter INSERT mode ("append")
* <kbd>u</kbd> and <kbd>Ctrl+r</kbd> to undo and redo actions
* <kbd>&gt;</kbd> and <kbd>&lt;</kbd> adds or removes the indentation

## Things that I am still getting used to

* <kbd>{number}{motion}</kbd> instead of <kbd>h</kbd>, <kbd>j</kbd>, <kbd>k</kbd>, <kbd>l</kbd>
* <kbd>a</kbd> instead of <kbd>i</kbd> to enter INSERT mode _after_ the cursor (as opposed to <kbd>i</kbd> which enters INSERT mode _before_ the cursor)
* <kbd>H</kbd>, <kbd>M</kbd> and <kbd>L</kbd> to go to the top, middle and the bottom of the screen (High, Mid and Low)
* <kbd>*</kbd> and <kbd>#</kbd> to search for the word under cursor forwards and backwards
* <kbd>{count}/{query}⏎</kbd> to go to the _count_-th occurrence of _query_; it is same as searching with <kbd>/</kbd> and then hitting <kbd>n</kbd> _count_ times
* <kbd>gd</kbd> navigates to a definition of an entity under the cursor
* <kbd>gf</kbd> navigates to the path under cursor
* <kbd>%</kbd> moves the cursor to the matching brace, bracket or curly brace
* <kbd>g~</kbd> toggle the case
* <kbd>=</kbd> format the selection
* <kbd>gU</kbd> makes the selection uppercase
