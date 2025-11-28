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

* `h`, `j`, `k`, `l` for slow but precise character-wise movement
* `w`, `b` for faster forward (and, correspondingly) backward word-by-word movement
* `$` and `^` to go to the last and first word character of the line
* `gg` and `G` to go to the beginning and the end of the file
* `O` (capital `o`) to create a blank line and go to INSERT mode above and below (lower-case `o`) the cursor
* `f`, `F` and `t` and `T` for single character lookup within the current line
* `/` and `?` to search forwards and backwards
* `c` (followed by the object) to change the _object_; this expands to the following few commands:
  * `cw` to change the current word under cursor
  * `cit` to change the text within the current XML tag
  * `ca'` to change the text surrounded by single quote
  * `ci<` to change the text surrounded with `<` and `>`
  * `ci{symbol}` to change the text surrounded by _symbol_, which could be `'`, `"`, `&#96;`; you can also use `b(` for block of text, surrounded by braces, `B{` for block of text surrounded by curly braces or `p` for paragraph, all instead of _symbol_
* `v` to enter the VISUAL mode, followed by the command:
  * `v{select}c` immediately change the selection
  * `v{select}d` cut the selected text
* `y` and `p` copy and paste the selected text (lower-case `p` pastes above the current line, capital-case `P` pastes below; capital-case `Y` copies the entire line, so the duplicate line command in Vim is `Y, P`)
* `{number}{command}` repeat the _command_ _number_ times
* `.` (the period or dot symbol) repeats the last command
* `x` to remove the character under cursor
* `r{char}` to replace the character under cursor with _char_
* `A` to go to the end of the line and enter INSERT mode ("append")
* `u` and `Ctrl+r` to undo and redo actions
* `&gt;` and `&lt;` adds or removes the indentation

## Things that I am still getting used to

* `{number}{motion}` instead of `h`, `j`, `k`, `l`
* `a` instead of `i` to enter INSERT mode _after_ the cursor (as opposed to `i` which enters INSERT mode _before_ the cursor)
* `H`, `M` and `L` to go to the top, middle and the bottom of the screen (High, Mid and Low)
* `*` and `#` to search for the word under cursor forwards and backwards
* `{count}/{query}⏎` to go to the _count_-th occurrence of _query_; it is same as searching with `/` and then hitting `n` _count_ times
* `gd` navigates to a definition of an entity under the cursor
* `gf` navigates to the path under cursor
* `%` moves the cursor to the matching brace, bracket or curly brace
* `g~` toggle the case
* `=` format the selection
* `gU` makes the selection uppercase
