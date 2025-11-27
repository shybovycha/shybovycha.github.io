---
layout: post
title: 'Modal text editors'
tags: [vim, neovim, helix, kakoune, text-editors, ide, productivity, developer-tools, plugins, code-editing]
---

I was using vim on-and-off since my pal showed me Linux on my first ever computer, back in circa 2005.
I then learned emacs back in 2015 and used it for remote development (over SSH).
That was the first time I used a bunch of plugins to improve my developer experience.
Then, circa 2022, I heard about an interesting take on vim ideology - [Kakoune](https://kakoune.org/).
I liked how it allowed user to _see_ what he is about to operate on before actually _performing_ an action.
I did not end up using it often, but I liked the idea.
Then at the end of 2024, I heard about [Helix](https://helix-editor.com/) - another take on Vim ideology (or more like Kakoune at that point).
At some random morning, scrolling the suggested videos on YouTube, I stumbled upon a semi-interesting (at the time) [video about Vim productivity tips](https://www.youtube.com/watch?v=LaIa1tQFOSY). Creator showed an interesting plugin, flash, which allowed him to quickly jump between any two points on the screen - much faster and easier than what I usually would do in Vim (relative line numbers and motions). I really liked the idea, to a point where I was on the edge of giving it a try.
Since we have a few people in our team using NeoVim or Vim plugin for VS Code or IntelliJ Idea, I thought about using each of them for a while for work and comparing the experience.

## IntelliJ Idea, baseline

For a number of years, the way I develop in IntelliJ is quite keyboard-centric - I disable tabs and use `Shift-Shift` menu and its tabs to navigate the project and perform actions.

<img src="/images/modal-text-editors/intellij-quick-actions-menu.png" loading="lazy" alt="Quick Actions menu in IntelliJ Idea">

The quick action menu has 6 tabs:

- Classes
- Files
- Symbols
- Actions
- Text
- All (combined of the above)

For the most part I only use Classes, Files and Actions tabs via a keyboard shortcut. From the Actions tab I rarely use more than "rename", "extract variable/method" or "copy path/reference".

The only other two features of the IDE I actually use are the file tree (so that I can see the _structure_ of the project - it comes handy for Java packages organization, for example) which I configure to automatically focus the file currently opened in the editor and the terminal, which I use for pretty much everything else (including Git interactions and running / building project).

Actually, IntelliJ Idea gives you the statistics of features you use in the **Help** -> **My Productivity** menu.

<img src="/images/modal-text-editors/intellij-features-usage.png" loading="lazy" alt="My feature usage in IntelliJ Idea">

In my case it looks like this (ordered by the usage frequency, descending - from most used to least):

- Recent files
- Go to declaration
- Code completion
- Syntax-aware selection (<kbd>Cmd</kbd> + <kbd>w</kbd>)
- Find in files
- Context actions (suggestions like "remove unused", "replace with X", "import Y", etc.)
- Find (in current file)
- Toggle comment
- Go to file
- Go to class
- Rename
- Find and replace (in current file)
- Generate code (constructors, getters and setters)
- Go to implementation
- Introduce variable
- Change case (camel case)
- Implement methods

Some of these (implement methods, introduce variable, generate code) might be hard to achieve in either of editors, so they are not requirements, but rather nice to haves.

So this would be my measure of success in other editors (in terms of comfort) - on top of movement in a document, I would like to compare the editors in terms of moving in the _project_ (files, symbols / classes and references) and quick actions.

## NeoVim

<img src="/images/modal-text-editors/nvim-0.png" loading="lazy" alt="Neovim">

I thought about trying [NeoVim](https://neovim.io/) since it was the hype at the time. I was dumbfolded by the fact it did not even come with the default config file to work with (in a stumbling contrast to Helix, which has `:config-open` and `:config-reload` commands built in).
Yet I kept going, spending days perfecting my config. I switched my terminal emulator from [Warp](https://www.warp.dev/) to [Ghostty](https://github.com/ghostty-org/ghostty) (so that I can see images right inside my terminal, improving file manager experience), installed a good dozen of plugins and messed with color schemes.

My full neovim config is actually [public](https://github.com/shybovycha/moo-nvim-config/).

After almost a week, my NeoVim plugin setup looked like this:

- [Lazy](https://github.com/folke/lazy.nvim) - plugin manager
- [Telescope](https://github.com/nvim-telescope/telescope.nvim) - for fzf and quick actions
- [Neo-tree](https://github.com/nvim-neo-tree/neo-tree.nvim) - the file tree

These three plugins alone allowed me to already be quite a bit productive compared to vanilla Vim experience. But it was not a complete setup:

- [telescope-file-browser](https://github.com/nvim-telescope/telescope-file-browser.nvim) (?) - as an alternative for Neo-tree
- [telescope-ui-select](https://github.com/nvim-telescope/telescope-ui-select.nvim) - for LSP integration (code actions, specifically)
- [mason](https://github.com/williamboman/mason.nvim) - for LSP management
- [mason-lspconfig](https://github.com/williamboman/mason-lspconfig.nvim) - a middleware between mason and lspconfig
- [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig) - for configuring LSP in NeoVim
- [nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter) - for treesitter integration
- [toggleterm](https://github.com/akinsho/toggleterm.nvim) - terminal integration, toggleable; obsolete when using zellij
- [vim-sleuth](https://github.com/tpope/vim-sleuth) - automatically detects tabwidth settings for the buffer
- [Comment.nvim](https://github.com/numToStr/Comment.nvim) - toggle comment for lines and blocks

A rather large set of plugins is required for autocomplete to work:

- [nvim-cmp](https://github.com/hrsh7th/cmp-path) - autocomplete engine
- [cmp-nvim-lsp](https://github.com/hrsh7th/cmp-nvim-lsp) - autocomplete integration for LSP
- [cmp-path](https://github.com/hrsh7th/cmp-path) - autocomplete for file paths
- [LuaSnip](https://github.com/L3MON4D3/LuaSnip) - snippets for Lua
- [cmp_luasnip](https://github.com/saadparwaiz1/cmp_luasnip) - autocomplete integration for luasnip
- [friendly-snippets](https://github.com/rafamadriz/friendly-snippets) - a set of snippets
- [lspkind.nvim](https://github.com/onsails/lspkind.nvim) - icons for autocomplete options

Those plugins make up for almost a complete setup.

But I wanted to further improve the Vim experience somewhat so motions become easier (like in the video from the top of the post):

- [nvim-treesitter-textobjects](https://github.com/nvim-treesitter/nvim-treesitter-textobjects) (?) - for moving between text blocks (paragraphs, function / class scopes, parameters, etc.)
- [nvim-surround](https://github.com/kylechui/nvim-surround) - for operations on surrounding characters (braces, brackets, quotes, ticks, etc.)
- [flash](https://github.com/folke/flash.nvim) - the fast movements
- [vim-visual-multi](https://github.com/mg979/vim-visual-multi) - true multi-cursor editing (unlike visual block mode, which would require macros for the best experience)

And there are a few quality-of-life improvements:

- [undotree](https://github.com/mbbill/undotree) - local file history
- [noice](https://github.com/folke/noice.nvim) - moves command line and search line to the middle of the screen, in a popup panel
- [indent-blankline](https://github.com/lukas-reineke/indent-blankline.nvim) - to show indentation lines
- [cmp-cmdline](https://github.com/hrsh7th/cmp-cmdline) - for autocomplete suggestions in the Noice command popup
- [nvim-treesitter-context](nvim-treesitter/nvim-treesitter-context) - shows the current context as fixed lines at the top of the screen (like in VSCode)
- [outline](https://github.com/hedyhli/outline.nvim) with [outline-treesitter-provider](https://github.com/epheien/outline-treesitter-provider.nvim) - shows symbols defined in the current buffer
- [quicker](https://github.com/stevearc/quicker.nvim) - allows for better quickref buffer experience
- [telescope-live-grep-args](https://github.com/nvim-telescope/telescope-live-grep-args.nvim) - allows to search not by text (or regex) alone, but also by filename / filetype (and more)

The config I have at the moment allows the following:

- file tree
    - `<leader>n` toggles NeoTree
    - ~~when NeoVim opens, NeoTree opens too~~ later I decided against this, since sometimes I want to just edit a single file without a care for the directory containing it
    - NeoTree follows the currently open file
    - NeoTree shows hidden files (including dot files and directories, like `.github`, which are hidden by default)
    - when NeoTree is the last open buffer, NeoVim will quit
- movements
    - `s` performs a Flash search (with literal references to different points in text)
    - `S-s` performs a treesitter Flash search
- terminal
    - trying to utilize zellij instead of built-in NeoVim terminal, following the "one tool for its purpose" principle
    - `<leader>t` toggles a terminal
    - `C-\ C-n` switches terminal to Normal mode, but I also remapped it to `<Esc><Esc>`
    - `i` switches terminal to Insert mode
- navigation
    - `C-left` / `C-right` goes through the jumplist (similarly to back/forward in IntelliJ / VSCode)
- editing
    - `ys<motion><character>` wraps selected text in a specified character
    - `cs<old char><new char>` changes the wrapped character around text under cursor from `<old char>` to `<new char>`
    - `ds<char>` deletes wrapped character around text under cursor
    - `gcc` comments / uncomments the line
    - `gc` comments the selected block of text
    - `<Esc>` in Normal mode sends `nohlsearch` to remove search highlights 
    - `<leader>u` shows a list of changes (undo tree), aka local file history
    - `C-n` / `C-<down>` / `C-<up>` creates a new cursor for the same word occurrences / below current cursor / above current cursor
- LSP
    - `S-k` shows the hint for the symbol under cursor
    - `<leader>ca` (built-in `gra` since NeoVim 0.11) shows **c**ode **a**ctions for the symbol under cursor in a Telescope panel
    - `treesitter-textobjects` provides custom scopes (like `w` for `word` or `W` for `WORD`):  
        - `ac` / `ic` - outer class code / inner class code
        - `af` / `if` - outer function / inner function
        - `as` - local scope
        - `ai` / `ii` - outer function call / inner function call (`i` for `invocation`)
        - `ap` / `ip` - outer / inner parameter
        - `ar` / `ir` - outer / inner return statement
    - `<leader>o` shows list of symbols defined in the current buffer (outline)
    - `gr` (built-in `grr` since NeoVim 0.11) shows a list of LSP references; `<C-q>` sends the list of locations to quick buffer
    - `gd` shows a list of LSP definitions
    - `gI` (built-in `gri` since NeoVim 0.11) shows a list of LSP implementations
    - `grn` (built-in since NeoVim 0.11) renames a symbol under cursor
    - `C-s` (built-in since NeoVim 0.11) shows method signature reference
    - `]d` / `[d` (built-in) goes to previous / next diagnostic message location (e.g. warnings, errors, suggestions in the code)
    - `C-w,d` shows the diagnostic message at cursor
- Telescope
    - `<leader><leader>` opens a quick action menu (Telescope builtin)
    - `<leader>f` opens search in files
    - `<leader>gf` opens search only in files tracked by Git
    - `<leader>b` opens a list of NeoVim buffers, ignoring current buffer and sorting by last used timestamp
    - `<leader>/` performs a live-grep (with arguments) on files
    - `C-q` (in the search results; needs Zellij in the locked mode - `C-g` - to prevent conflict) moves files to quickref buffer; then
        - `]c` / `[c` for next / previous quickref entry
        - `<leader>q` toggles quickref buffer
        - `<leader>l` toggles locations buffer

Aside from ridiculous amount of time spent configuring Neovim, it ticks most of my boxes:

- Recent files ✅ `<leader>b`
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/nvim2-2025-11-26_09.24.48.webm" />
    </video>
- Go to declaration ✅ `gd`
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/nvim2-2025-11-26_09.09.54.webm" />
    </video>
- Code completion ✅ (`nvim-cmp`) `C-space`
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/nvim2-2025-11-26_09.12.35.webm" />
    </video>
- Syntax-aware selection ✅ (`nvim-treesitter` with customized `incremental_selection`): initialize treesitter selection mode with `gsn` and then increment / decrement node selection with `gss` and `gsm`
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/nvim2-2025-11-26_09.13.21.webm" />
    </video>
- Find in files ✅ `<leader>/` (`telescope` + `telescope-live-grep-args`, [rg](https://github.com/BurntSushi/ripgrep)-powered)
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/nvim2-2025-11-26_09.28.39.webm" />
    </video>
- Context actions ✅ `gra`
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/nvim2-2025-11-26_09.28.19.webm" />
    </video>
- Find in current file ✅ `/`
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/nvim2-2025-11-26_09.29.25.webm" />
    </video>
- Toggle comment ✅ `gcc` / `gc<object>` (e.g. `gcaw` - comment word)
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/nvim2-2025-11-26_09.29.48.webm" />
    </video>
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/nvim2-2025-11-26_09.30.31.webm" />
    </video>
- Go to file ✅ `<leader>f`
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/nvim2-2025-11-26_09.30.53.webm" />
    </video>
- Go to class ✅ `<leader>S` (`telescope` + LSP)
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/nvim2-2025-11-26_09.31.45.webm" />
    </video>
- Rename ✅ `grn`
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/nvim2-2025-11-26_09.32.07.webm" />
    </video>
- Find and replace in current file `:%s/<search>/<replace>`
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/nvim2-2025-11-26_09.32.25.webm" />
    </video>
- Generate code ❌
- Go to implementation ✅ `gri`
- Introduce variable ❌
- Change case ✅ (`text-case.nvim` or through external program)
- Implement methods ❌

## Helix

<img src="/images/modal-text-editors/helix-0.png" loading="lazy" alt="Helix">

Actually, my first try was Helix, but I immediately stumbled upon the first blocker - the task I was working on at that time involved some Mustache templates. And Helix did not support it even on the most basic level (syntax highlighting). Moreover, Helix did not have plugins at the time, so there was little I could do for that particular task. The reason I wanted at least syntax highlighting had to do with the issue I was working on, which had misplaced conditionals in the template file, resulting in an incorrect rendering. I liked, however, how Helix came with a lot of handy utilities out of the box - the file picker, treesitter integration (so I could jump between the rest of Java / TypeScript codebase with ease).

I tried configuring a file tree via [Yazi](https://github.com/sxyazi/yazi), which has an integration example on their website (using [zellij](https://github.com/zellij-org/zellij) terminal multiplexer panels), but it just refused to work for me on my Mac. That was when I timeboxed this and decided to switch my focus to NeoVim for the moment being.

After a while of just living my life, a new version of Helix has dropped, 25.07. And it introduced a few quality o flife changes. For example, the file explorer, which is now built in. It is not as powerful as Yazi, but it does the job.
So I decided to switch to Helix for a while.

Upon getting back to Helix some time after, there was a new release, [25.07.1](https://helix-editor.com/news/release-25-07-highlights/) which introduced a few quality of life improvements, including the file browser, sort of addressing the file tree issue I had before. Invoked with `<leader>e` (compared to `<lead>f` which just lists all the files in one long list), this one allows you to see directories. It is sort of an immediate-mode file manager as in it only shows the contents of a selected directory, but it is arguably more useful solution to the file tree problem than a list of all the files (recursively listing sub-directories). Additionally, tree-sitter was replaced so the syntax highlighting should be better - this would most likely be helpful in content like my blogs, often featuring multiple languages on top of the main Markdown.

Something that I liked about Helix was how it handles selections. For the most part, native multi-cursor is a great start - you just have to remember the difference between splitting the selection and selecting inside the selection - `S` (split) vs `s` (select) -  both would create multiple cursors, just in a different way.

Different selection objects, including the ones provided by the tree-sitter - with the [match mode](https://docs.helix-editor.com/keymap.html#match-mode) you can select different objects (`ma` and `mi` are your friends). But you can also surround the selection (`ms<char>`) and change the surrounding (`mr<from><to>`) and delete the surrounding (`md<char>`), which I find pretty handy sometimes.

Some things I find bearable are the use of external tools for some actions, instead of plugins. Since Helix lacks plugin system in any shape or form (as of late October 2025), you can just pipe the selection to the external program. One such case is changing the case of selection - I use this **often**, especially in conjunction with multiple cursors - to quickly edit, say test data or a bunch of constants or to refactor multiple class fields. In this case I found a [comment on Github](https://github.com/helix-editor/helix/issues/5197#issuecomment-2569811785) suggesting the use of [ccase](https://github.com/rutrum/ccase) - you first need to install this Rust utility, ccase, and then you'll be able to send your selection(-s) by using `:pipe ccase -t screamingsnake`, for instance.

Something I did not realize until writing this blog is that there are some really useful default keybindings for insert mode, like `Ctrl+w` to delete the previous word, `Alt+w` to delete the next word, `Ctrl+k` to delete to the end of the line and `Ctrl+x` to trigger autocomplete.

One annoying thing about Helix is the buffer picker (available via `<lead>b`) - it shows the list of open buffers (files). It does not appear to be sorting the elements of the list and the first element is always the _current buffer_. This is not really helpful if you want to quickly go the previously edited file. To make things worse, it does not seem to have any configuration around it, so the behaviour is pretty much set in stone.

I also am still missing one feature which made me try NeoVim in the first place, which is [`flash.nvim`](https://github.com/folke/flash.nvim) for quick jumps on the screen. Helix team did add something similar to `HopWord` command from [`hop.nvim`](https://github.com/smoka7/hop.nvim), called `goto_word`, available via `gw` key shortcut. But unlike `hop.nvim`, Helix implementation is barely configurable (you can only configure the alphabet used for making labels) and comes with just one mode - `HopWord`. Additionally, it enforces exactly two-character abbreviations (jump labels). I thought this is rather limiting and since Helix is an open-source project, I decided to implement a behaviour similar to `leap.nvim`, since it is much less obstructive (it does not cover your entire screen in labels, making it super hard to figure out where you want to jump) and it allows to narrow down the places to jump by having prefix search. So I raised a [PR](https://github.com/helix-editor/helix/pull/14644) and a [suggestion discussion](https://github.com/helix-editor/helix/discussions/14653) on Helix Github repo. To which the team just said "we don't want it" and dismissed the proposal:

> We made intentional choices when implementing the goto_word behavior. We were quite aware if the nvim plugins. We are not going to replace or add alternative commands

One comment pointed to the plugin system when it is available:

> I'm not inclined to add more jumping commands as core Helix commands like gw. There are a lot of different spins on jumping functions in Neovim plugins and I believe that future work should be done in plugins (once available) rather than as core faetures.

For context, plugin system has been [discussed](https://github.com/helix-editor/helix/discussions/3806) for over three years (as the moment of this writing, since September 2022) and apparently has been [worked on](https://github.com/helix-editor/helix/pull/8675) for over two years (since October 2023). So I have no hope of seeing it released in the near future.

But instead of giving up, I decided to give a try to this work-in-progress plugin system and after a week a [`flash.hx`](https://github.com/shybovycha/flash.hx/) plugin was born.

Here is my condensed cheatsheet of key shortcuts I use in Helix:

- Pickers
    - `<lead>b` buffer list
    - `<lead>e` file explorer; far superior to file list
    - `<lead>/` search
    - `<lead>d` show diagnostics (hints, errors, warnings, etc)
- Multiple cursors
    - `,` collapse all cursors into one
    - `s` select in selection; create a new cursor at each selection; takes a regexp as a pattern
    - `S` split selection; create a new cursor at each occurrence of pattern; takes regexp as a pattern
- Selection
    - `v` enters visual (selection) mode; this is how you can select multiple words - `vwwww`
    - `m` enters marking mode; this is how you can select paragraphs, text and LSP objects, add, change and remove wrapping characters (like brackets and quotes); few examples:
        - `mi(` - select inside parentheses
        - `ma"` - select everything inside double-quotes and the double-quotes themselves
        - `ms[` - surround selection with square brackets
        - `mr[(` - replace the surrounding square brackets with regular braces
        - `mif` - select the body of a function (method)
    - `=` / `<` / `>` - format / unindent / indent selection
    - `:pipe` or `|` - send selection to an external program and replace selection with the output of said program
        * as of version 25.7 you can also use interpolation with `%{}`, for instance, `%{cursor_line}` or `%{buffer_name}`, so you can pass in the filename to the external tool
    - `%` - select an entire buffer (file)
    - `<lead><c>` / `<lead><C>` - comment/uncomment the selection
- Go to
    - `gl` / `gh` - go to end of line / start of line (contrary to `$` and `^` in Vim)
    - `gg` / `ge` - go to first line / last line of the file
    - `Ctrl+i` / `Ctrl+o` - go back and forth in your jumplist (places where the cursor has been placed; jumplist itself is available with `<lead>j`)
    - `gd` / `gD` - go to definition / declaration
    - `gr` - go to references
- Copying (yanking)
    - `"+y` - copy to the OS clip buffer (translates to "change the buffer to the `+` - OS clipboard and then yank the selection")

After a few weeks of working in Helix I was pleasantly surprised by how much out-of-the-box stuff it comes shipped with. And I did not have to spend a whole lot of time configuring it (aside from that week I spent developing `flash.hx`).

It checks most of my boxes too:

- Recent files ✅ `<leader>b`
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/helix2-2025-11-26_09.47.19.webm" />
    </video>
- Go to declaration ✅ `gd`
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/helix2-2025-11-26_09.48.21.webm" />
    </video>
- Code completion ✅ `C-x`
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/helix2-2025-11-26_09.48.52.webm" />
    </video>
- Syntax-aware selection ✅ `m<selection><see tooltip>`
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/helix2-2025-11-26_09.49.32.webm" />
    </video>
- Find in files ✅ `<leader>/` (simplified)
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/helix2-2025-11-26_09.50.42.webm" />
    </video>
- Context actions ✅ `<leader>a` (depends on LSP)
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/helix2-2025-11-26_09.51.04.webm" />
    </video>
- Find in current file ✅ `/`
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/helix2-2025-11-26_09.51.22.webm" />
    </video>
- Toggle comment ✅ `<leader>c`
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/helix2-2025-11-26_09.51.42.webm" />
    </video>
- Go to file ✅ `<leader>f`
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/helix2-2025-11-26_09.52.11.webm" />
    </video>
- Go to class ✅ `<leader>S` (capital `S` for workspace symbols, lowercase `s` for current file symbols) (partial)
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/helix2-2025-11-26_09.52.37.webm" />
    </video>
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/helix2-2025-11-26_09.53.15.webm" />
    </video>
- Rename ✅ `<leader>r`
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/helix2-2025-11-26_09.53.45.webm" />
    </video>
- Find and replace in current file ⚠️ (unconventinal): select scope (`%` for current buffer), then use `s` to select occurrences or `S` to split selections into multiple cursors, then use actions - `i` or `c` to change the selections, `d` to delete selections
- Generate code ❌
- Go to implementation ✅ `gi` (depends on LSP)
- Introduce variable ❌
- Change case ✅ (through external tool): select text, then `|` (pipe it), then specify external program (`ccase -t snake`, for instance)
    <video preload autoplay loop muted style="max-width:100%">
    <source src="/images/modal-text-editors/helix2-2025-11-26_09.55.39.webm" />
    </video>
- Implement methods ❌

## Kakoune

_TBD_

## Final thoughts

I was surprised by how nice these seemingly limiting editors have become over the years (last time I used them was over-configured Emacs back in circa 2015)!

Although rough around the edges and coming with massive pros and massive cons, both editors are being actively developed to become even nicer.

Neovim:

- 👍 comes with some quite nice things out of the box
- 👎 the out-of-the-box niceities need LSP to be plugged in, which comes through a plugin manager
- 👍 insane amount of plugins for all sorts of things
- 👎 a proportionally insane amount of time required to find and configure the plugins to make for a comfortable experience
- 👍 overall, the experience that you _can_ create is on its own league
- 👍 has a nice file tree implementation (yes, I like to see the structure of the directory and its parents)
- 👍 has *very* nice semantic and LSP selection **expand**
- 👍 has an OG Flash implementation
- 👍 has nice surround behaviour
- 👍 file picker powered by ripgrep so you can filter all you want
- 👎 both selection expand and surround are a bit tricky to trigger (`gsn` followed by `gss`; `ysaw"` and `cs[(` and `ds"`)

Helix:

- 👍 comes with **a ton** of nice things out of the box
- 👍 none of the niceities require extensive configuration (if any at all)
- 👎 very slow development cycle, so new features do not come out often (like twice a year, I believe)
- 👎 no plugin system, making the new features even less frequent or even possible
- 👎 very strongly opinionated developers, so if you are not comfortable with a feature - tough luck!
- 👎 does not have file tree
- 👎 file picker is very basic - only offers filtering by partial path match
- 👎 semantic selection expansion is not exactly *expansion*, but more like "select this exact scope"
- 👍 surround and semantic selection triggers are consistent e.g. make much more sense (`ms(`, `mr([`, `md[`; `maf`, `maa`)

My current stance is that Neovim is a much stronger contender with a much broader set of (much better implemented) features, but the amount of time you have to spend to get to that state is enormous. And whilst much nicely organised out of the box, Helix is very much undercooked (in my opinion).

Hence, for powerful and actual workloads - use Neovim, it is worth spending all that time configuring it for that purpose. For a casual relaxing but very restricted editing - Helix is an-okay choice.
