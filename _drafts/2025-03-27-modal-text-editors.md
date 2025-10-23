---
layout: post
title: 'Modal text editors'
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

The quick action menu has 6 tabs:

- Classes
- Files
- Symbols
- Actions
- Text
- All (combined of the above)

For the most part I only use Classes, Files and Actions tabs via a keyboard shortcut.

The only other two features of the IDE I actually use are the file tree (so that I can see the _structure_ of the project - it comes handy for Java packages organization, for example) which I configure to automatically focus the file currently opened in the editor and the terminal, which I use for pretty much everything else (including Git interactions and running / building project).

So this would be my measure of success in other editors (in terms of comfort) - on top of movement in a document, I would like to compare the editors in terms of moving in the _project_ (files, symbols / classes and references), quick action access and terminal integration.

## Helix

My first take was Helix, but I immediately stumbled on the first roadblock - the task I was working on at that time involved some Mustache templates. And Helix did not support it even on the most basic level (syntax highlighting). Moreover, Helix did not have plugins at the time, so there was little I could do for that particular task. The reason I wanted at least syntax highlighting had to do with the issue I was working on, which had misplaced conditionals in the template file, resulting in an incorrect rendering. I liked, however, how Helix came with a lot of handy utilities out of the box - the file picker, treesitter integration (so I could jump between the rest of Java / TypeScript codebase with ease).

I tried configuring a file tree via [Yazi](https://github.com/sxyazi/yazi), which has an integration example on their website (using [zellij](https://github.com/zellij-org/zellij) terminal multiplexer panels), but it just refused to work for me on my Mac. That was when I timeboxed this and decided to switch my focus to NeoVim for the moment being.

Upon getting back to Helix, there was a new release, [25.07.1](https://helix-editor.com/news/release-25-07-highlights/) which introduced a few quality of life improvements, including the file browser, sort of addressing the file tree issue I had before. Invoked with `<leader>e` (compared to `<lead>f` which just lists all the files in one long list), this one allows you to see directories. It is sort of an immediate-mode file manager as in it only shows the contents of a selected directory, but it is arguably more useful solution to the file tree problem than a list of all the files (recursively listing sub-directories). Additionally, tree-sitter was replaced so the syntax highlighting should be better - this would most likely be helpful in content like my blogs, often featuring multiple languages on top of the main Markdown.

Something that I liked about Helix was how it handles selections. For the most part, native multi-cursor is a great start - you just have to remember the difference between splitting the selection and selecting inside the selection - `S` (split) vs `s` (select) -  both would create multiple cursors, just in a different way.

Different selection objects, including the ones provided by the tree-sitter - with the [match mode](https://docs.helix-editor.com/keymap.html#match-mode) you can select different objects (`ma` and `mi` are your friends). But you can also surround the selection (`ms<char>`) and change the surrounding (`mr<from><to>`) and delete the surrounding (`md<char>`), which I find pretty handy sometimes.

Some things I find bearable are the use of external tools for some actions, instead of plugins. Since Helix lacks plugin system in any shape or form (as of late October 2025), you can just pipe the selection to the external program. One such case is changing the case of selection - I use this **often**, especially in conjunction with multiple cursors - to quickly edit, say test data or a bunch of constants or to refactor multiple class fields. In this case I found a [comment on Github](https://github.com/helix-editor/helix/issues/5197#issuecomment-2569811785) suggesting the use of [ccase](https://github.com/rutrum/ccase) - you first need to install this Rust utility, ccase, and then you'll be able to send your selection(-s) by using `:pipe ccase -t screamingsnake`, for instance.

Something I did not realize until writing this blog is that there are some really useful default keybindings for insert mode, like `Ctrl+w` to delete the previous word, `Alt+w` to delete the next word, `Ctrl+k` to delete to the end of the line and `Ctrl+x` to trigger autocomplete.

One annoying thing about Helix is the buffer picker (available via `<lead>b`) - it shows the list of open buffers (files). It does not appear to be sorting the elements of the list and the first element is always the _current buffer_. This is not really helpful if you want to quickly go the previously edited file. To make things worse, it does not seem to have any configuration around it, so the behaviour is pretty much set in stone.

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
    - `%` - select an entire buffer (file)
- Go to
    - `gl` / `gh` - go to end of line / start of line (contrary to `$` and `^` in Vim)
    - `gg` / `ge` - go to first line / last line of the file
    - `Ctrl+i` / `Ctrl+o` - go back and forth in your jumplist (places where the cursor has been placed; jumplist itself is available with `<lead>j`)
    - `gd` / `gD` - go to definition / declaration
    - `gr` - go to references
- Copying (yanking)
    - `"+y` - copy to the OS clip buffer (translates to "change the buffer to the `+` - OS clipboard and then yank the selection")

After a while of just living my life, a new version of Helix has dropped, 25.07. And it introduced a few quality o flife changes. For example, the file explorer, which is now built in. It is not as powerful as Yazi, but it does the job.
So I decided to switch to Helix for a while.

Here are the important notes I have learned:

* `m` lesser mode - allows you to select inside / outside words, paragraphs, LSP entities (function body, arguments, etc.) and change the surrounding
* search
  * selecting a word and entering a visual mode with `v` allows you to extend selection with the next occurrence of the selected text by using `n` or previous occurrence with `N`; to skip the current match, the view mode is used with `z`, making it `zn` to ignore the current occurrence and go to the next
  * the selection this way would create multiple cursors, so go wild!
* adding a new cursor with `C`
* collapsing cursors with `,`
* file explorer with `<Spc><e>` or `<Spc><E>` to open it in the directory of a current buffer
* commenting the selection with `<Spc><c>` and `<Spc><C>`
* piping the selection to the external command with just `|` (pipe character)
  * use interpolation with `%{}`, for instance, `%{cursor_line}` or `%{buffer_name}`
* searching within the selection with `s`
* using external tool like `ccase` (from Cargo) to change case: `| ccase -t screamingsnake`

## NeoVim

So I decided to use Vim to try it out first. I thought about trying [NeoVim](https://neovim.io/) since it was the hype at the time. I was dumbfolded by the fact it did not even come with the default config file to work with (in a stumbling contrast to Helix, which has `:config-open` and `:config-reload` commands built in).
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

## Kakoune

I kept pushing forward. 
