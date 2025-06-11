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

I tried configuring a file tree via [Yazi](https://github.com/sxyazi/yazi), which has an integration example on their website (using [zellij](https://github.com/zellij-org/zellij) terminal multiplexer panels), but it just refused to work for me. That was when I timeboxed this and decided to switch my focus to NeoVim for the moment being.

## NeoVim

So I decided to use Vim to try it out first. I thought about trying [NeoVim](https://neovim.io/) since it was the hype at the time. I was dumbfolded by the fact it did not even come with the default config file to work with (in a stumbling contrast to Helix, which has `:config-open` and `:config-reload` commands built in).
Yet I kept going, spending days perfecting my config. I switched my terminal emulator from [Warp](https://www.warp.dev/) to [Ghostty](https://github.com/ghostty-org/ghostty) (so that I can see images right inside my terminal, improving file manager experience), installed a good dozen of plugins and messed with color schemes.

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
- [toggleterm](https://github.com/akinsho/toggleterm.nvim) - terminal integration, toggleable
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
