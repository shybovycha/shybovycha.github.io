---
layout: post
title: Improving ZSH startup performance
date: '2018-06-01T14:00:00+10:00'
---

## Adding profiling to ZSH

Add this line as the first one, to the very top of your `~/.zshrc`:

```
zmodload zsh/zprof
```

And add this one to the very bottom of this same file:

```
zprof
```

Now, whenever you open up ZSH, you shall see a list of things which start and how much time each entry takes to load.
For me it was like this:

```
num  calls                time                       self            name
-----------------------------------------------------------------------------------
 1)    1        1123.49  1123.49   87.59%   1123.49  1123.49   87.59%  _pyenv-from-homebrew-installed
 2)    1          84.02    84.02    6.55%     59.50    59.50    4.64%  compinit
 3)    3          26.94     8.98    2.10%     26.94     8.98    2.10%  __sdkman_export_candidate_home
 4)    3          25.77     8.59    2.01%     25.77     8.59    2.01%  __sdkman_prepend_candidate_to_path
 5)    2          24.52    12.26    1.91%     24.52    12.26    1.91%  compaudit
 6)    2           7.98     3.99    0.62%      7.98     3.99    0.62%  grep-flag-available
 7)    2           7.54     3.77    0.59%      7.54     3.77    0.59%  env_default
 8)    2           2.43     1.22    0.19%      2.43     1.22    0.19%  _has
 9)   23           2.43     0.11    0.19%      2.43     0.11    0.19%  compdef
10)    1           1.09     1.09    0.09%      1.09     1.09    0.09%  colors
11)   12           0.38     0.03    0.03%      0.38     0.03    0.03%  is_plugin
12)    1           0.38     0.38    0.03%      0.38     0.38    0.03%  is-at-least
13)    1           0.21     0.21    0.02%      0.21     0.21    0.02%  _homebrew-installed
14)    1           0.03     0.03    0.00%      0.03     0.03    0.00%  __sdkman_echo_debug

-----------------------------------------------------------------------------------
```

I was able to measure the total startup time by running `time zsh -i -c exit`. And the total startup time was **almost two seconds**.

```
zsh -i -c exit  1.16s user 0.73s system 100% cpu 1.889 total
```

As you can see, `pyenv` takes whole bunch of time (1123 millis!). So I do have the `pyenv` plugin for ZSH and I did disable it. Here's what happened afterwards:

```
num  calls                time                       self            name
-----------------------------------------------------------------------------------
 1)    1          91.42    91.42   53.38%     62.48    62.48   36.48%  compinit
 2)    2          28.94    14.47   16.90%     28.94    14.47   16.90%  compaudit
 3)    3          28.62     9.54   16.71%     28.62     9.54   16.71%  __sdkman_export_candidate_home
 4)    3          27.65     9.22   16.14%     27.65     9.22   16.14%  __sdkman_prepend_candidate_to_path
 5)    2           8.27     4.14    4.83%      8.27     4.14    4.83%  grep-flag-available
 6)    2           8.22     4.11    4.80%      8.22     4.11    4.80%  env_default
 7)    2           2.55     1.28    1.49%      2.55     1.28    1.49%  _has
 8)   23           2.50     0.11    1.46%      2.50     0.11    1.46%  compdef
 9)    1           1.24     1.24    0.72%      1.24     1.24    0.72%  colors
10)    1           0.41     0.41    0.24%      0.41     0.41    0.24%  is-at-least
11)   10           0.37     0.04    0.21%      0.37     0.04    0.21%  is_plugin
12)    1           0.02     0.02    0.01%      0.02     0.02    0.01%  __sdkman_echo_debug

-----------------------------------------------------------------------------------
```

```
zsh -i -c exit  0.28s user 0.23s system 96% cpu 0.526 total
```

## NVM

Not to forget I had NVM installed with the default settings.

With it I had the startup time of `1.9` seconds:

```
zsh -i -c exit  1.17s user 0.74s system 99% cpu 1.916 total
```

If you follow this [simple instruction](http://www.growingwiththeweb.com/2018/01/slow-nvm-init.html) or just replace these two lines, initializing NVM in your `~/.zshrc` file:

```bash
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
```

with these lines:

```bash
if [ -s "$HOME/.nvm/nvm.sh" ] && [ ! "$(type __init_nvm)" = function ]; then
 export NVM_DIR="$HOME/.nvm"

 [ -s "$NVM_DIR/bash_completion" ] && . "$NVM_DIR/bash_completion"

 declare -a __node_commands=('nvm' 'node' 'npm' 'yarn' 'gulp' 'grunt' 'webpack')

 function __init_nvm() {
   for i in "${__node_commands[@]}"; do unalias $i; done
   . "$NVM_DIR"/nvm.sh
   unset __node_commands
   unset -f __init_nvm
 }

 for i in "${__node_commands[@]}"; do alias $i='__init_nvm && '$i; done
fi
```

The start-up time drops by `0.1` second. But it still is an improvement, right?
