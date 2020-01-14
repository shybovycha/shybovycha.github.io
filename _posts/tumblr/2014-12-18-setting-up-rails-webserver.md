---
layout: post
title: Setting up Rails webserver
date: '2014-12-18T10:55:00+01:00'
tags:
- rails
- rails4
- tutorial
- servers
tumblr_url: http://shybovycha.tumblr.com/post/105514079521/setting-up-rails-webserver
---

## Foreword

This tutorial I wrote when was quitting my previous job, almost one year ago. But it's still handy!

## Abstract Rails application setup

```bash
$ git clone .../project_name.git
$ cd project_name
$ [sudo] bundle install
$ cat config/database.yml
$ # create database and/or change config/database.yml settings
$ rake db:migrate RAILS_ENV=production
$ rake db:seed RAILS_ENV=production # don't worry if one fails
$ # start the server of your choice
``

## `Puma` webserver

### Application-wide settings

First you need to set up **Puma** for your specific project. For this purpose, add this
line to the `Gemfile`:

```ruby
gem 'puma'
```

Then, run `[sudo] bundle install`.

When you are done, you should be able to create a Puma config file at `$PROJECT_DIR/config/puma.rb`:

```ruby
def home_dir
    '/home/user/$PROJECT_DIR/'
end

def path(p)
    File.join(home_dir, p)
end

directory home_dir
environment 'development'
daemonize
pidfile path('tmp/pids/puma.pid')
state_path path('tmp/pids/puma.state')
stdout_redirect path('log/puma.log'), path('log/error.puma.log'), true
threads 0, 1
bind 'tcp://0.0.0.0:5100'
activate_control_app
```

More details here: <a href="https://github.com/puma/puma/blob/master/examples/config.rb">https://github.com/puma/puma/blob/master/examples/config.rb</a>

Now, add project root path to the `/etc/puma.conf` file, e. g.:

```
/home/user/project_name
```

### Start Puma at boot

There is a specific utility, called **Jungle**. It manages your applications' instances at startup.

#### Ububtu-based systems

First of all, create `/etc/init/puma.conf` file and fill it with this:

```bash
# /etc/init/puma.conf - Puma config

# This example config should work with Ubuntu 12.04+.  It
# allows you to manage multiple Puma instances with
# Upstart, Ubuntu's native service management tool.
#
# See workers.conf for how to manage all Puma instances at once.
#
# Save this config as /etc/init/puma.conf then manage puma with:
#   sudo start puma app=PATH_TO_APP
#   sudo stop puma app=PATH_TO_APP
#   sudo status puma app=PATH_TO_APP
#
# or use the service command:
#   sudo service puma {start,stop,restart,status}
#

description "Puma Background Worker"

# no "start on", we don't want to automatically start
stop on (stopping puma-manager or runlevel [06])

# change apps to match your deployment user if you want to use this as a less privileged user (recommended!)
setuid apps
setgid apps

respawn
respawn limit 3 30

instance ${app}

script
# this script runs in /bin/sh by default
# respawn as bash so we can source in rbenv/rvm
# quoted heredoc to tell /bin/sh not to interpret
# variables
exec /bin/bash <<'EOT'
  # set HOME to the setuid user's home, there doesn't seem to be a better, portable way
  export HOME="$(eval echo ~$(id -un))"

  cd $app

  if [ -d "$HOME/.rbenv/bin" ]; then
    export PATH="$HOME/.rbenv/bin:$PATH"
  elif [ -f  /etc/profile.d/rvm.sh ]; then
    source /etc/profile.d/rvm.sh
  elif [ -f /usr/local/rvm/scripts/rvm ]; then
    source /etc/profile.d/rvm.sh
  elif [ -f "$HOME/.rvm/scripts/rvm" ]; then
    source "$HOME/.rvm/scripts/rvm"
  elif [ -f /usr/local/share/chruby/chruby.sh ]; then
    source /usr/local/share/chruby/chruby.sh
    if [ -f /usr/local/share/chruby/auto.sh ]; then
      source /usr/local/share/chruby/auto.sh
    fi
    # if you aren't using auto, set your version here
    # chruby 2.0.0
  fi

  logger -t puma "Starting server: $app"

  exec bundle exec puma -C config/puma.rb
EOT
end script
```

Now, create `/etc/init/puma-manager.conf` and fill it with this:

```bash
# /etc/init/puma-manager.conf - manage a set of Pumas

# This example config should work with Ubuntu 12.04+.  It
# allows you to manage multiple Puma instances with
# Upstart, Ubuntu's native service management tool.
#
# See puma.conf for how to manage a single Puma instance.
#
# Use "stop puma-manager" to stop all Puma instances.
# Use "start puma-manager" to start all instances.
# Use "restart puma-manager" to restart all instances.
# Crazy, right?
#

description "Manages the set of puma processes"

# This starts upon bootup and stops on shutdown
start on runlevel [2345]
stop on runlevel [06]

# Set this to the number of Puma processes you want
# to run on this machine
env PUMA_CONF="/etc/puma.conf"

pre-start script
  for i in `cat $PUMA_CONF`; do
    app=`echo $i | cut -d , -f 1
    logger -t "puma-manager" "Starting $app"
    start puma app=$app
  done
end script
```

And create a blank `/etc/puma.conf` file. This will be filled for each application separately.

#### Caveat:

You need to customise `/etc/init/puma.conf` to:

* Set the right user your app should be running on unless you want root to execute it!
  * Look for `setuid apps` and `setgid apps`, uncomment those lines and replace `apps` to whatever your deployment user is.
  * Replace `apps` on the paths (or set the right paths to your user's home) everywhere else.
* Uncomment the source lines for `rbenv` or `rvm` support unless you use a system wide installation of Ruby.

Now, start Jungle like this: `sudo start puma-manager`.
And all your applications should be available when you reboot the machine.

More details at <a href="https://github.com/puma/puma/tree/master/tools/jungle/">https://github.com/puma/puma/tree/master/tools/jungle/</a>

#### Debian-based systems

_TBD_

## Starting up and shutting down

To start up the application is easy enough. Just navigate yourself to project directory and run the following: `puma -C config/puma.rb`.

If you want to shut down one, run this command in the project directory: `[sudo] pumactl -S tmp/pids/puma.state halt`.
