---
layout: post
title: Speeding up with Ruby native extensions
date: '2014-11-15T00:21:00+01:00'
tags:
- ruby
- programming
- c
- optimization
tumblr_url: http://shybovycha.tumblr.com/post/102644483371/speeding-up-with-ruby-native-extensions
---

<img alt="" src="https://31.media.tumblr.com/5c826c2ac8e6d86d2aa4b21715002736/tumblr_inline_nf1y0xXwtM1qh5oee.jpg"/>

## Foreword

At my job, our current project has many bottle-necks, where Ruby really sucks on its performance. We were thinking on how to optimize them, and finally come to usage of Ruby Native API.

Our project uses Redis and MySQL hardly, so much of statistic data is stored in Redis. For speeding up. But one fine day made us use a <strong>reduce</strong> on a set of statistic data from Redis. And that’s where we got stuck on Ruby’ performance. Our server timed out in a minute of waiting for that reduce to complete.

<!--more-->

The trouble was in a loop like this:

```ruby
json_data = JSON.pase(json_file)
keys = $redis.keys "*:hash_pattern:date:*"
count, total_count = 0, 0

keys.each do |hash_key|
    elements = $redis.hgetall hash_key

    elements.each do |key, value|
        i_value = value.to_i

        count += i_value if key =~ /some:regex/ and json_data.has_key? key
        total_count += i_value
    end
end
```

My first attempt was implemented on a D language. But when I tried to use the compiled code library with Ruby, I failed. That’s why I thought <em>I feel more comfortable with C/C++ than with D</em>. And wrote the same code on C/C++. I took three third-party libraries:

* `RE2` for regular expressions
* `hiredis` for Redis operations
* `rapidjson` for JSON parsing

But when I compiled and ran what I’ve done, I could not believe my eyes - the process worked for **59 seconds**!
That was more than **ten times** slower than Ruby version!

So, I started optimizing for speed.

<img alt="" src="https://31.media.tumblr.com/69082ba5dc2c74b2e5d9059a05a3b07a/tumblr_inline_nf1xzoyiAa1qh5oee.jpg"/>

<!--more-->

First of all, I dropped regular expressions as they were simply replaced by substring check and substring extraction (as the first part of a string in a regular expression had a fixed length). That did the trick, lowering the execution time to <strong>25 seconds</strong>. Yet, it was too much.

The last step I took, I removed hiredis and replaced it with a set of five custom functions, performing only those operations, which we needed via sockets. First, that failed with a really, REALLY long segfault. Yet, when I replaced the host string from <em>&ldquo;localhost&rdquo;</em> to <em>&ldquo;127.0.0.1&rdquo;</em>, my tiny extension arose and did its job in <strong>4.8 seconds</strong>.

That was great! Yet, it is not the best time I can get, let’s take a look on what was done and in which manner.

## Creating native extensions for Ruby

Creating a native extension will need you to have compiled **shared object** file. Shared object is a library for POSIX OSes.
There are two kinds of library formats for Linux and others:

* **shared libraries** (`*.so` files) - could be placed anywhere and used in a runtime by a few applications
* **static libraries** (`*.a` files) - are bundled to a compile target (library, executable...) and are used in that environment

For that purpose you’d better use **C/C++ Ruby API**.
Yes, you _could_ use other-language-compiled shared libraries, but through an interface called **FFI**,
which I did not manage to work for me. Thus, this article covers only the C/C++ way.

To make your extension available in Ruby, you will need to define some of these:

* method for existing classes and modules
* new class or module

All of them are not hard to implement. We will make our own module and define its method.

First, create a directory names as your extension will be named. Let’s say, `my_ext`. Create two files there - `my_ext.cpp` and `extconf.rb`.
First file will define an extension shared library, whilst the second one will create `Makefile` for us.

Our extension will have a very simple source file with just one non-standard include and two functions defined:

```c
#include "ruby.h"

#include <string.h>
#include <stdlib.h>

VALUE moo_method(VALUE _name, VALUE _age, VALUE _self) {
    char* name = StringValueCStr(_name);
    unsigned int age = num2uint(_age);

    char* result = (char*) malloc(255 * sizeof(char));
    sprintf(result, "Hello, my name is %s and I am %d years old!\n", name, age);

    return rb_str_new2(result);
}

extern "C"

void Init_my_ext() {
  VALUE MyModule = rb_define_module("MyModule");
  rb_define_module_function(MyModule, "moo", reinterpret_cast(moo), 2);
}
```

Now let’s look at this source. There is only one exported function, `Init_my_ext`. That’s correct, because all our extension needs to do is to define something. And that is done in that method. The function `Init_my_ext` should have such name format: `Init_$extension_name$`. That’s how Ruby finds out what to call first.

Now, there are many of those `VALUE` type instances. That is internal type of Ruby Native API. That is the variant type, holding Ruby’ value. And whilst Ruby is not strongly typed language, that type could contain anything - from `nil` to `string` and even `object`. There are a few really useful functions defined in `ruby.h` to help you checking variables for types and converting them to C++ types.

Then we define a module named `MyModule` and stored its reference in the `MyModule` variable. Then we can do what we want with that module - define classes, variables and methods. Let’s see how we defined a method. Function `rb_define_module_function` contains four arguments:

* **reference to a module**
* **method name**
* **pointer to a C function, representing method internals** - note the `reinterpret_cast`
* **argument count** - when this number is less than zero, than method will receive three arguments - `int argc`, `VALUE* argv` and `VALUE self`, representing variable amount of arguments; if this number is greater than zero - it defines the amount of required method arguments

Now, lets create a `extconf.rb` file, which will create `Makefile` for final library compilation:

```ruby
require 'mkmf'

extension_name = 'my_ext'

def get_dir(name)
    File.expand_path(File.join(File.dirname(__FILE__), name))
end

LIBDIR     = RbConfig::CONFIG['libdir']
INCLUDEDIR = RbConfig::CONFIG['includedir']

HEADER_DIRS = [ INCLUDEDIR ]

# setup constant that is equal to that of the file path that holds that static libraries that will need to be compiled against
LIB_DIRS = [ LIBDIR ]

libs = []

# The destination
dir_config(extension_name, HEADER_DIRS, LIB_DIRS)

libs.each do |lib|
    $LOCAL_LIBS << "#{lib} "
end

# Additional compiler / linker flags
# $CFLAGS << " -fPIC "
# $LDFLAGS << " -lpthread "

# Do the work
create_makefile(extension_name)
```

That’s it, it defines parameters for our future `Makefile`. Note the `get_dir(name)` method - I’ve defined it for you to simplify adding library sub-directories to the `LIBDIR` and `INCLUDEDIR` arrays, just like this:

```ruby
LIB_DIRS = [ LIBDIR, get_dir('hiredis') ]
```

Also, note the `-fPIC` option - it is needed for most libraries to compile under different architectures. So, you may need to add them to your third-party libraries’ Makefiles to resolve corresponding compiler errors when building the extension.

When you are done, let’s generate Makefile:

```ruby
ruby extconf.rb
```

Then, you should be able to build your shared object with

```bash
$ make
```

Using our extension is simple when playing around locally - you just add it to your `irb` or `ruby` command-line arguments like this:

```bash
$ irb -r ./my_ext.so
```

And then just using the modules you’ve defined. But in most situations, that is impossible, as, for example, you are running a Rails application on a production server. So, you will probably want a RubyGem for that purpose.

## Wrapping extension in a Gem

Building a Ruby Gem containing native extension is a little different than building usual gems. You here have two options:

* bundle a pre-built library with a gem
* provide a sources to perform build on a target machine

First way is for dummies. That’s it, you will probably want your code ran on different platforms than your own machine. So, you will not want your gem to fail with a segfault like _this architecture differs from what the library was built on_. Thus, we will concentrate on a second way.

<img alt="" src="https://31.media.tumblr.com/fa991a97c370065b424f4e58f57fa947/tumblr_inline_nf1y1oTQRp1qh5oee.jpg"/>

First, we will need a correct directory structure:

```
.
├── ext
│   └── my_gemname
│       ├── extconf.rb
│       └── my_ext.cpp
├── lib
│   └── my_gemname.rb
└── my_gemname.gemspec
```

File `lib/my_gemname.rb` will contain only the extension initialization call:

```ruby
require 'my_gemname/my_ext'
```

Whilst the main difference hides in gemspec file:

```ruby
Gem::Specification.new do |spec|
  spec.name = 'my_gemname'
  spec.version = '0.1'
  spec.description = 'Some cool description here'
  spec.summary = 'Short description'
  spec.email = 'author@email.com'
  spec.homepage = ''
  spec.author = 'Author Name'
  spec.files = Dir['lib/**/*.rb'] + Dir['ext/**/*']
  spec.platform = Gem::Platform::RUBY
  spec.require_paths = [ 'lib', 'ext' ]
  spec.extensions = Dir['ext/my_gemname/extconf.rb']
end
```

Here four lines make the magick:

```ruby
  spec.files = Dir['lib/**/*.rb'] + Dir['ext/**/*']
  spec.platform = Gem::Platform::RUBY
  spec.require_paths = [ 'lib', 'ext' ]
  spec.extensions = Dir['ext/my_gemname/extconf.rb']
```

They set, respectively:

* directories of the extension with all the files and sub-directories, needed to compile it
* universal target platform
* extension required path
* path to the extension’ extconf file

Now you can build your gem with

```bash
$ gem build my_gemname.gemspec
```

Using the gemfile may require you never to push it to RubyGems repository. For example, when your gem is a very specific for the project you are working on, or it may conflict with your job contract. But you can’t simply specify the `path` attribute for your gem in the `Gemfile` - it just does not work!

Way to solve this lays beyound using custom repository. My solution was to create a directory under `lib/` sub-directory of our project:

```
repository
└── gems
    └── my_gemname-0.1.gem
```

Then, go to the `repository` directory (that’s important NOT to go to the `gems` subdir) and run this magic command:

```bash
$ gem generate_index
```

This will make your repository directory look like this:

```
repository
├── gems
│   └── my_gemname-0.1.gem
├── latest_specs.4.8
├── latest_specs.4.8.gz
├── prerelease_specs.4.8
├── prerelease_specs.4.8.gz
├── quick
│   └── Marshal.4.8
│       └── my_gemname-0.1.gemspec.rz
├── specs.4.8
└── specs.4.8.gz
```

This directory now could be used as a RubyGems repository. Just like the <strong>rubygems.org</strong>! Just point your <strong>Gemfile</strong> to this directory:

```ruby
source File.join('file://', File.dirname(__FILE__), 'lib', 'repository')
```

And an **important note**: keep your `Gemfile` and `Gemfile.lock` up-to date - use only `= latest.version` in the `Gemfile` when running with your native extension gem!
