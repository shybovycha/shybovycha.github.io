---
layout: post
title: '.gitignore is not ignoring'
---

This is going to be a very short blog. I have been struggling with this issue for few days now - having a seemingly valid `.gitignore` file which does not make Git ignore any of the files.

The `.gitignore` file contents:

```gitignore
node_modules

**/*.bundle.js
*.log

*compiled-proto*
*compiled-bundle*
```

Running `git status`:

```
$ git st
On branch master
Untracked files:
  (use "git add <file>..." to include in what will be committed)
        node_modules/
        test1/flatbuffers-compiled-proto/
        test5/index.bundle.js
        test5/test5-avro.bundle.js
        test5/test5-bson.bundle.js
        test5/test5-cbor.bundle.js
        test5/test5-flatbuffers-compiled-proto/
        test5/test5-flatbuffers-compiled.bundle.js
        test5/test5-messagepack.bundle.js
        test5/test5-protobuf-compiled-proto.js
        test5/test5-protobuf-compiled.bundle.js
        test5/test5-protobuf.bundle.js

nothing added to commit but untracked files present (use "git add" to track)
```

Weird, isn't it?

There is a command which checks if a given path (whatever you pass as a parameter to the command, so technically just a string) would be ignored by any of the rules in `.gitignore` file or not:

```
git check-ignore --verbose <path>
```

Let's run it on my repo:

```
$ git check-ignore --verbose node_modules
```

No output means the path (in this case - `node_modules`) will **not** be ignored. Which should not be the case - there's a rule as the first line of the `.gitignore` file, right?!

The issue seems to be somewhat hidden - the file was saved in the `UTF16-LE` encoding, since I have used PowerShell to initialize the file with `echo 'node_modules' >> .gitignore`:

<img data-src="/images/gitignore-not-ignoring/Screenshot 2022-05-11 093618.webp">

And seems like the valid encoding for `.gitignore` file would be `UTF-8`. Let's use VSCode itself to save it in `UTF-8` instead and try it again:

```
$ git check-ignore --verbose node_modules
.gitignore:1:node_modules       node_modules
```

That output tells us the rule in the first line, namely `node_modules` (no asterisks or slashes) ignores the path `node_modules`.
Issue solved!
