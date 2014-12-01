# Introduction

Handling conflicts is difficult!

One useful way to handle them, is to use git's diff3 conflict style:

```shell
git config --global merge.conflictstyle diff3
```

And then when you get a conflict, it looks like:

```
Unconflicted stuff

<<<<<<< HEAD
Version A changes
|||||||
Base version
======= Version B
Version B changes
>>>>>>>

More unconflicted stuff here
```

Then you are supposed to manually merge the useful changes in the top and bottom parts, relative to the base version.

A useful way to do this is to figure out which of the changes (Version A or Version B) is a simpler change.

Perhaps one of the versions just added a small comment above the code section:

```
Unconflicted stuff

<<<<<<< HEAD
Added a comment here
BASE
|||||||
BASE
======= Version B
BASE and complex changes here
>>>>>>>

More unconflicted stuff here
```

One easy thing to do, mechanically, is to apply the simple change to
the other 2 versions. Thus, it becomes:

```
Unconflicted stuff

<<<<<<< HEAD
Added a comment here
BASE
|||||||
Added a comment here
BASE
======= Version B
Added a comment here
BASE and complex changes here
>>>>>>>

More unconflicted stuff here
```

Now, you can run this little utility: resolve-trivial-conflicts, which will see
the conflict has become trivial (only one side changed anything) and
select that side appropriately.

When all conflicts have been resolved in a file, "git add" will be
used on it automatically.

## Simpler case

You might just resolve the conflicts manually and remove the merge markers from all of the conflicts.

In such a case, just run resolve-trivial-conflicts, and it will "git add" the
file for you.

# Installation

First, make sure `~/.cabal/bin` is in your `$PATH`.

```code
git clone https://github.com/ElastiLotem/resolve-trivial-conflicts
cd resolve-trivial-conflicts
cabal install
```

# Use

Call the resolve-trivial-conflicts from a git repository with conflicts
