# git-mediate [![Hackage version](https://img.shields.io/hackage/v/git-mediate.svg?label=Hackage)](https://hackage.haskell.org/package/git-mediate) [![homebrew](https://img.shields.io/homebrew/v/git-mediate.svg)](https://formulae.brew.sh/formula/git-mediate)

## Introduction

Handling conflicts is difficult!

One useful way to handle them, is to use git's diff3 conflict style:

```shell
git config --global merge.conflictstyle diff3
```

And then when you get a conflict, it looks like:

    Unconflicted stuff

    <<<<<<< HEAD
    Version A changes
    |||||||
    Base version
    ======= Version B
    Version B changes
    >>>>>>>

    More unconflicted stuff here

Then you are supposed to manually merge the useful changes in the top and bottom parts, relative to the base version.

A useful way to do this is to figure out which of the changes (Version A or Version B) is a simpler change.

Perhaps one of the versions just added a small comment above the code section:

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

One easy thing to do, mechanically, is to apply the simple change to
the other 2 versions. Thus, it becomes:

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

Now, you can run this little utility: git-mediate, which will see
the conflict has become trivial (only one side changed anything) and
select that side appropriately.

When all conflicts have been resolved in a file, "git add" will be
used on it automatically.

### Simpler case

You might just resolve the conflicts manually and remove the merge markers from all of the conflicts.

In such a case, just run git-mediate, and it will "git add" the
file for you.

## Installation

### Using package managers

* macOS: `brew install git-mediate`
* Linux (debian): `apt-get install git-mediate`

### Using haskell-stack

1. Install [haskell stack](https://docs.haskellstack.org/en/stable/)
2. Run: `stack install git-mediate`

### From sources

Clone it:

    git clone https://github.com/Peaker/git-mediate
    cd git-mediate

Option #1: Build & install using stack: `stack install` (make sure you installed [haskell stack](https://docs.haskellstack.org/en/stable/))

Option #2: Build & install using cabal: `cabal install` (make sure `~/.cabal/bin` is in your `$PATH`)

## Use

Call the git-mediate from a git repository with conflicts.

## Additional features

### Open editor

You can use the `-e` flag to invoke your `$EDITOR` on every conflicted file that could not be automatically resolved.

### Show conflict diffs

Sometimes, the conflict is just a giant block of incomprehensible text next to another giant block of incomprehensible text.

You can use the `-d` flag to show the conflict in diff-from-base form. Then, you can manually apply the changes you see in both the base and wherever needed, and use git-mediate again to make sure you've updated everything appropriately.

## License

Copyright (C) 2014-2024  Eyal Lotem

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; version 2 of the License only.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
