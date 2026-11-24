# git-mediate [![Hackage version](https://img.shields.io/hackage/v/git-mediate.svg?label=Hackage)](https://hackage.haskell.org/package/git-mediate) [![homebrew](https://img.shields.io/homebrew/v/git-mediate.svg)](https://formulae.brew.sh/formula/git-mediate)

## Introduction

Handling conflicts is difficult!

One useful way to handle them, is to use git's {z,}diff3 conflict style:

```shell
git config --global merge.conflictstyle zdiff3
```

You can also set this with `git-mediate -s`.

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

Use `git mediate --reset-index` after a conflicted `git stash pop` to
leave resolved changes unstaged. For each fully resolved path, this resets
the index entry to `HEAD`, discarding any staged changes for that path
and preserving the resolved working-tree contents.

### Simpler case

You might just resolve the conflicts manually and remove the merge markers from all of the conflicts.

In such a case, just run git-mediate, and it will "git add" the
file for you.

## Installation

### Using package managers

* macOS: `brew install git-mediate`
* Linux (debian): `apt-get install git-mediate`
* Linux (arch): `yay -S git-mediate` (see https://aur.archlinux.org/packages/git-mediate)

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

### Merge Markdown bullet additions

Run `git-mediate --markdown-bullets` when two branches insert entries at the
same place in a Markdown list. It places the incoming block first and keeps
each block in its original order. During a rebase this puts the replayed
feature commit’s entries above the upstream entries. In an ordinary merge
the branch being merged in comes first.

The option applies to `.md` and `.markdown` files. It recognizes flush-left
lists using the same bullet marker (`-`, `*`, or `+`) and keeps indented
continuations, nested lists, and blank lines with their entries. Competing
edits to an existing item still need review.

### Run automatically as a Git merge driver

With `git-mediate` on your `PATH`, configure the driver in each clone where
you want it enabled.

```sh
git config merge.mediate.driver 'git-mediate --merge-driver %O %A %B %L %P'
git config merge.mediate.recursive text
```

Add `--global` to configure the driver across repositories. For Markdown
bullets, set `GIT_MEDIATE_OPTIONS=--markdown-bullets` in the project environment.

Select paths in `.gitattributes` for the project or `.git/info/attributes`
for your local clone. For example:

```gitattributes
ChangeLog.md merge=mediate
```

The driver runs Git's three-way text merge with `zdiff3` and applies
git-mediate's enabled rules to the remaining conflicts. Git stages a clean
result and can continue a rebase automatically. Any remaining conflicts
are left for review. The driver uses its own conflict style, so this setup
works independently of `merge.conflictstyle`.

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
