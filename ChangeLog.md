## 1.1.0 / 2024.09.20

* `--split-markers` option to help users split large conflicts to smaller parts
* `--lines-added-around` option to auto-resolve conflicts of line added from different sides
* Command-line options are are also parsed from the `GIT_MEDIATE_OPTIONS` environment variables
* Can disable auto-resolution with the `--no-trivial`, `--no-reduce`, and `--no-line-endings` flags
* Improved `--editor` support for VS Code and Xcode
* Fixed handling of filenames containing spaces and special characters

## 1.0.9 / 2023.07.25

* Do not warn when git is set to use `zdiff3` conflict style
* Resolve line ending conventions changes (i.e changes from Unix/Windows line endings)
* Preserve file modes when resolving conflicts (i.e executable scripts remain executable)
* Handle changes in `git status` formatting for files in spaces in their names
* `--context` option for `-d` mode to control size of context shown around diff
* Fixed parsing of conflicts with nested recursive conflicts
* `--editor` goes to the location of the first conflict in the file

## 1.0.8.1 / 2020.10.13

* First release on Debian (entered Debian unstable at 2023.07.20)
* Build maintenance (anti-bitrot)

## 1.0.6 / 2020.01.07

* `--merge-file` option to merge specific file, even if file is not marked as conflicted
* Reduce add/add conflicts with matching prefix/suffix lines
* Add support for `--untabify`

## 1.0.5 / 2018.07.24

* Windows compatibility fixes

## 1.0.1 / 2018.07.24

* Conflict headers for `-d` option (i.e "### Conflict 3 of 7")
* Improved error message when not running inside a git repository

## 1.0 / 2016.12.20

* Renamed to `git-mediate`
* First release on Hackage
* Add `--version` flag

## 0.3.2.4 / 2016.12.20

* Reduce conflicts when first or last lines match in all parts

## 0.3.2.1 / 2015.12.25

* `--diff2` option to dump diffs in `diff2` format
* Fix bug in modify/delete conflicts when not running from the repo's root

## 0.3.1.2 / 2015.09.20

* Better error reporting for conflict parsing errors

## 0.3.1.1 / 2015.09.01

* Support modify/delete conflicts

## 0.3.0.3 / 2015.08.16

* `-d` option also prints the diffs markers
* Support add/add conflicts
* Fixed bug with submodule conflicts

## 0.3.0.1 / 2015.06.06

* Detect git using `diff2` conflict style and add `-s` option to switch to `diff3` style
* Fixed bug with filenames containing spaces

## 0.2.0.2 / 2015.02.03

* Support for terminals without color

## 0.2.0.1 / 2015.01.12

* Don't keep `.bk` backups files for content before resolution

## 0.2 / 2014.12.18

* `-d` option for displaying the remaining conflicts

## 0.1.0.1 / 2014.12.10

* Conflict also resolves if both sides match (regardless of base also matching)

## Development started / 2014.12.1

* Tool developed started originally as "resolve-trivial-conflicts"
