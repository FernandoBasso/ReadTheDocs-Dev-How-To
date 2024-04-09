# Bash Completion

## coreutils on macos

TAGS: @bash @coreutils @findutis @gnu @grep @find @sed

- [Using GNU command line tools in macOS instead of FreeBSD tools · GitHub](https://gist.github.com/skyzyx/3438280b18e4f7c490db8a2a2ca0b9da)

- [GitHub - fabiomaia/linuxify: 🍏🐧 Transparently transform the macOS CLI into a fresh GNU/Linux CLI experience.](https://github.com/fabiomaia/linuxify)

- [terminal - How to replace Mac OS X utilities with GNU core utilities? - Ask Different](https://apple.stackexchange.com/questions/69223/how-to-replace-mac-os-x-utilities-with-gnu-core-utilities)

```
##############################################################################
# GNU Coreutils on MACOS
# ======================
#
# NOTE: Add this to `~/.bash_profile`.
#
# On the shell, run this:
#
#   $ find /usr/local/opt -type d -follow -name gnubin -print
#
# Then paste the output lines on this array.
#
# REFS:
# • https://gist.github.com/skyzyx/3438280b18e4f7c490db8a2a2ca0b9da
# • https://github.com/fabiomaia/linuxify
# • https://apple.stackexchange.com/questions/69223/how-to-replace-mac-os-x-utilities-with-gnu-core-utilities
#
gnubins=(
  '/usr/local/opt/coreutils/libexec/gnubin'
  '/usr/local/opt/gnu-indent/libexec/gnubin'
  '/usr/local/opt/gnu-tar/libexec/gnubin'
  '/usr/local/opt/grep/libexec/gnubin'
  '/usr/local/opt/gnu-sed/libexec/gnubin'
  '/usr/local/opt/gsed/libexec/gnubin'
  '/usr/local/opt/gawk/libexec/gnubin'
  '/usr/local/opt/findutils/libexec/gnubin'
)

for gnubin in "${gnubins[@]}"; do
  PATH="$gnubin:$PATH"
done

export PATH
##############################################################################
```

Completion
