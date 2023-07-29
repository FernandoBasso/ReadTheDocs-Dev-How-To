#!/usr/bin/env bash

##
# Creates a kitty session with a few appropriately named tabs and
# windows.
#
# NOTE: Run this script from INSIDE kitty from the directory where this
# file is located and from a fresh kitty window without any other tabs
# or splits as this script relies on index for the tabs and windows.
#
# USAGE:
#
#   $ cd /path/to/this/dev-how-to/directory
#   $ bash ./kitty-session.sh
##

if [[ "$TERM" != xterm-kitty ]] ; then
	cat 1>&2 <<-'EOF'

!! NOTE !!

This script can only be used from Kitty terminal emulator.'

Bailing out...
	EOF

	exit 1
fi

win_title='λ DEVHOWTO λ'
work_dir="$PWD"

kitty @ set-tab-title 'DOCS nvim'

kitty @ launch \
  --type=tab \
  --cwd "$work_dir" \
  --tab-title 'DOCS server+shell' \
  --title "$win_title"

kitty @ launch \
	--type=window \
	--cwd "$work_dir" \
	--title "$win_title"

kitty @ launch \
	--type=tab \
	--cwd "$work_dir" \
	--tab-title shell \
	--title "$win_title"

##
# Focus the shell tab on the left pane and start the
# develpment server there.
#
# No \n so it doesn't actually runs the command as I don't always want
# or need to start Sphinx server/build stuff.
#
# ASSUME: We have a pipenv setup on ./localshipnx/ directory.
#
kitty @ focus-tab -m index:1
kitty @ send-text -m num:0 'source ./localsphinx/bin/activate && make develop'

#
# Focus the shell tab on the right pane and run ‘git status’
# on it.
#
kitty @ focus-tab -m index:1
kitty @ send-text -m num:1 'git status\n'

kitty @ focus-tab -m index:2
kitty @ send-text 'help :\n'

##
# Focus on the first tab, index 0, which is the nvim/editor tab now.
#
kitty @ focus-tab -m index:0
kitty @ send-text clear\\n

#
# vim: set tw=72:
#
