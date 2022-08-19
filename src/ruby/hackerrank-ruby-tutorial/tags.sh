#!/usr/bin/env bash

#
# Finds all “tags” in any files in the current directory and
# subdirectories.
#
# Tag lines must be in the format:
#
#   tags: [foo, bar, foo-bar, foo_bar, foo123]
#
# Any other characters can appear before `tags: `, like comment
# markers for programming languages.
#
# A tag line must be on a single line.
#

grep \
	--no-filename \
	--recursive \
	'tags: \[' \
	| sed 's/^.\+ tags: \[\([0-9A-Za-z, _-]\+\)\]/\1/' \
	| sed 's/, /\n/g' \
	| sort \
	| uniq

#
# vim: set textwidth=72:
#
