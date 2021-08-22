#!/usr/bin/env bash

##
# Sets all Kitty windows to the given title. Defaults “λ KITTY λ” if not passed.
#
# Note that Kitty has the concept of windows and OS windows. Check:
#
# • https://sw.kovidgoyal.net/kitty/overview/#tabs-and-windows
#
# DEPENDENCIES
#
# Make sure you have these programs installed:
#
# • kitty (duh 🤣)
# • jq
# • coreutils (tr, sed, etc).
##

win_title="${1:-λ KITTY λ}"

IFS=' ' read -ra win_ids <<<$(
	kitty @ ls \
		| jq '.[].tabs[] | .windows[] | .id' \
		| tr '\n' ' '
)

for id in "${win_ids[@]}" ; do
	kitty @ set-window-title -m id:"$id" "λ $win_title λ"
done

#
# vim: set tw=72 noet ts=2:
#
