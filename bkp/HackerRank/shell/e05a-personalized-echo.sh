#!/bin/bash
#
# A Personalized Echo
# ===================
#
# https://www.hackerrank.com/challenges/bash-tutorials---a-personalized-echo
#

read -r -p 'What is your name‽ ' name
printf '%s %s\n' Welcome "$name"

