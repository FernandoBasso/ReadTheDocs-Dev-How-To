#!/bin/bash

read -r x <&0
read -r y <&0

[[ "$x" -lt "$y" ]] && printf '%s\n' 'X is less than Y'
[[ "$x" -gt "$y" ]] && printf '%s\n' 'X is greater than Y'
[[ "$x" -eq "$y" ]] && printf '%s\n' 'X is equal to Y'

