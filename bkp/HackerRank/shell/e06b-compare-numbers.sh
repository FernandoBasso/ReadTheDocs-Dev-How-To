#!/bin/bash

read -r x <&0
read -r y <&0

if [[ "$x" < "$y" ]]
then
  printf '%s\n' 'X is less than Y'
elif [[ "$x" > "$y" ]]
then
  printf '%s\n' 'X is greater than Y'
else
  printf '%s\n' 'X is equal to Y'
fi

