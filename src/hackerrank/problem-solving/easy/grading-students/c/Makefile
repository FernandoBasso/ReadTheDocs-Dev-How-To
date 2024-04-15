CC=gcc
CFLAGS=-std=c99 -Wall -pedantic -g -lm

##
# Run like:
#
#   $ make out/solution_v1
#   $ make out/solution_v2
#
# Then, execute with:
#
#   $ ./out/solution_v1
#   $ ./out/solution_v2
#
out/%: %.c
	mkdir -pv ./out
	$(CC) $(CFLAGS) $< -o $@

clear:
	rm -v ./out/*

