CC=gcc
CFLAGS=-std=c99 -Wall -pedantic -g

all: \
	outdir \
	./outdir/main \
	./outdir/card_count_while \
	./outdir/cards_count_if \
	./outdir/cards_count_switch \
	./outdir/larger \
	./outdir/num_decks

outdir:
	mkdir -pv ./outdir

./outdir/%: ./%.c
	$(CC) $(CFLAGS) $< -o $@

