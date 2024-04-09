CC=gcc
CFLAGS=-std=c99 -Wall -pedantic -g

all: \
	outdir \
	./outdir/broken_heart \
	./outdir/buf_fgets \
	./outdir/cards_segfault \
	./outdir/doses \
	./outdir/fortune_cookie \
	./outdir/jimy_dead \
	./outdir/name_age_scanf \
	./outdir/south_east_nok \
	./outdir/south_east_ok

outdir:
	mkdir -pv ./outdir

./outdir/%: ./%.c
	$(CC) $(CFLAGS) $< -o $@
