.PHONY: all

CC = gcc
CFLAGS = -g -I../include -I../src -Wall -Wextra -Wmissing-prototypes -Wno-missing-field-initializers
LFLAGS = -L../build -lgit2 -lz
APPS = general showindex diff rev-list cat-file status log rev-parse init blame tag remote
APPS += for-each-ref
APPS += describe

all: $(APPS)

% : %.c
	$(CC) -o $@ common.c $(CFLAGS) $< $(LFLAGS)

clean:
	$(RM) $(APPS)
	$(RM) -r *.dSYM
