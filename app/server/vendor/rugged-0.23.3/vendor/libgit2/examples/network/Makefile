default: all

CC = gcc
CFLAGS += -g
CFLAGS += -I../../include
LDFLAGS += -L../../build -L../..
LIBRARIES += -lgit2 -lpthread

OBJECTS = \
  git2.o \
  ls-remote.o \
  fetch.o \
  clone.o \
  index-pack.o \
  common.o

all: $(OBJECTS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o git2 $(OBJECTS) $(LIBRARIES)

clean:
	$(RM) $(OBJECTS)
	$(RM) git2
