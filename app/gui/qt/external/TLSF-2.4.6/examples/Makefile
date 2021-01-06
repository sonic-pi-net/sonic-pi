all: test test1 test2 test3 test4

TLSF_PATH=../src

WARNINGS = -Wextra -Wall -Wwrite-strings -Wstrict-prototypes \
           -Wmissing-prototypes -Wno-long-long -Wstrict-aliasing=2
CFLAGS+=-g -O2 -I$(TLSF_PATH) $(WARNINGS)
# CFLAGS+=-ftest-coverage -fprofile-arcs
CC = gcc

test: test.o $(TLSF_PATH)/tlsf.h
	$(CC) $(CFLAGS) -o test test.o $(TLSF_PATH)/tlsf.o

test.o: test.c $(TLSF_PATH)/tlsf.h

test1: test1.o $(TLSF_PATH)/tlsf.h
	$(CC) $(CFLAGS) -o test1 test1.o $(TLSF_PATH)/tlsf.o -lm

test1.o: test1.c $(TLSF_PATH)/tlsf.h

test2: test2.o $(TLSF_PATH)/tlsf.h
	$(CC) $(CFLAGS) -o test2 test2.o $(TLSF_PATH)/tlsf.o -lm

test2.o: test2.c $(TLSF_PATH)/tlsf.h

test3: test3.o $(TLSF_PATH)/tlsf.h
	$(CC) $(CFLAGS) -o test3 test3.o $(TLSF_PATH)/tlsf.o -lm

test3.o: test3.c $(TLSF_PATH)/tlsf.h

test4: test4.o $(TLSF_PATH)/tlsf.h
	$(CC) $(CFLAGS) -o test4 test4.o $(TLSF_PATH)/tlsf.o -lm

test4.o: test4.c $(TLSF_PATH)/tlsf.h

clean:
	$(RM) -rf *.o test test?  *~ *.c.gcov *.gcda *.gcno
