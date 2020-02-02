## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = foreign

check_PROGRAMS = test

check: $(check_PROGRAMS)
	./test$(EXEEXT)

AM_CPPFLAGS = -I$(top_srcdir)/include @OGG_CFLAGS@

test_SOURCES = util.c util.h write_read.c write_read.h test.c
test_LDADD = ../lib/libvorbisenc.la ../lib/libvorbis.la @OGG_LIBS@ @VORBIS_LIBS@

debug:
	$(MAKE) check CFLAGS="@DEBUG@"

profile:
	$(MAKE) check CFLAGS="@PROFILE@"
