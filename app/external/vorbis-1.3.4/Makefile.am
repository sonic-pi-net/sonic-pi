## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = foreign 1.11 dist-zip dist-xz

ACLOCAL_AMFLAGS = -I m4

SUBDIRS = m4 include vq lib test doc

if BUILD_EXAMPLES 
SUBDIRS += examples 
endif 

m4datadir = $(datadir)/aclocal
m4data_DATA = vorbis.m4

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = vorbis.pc vorbisenc.pc vorbisfile.pc

EXTRA_DIST = \
	CHANGES COPYING \
	autogen.sh \
	libvorbis.spec libvorbis.spec.in \
	vorbis.m4 \
	vorbis.pc.in vorbisenc.pc.in vorbisfile.pc.in \
	vorbis-uninstalled.pc.in \
	vorbisenc-uninstalled.pc.in \
	vorbisfile-uninstalled.pc.in \
	symbian \
	macosx win32 CMakeLists.txt


DISTCHECK_CONFIGURE_FLAGS = --enable-docs

dist-hook:
	for item in $(EXTRA_DIST); do \
	  if test -d $$item; then \
	    echo -n "cleaning $$item dir for distribution..."; \
	    rm -rf `find $(distdir)/$$item -name .svn`; \
	    echo "OK"; \
	  fi; \
	done

debug:
	$(MAKE) all CFLAGS="@DEBUG@"

profile:
	$(MAKE) all CFLAGS="@PROFILE@"
