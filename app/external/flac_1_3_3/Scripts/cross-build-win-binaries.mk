#!/usr/bin/make -f

#  Copyright (C) 2014-2016  Xiph.Org Foundation
#
#  This file is part the FLAC project.  FLAC is comprised of several
#  components distributed under different licenses.  The codec libraries
#  are distributed under Xiph.Org's BSD-like license (see the file
#  COPYING.Xiph in this distribution).  All other programs, libraries, and
#  plugins are distributed under the GPL (see COPYING.GPL).  The documentation
#  is distributed under the Gnu FDL (see COPYING.FDL).  Each file in the
#  FLAC distribution contains at the top the terms under which it may be
#  distributed.
#
#  Since this particular file is relevant to all components of FLAC,
#  it may be distributed under the Xiph.Org license, which is the least
#  restrictive of those mentioned above.  See the file COPYING.Xiph in this
#  distribution.

ogg_version = 1.3.2
ogg_sha256sum = e19ee34711d7af328cb26287f4137e70630e7261b17cbe3cd41011d73a654692

flac_version = $(shell grep ^AC_INIT configure.ac | sed 's/[^ ]* \[//;s/\].*//')

win_build = $(shell pwd)/win-build

win32_name = i686-w64-mingw32
win64_name = x86_64-w64-mingw32

win32_target = --host=$(win32_name) --target=$(win32_name)
win64_target = --host=$(win64_name) --target=$(win64_name)

flac-$(flac_version)-win.zip : flac-$(flac_version)-win//AUTHORS
	zip -r $@ flac-$(flac_version)-win
	rm -rf flac-$(flac_version)-win



flac-$(flac_version)-win//AUTHORS : win-build/.stamp-flac-win32-install win-build/.stamp-flac-win64-install
	mkdir -p flac-$(flac_version)-win/win32  flac-$(flac_version)-win/win64
	cp $(win_build)/flac32/bin/flac.exe flac-$(flac_version)-win/win32/
	cp $(win_build)/flac32/bin/metaflac.exe flac-$(flac_version)-win/win32/
	$(win32_name)-strip flac-$(flac_version)-win/win32/*.exe
	cp $(win_build)/flac64/bin/flac.exe flac-$(flac_version)-win/win64/
	cp $(win_build)/flac64/bin/metaflac.exe flac-$(flac_version)-win/win64/
	$(win64_name)-strip flac-$(flac_version)-win/win64/*.exe
	cp -r doc/html flac-$(flac_version)-win/
	rm -rf flac-$(flac_version)-win/html/api
	find flac-$(flac_version)-win/ -name Makefile\* -exec rm -f {} \;
	cp AUTHORS COPYING.* README flac-$(flac_version)-win/
	touch $@

#-------------------------------------------------------------------------------
# Build and install 32 and 64 bit versions of a statically linked flac and
# metaflac executable.

win-build/.stamp-flac-win64-install : win-build/.stamp-flac-win64-config
	make clean all install
	touch $@

win-build/.stamp-flac-win64-config : win-build/.stamp-flac-prepare configure
	mkdir -p $(win_build)/ogg64
	./configure --disable-shared  $(win64_target) --with-ogg=$(win_build)/ogg64 --prefix=$(win_build)/flac64
	touch $@

win-build/.stamp-flac-win32-install : win-build/.stamp-flac-win32-config
	make clean all install
	touch $@

win-build/.stamp-flac-win32-config : win-build/.stamp-flac-prepare configure
	mkdir -p $(win_build)/ogg32
	./configure --disable-shared  $(win32_target) --with-ogg=$(win_build)/ogg32 --prefix=$(win_build)/flac32
	touch $@

win-build/.stamp-flac-prepare : win-build/.stamp-win32-install win-build/.stamp-win64-install
	touch $@

#-------------------------------------------------------------------------------
# Build libogg for win32 and win64.

win-build/.stamp-win64-install : win-build/.stamp-win64-configure
	(cd win-build/libogg-$(ogg_version) && make clean all check install)
	touch $@

win-build/.stamp-win64-configure : win-build/.stamp-source
	mkdir -p $(win_build)/win64
	(cd win-build/libogg-$(ogg_version) && ./configure --prefix=$(win_build)/ogg64 $(win32_target) --disable-shared)
	touch $@

win-build/.stamp-win32-install : win-build/.stamp-win32-configure
	(cd win-build/libogg-$(ogg_version) && make clean all check install)
	touch $@

win-build/.stamp-win32-configure : win-build/.stamp-source
	mkdir -p $(win_build)/win32
	(cd win-build/libogg-$(ogg_version) && ./configure --prefix=$(win_build)/ogg32 $(win32_target) --disable-shared)
	touch $@

win-build/.stamp-source : win-build/.stamp-sha256sum-checked
	(cd win-build && tar xf libogg-$(ogg_version).tar.gz)
	touch $@

#-------------------------------------------------------------------------------
# Retrieve and check libogg tarball.

win-build/.stamp-sha256sum-checked : win-build/libogg-$(ogg_version).tar.gz
	@if test $$(sha256sum $+ | sed 's/ .*//') != $(ogg_sha256sum) ; then exit 1 ; fi
	@echo "sha256 sum : ok"
	touch $@

win-build/libogg-$(ogg_version).tar.gz :
	mkdir -p win-build
	wget http://downloads.xiph.org/releases/ogg/$$(basename $@) -O $@

#-------------------------------------------------------------------------------
# Autotool stuff.

configure : configure.ac autogen.sh
	./autogen.sh

clean :
	rm -rf $(win_build) flac-$(flac_version)-win
