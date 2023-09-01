#!/usr/bin/make -f
# -*- makefile -*-
#
# This makefile contains simple rules to prepare, compile, test, and install
# aubio. Try one of the following rules:
#
# $ make configure
# $ make build
# $ make install
# $ make test_python

WAFCMD=python waf

#WAFOPTS:=
# turn on verbose mode
WAFOPTS += --verbose
# build wafopts
WAFOPTS += --destdir $(DESTDIR)
# multiple jobs
WAFOPTS += --jobs 4
# if HAVE_AUBIO_DOUBLE is defined, pass --enable-double to waf
# python/lib/moresetuptools.py also checks for HAVE_AUBIO_DOUBLE
WAFOPTS += $(shell [ -z $(HAVE_AUBIO_DOUBLE) ] || echo --enable-double )

PIPOPTS += --verbose

DESTDIR:=$(PWD)/build/dist
PYDESTDIR:=$(PWD)/build/pydist

# default install locations
PREFIX?=/usr/local
EXEC_PREFIX?=$(PREFIX)
LIBDIR?=$(PREFIX)/lib
INCLUDEDIR?=$(PREFIX)/include
DATAROOTDIR?=$(PREFIX)/share
MANDIR?=$(DATAROOTDIR)/man

# default python test command
PYTEST?=pytest --verbose

SOX=sox

TESTSOUNDS := python/tests/sounds

LCOVOPTS += --rc lcov_branch_coverage=1

all: build

checkwaf:
	@[ -f waf ] || make getwaf

getwaf:
	./scripts/get_waf.sh

expandwaf: getwaf
	[ -d wafilb ] || rm -fr waflib
	$(WAFCMD) --help > /dev/null
	mv .waf*/waflib . && rm -fr .waf*
	sed '/^#==>$$/,$$d' waf > waf2 && mv waf2 waf
	chmod +x waf && chmod -R go-w waflib

cleanwaf:
	rm -rf waf waflib .waf*

configure: checkwaf
	$(WAFCMD) configure $(WAFOPTS)

build: configure
	$(WAFCMD) build $(WAFOPTS)

install:
	# install
	$(WAFCMD) install $(WAFOPTS)

list_installed:
	find $(DESTDIR) -ls | sed 's|$(DESTDIR)|/«destdir»|'

list_installed_python:
	pip show -f aubio

list_all_installed: list_installed list_installed_python

uninstall:
	# uninstall
	$(WAFCMD) uninstall $(WAFOPTS)

delete_install:
	rm -rf $(PWD)/dist/test

build_python:
	# build python-aubio, using locally built libaubio if found
	python ./setup.py build

build_python_extlib:
	# build python-aubio using (locally) installed libaubio
	[ -f $(DESTDIR)/$(INCLUDEDIR)/aubio/aubio.h ]
	[ -d $(DESTDIR)/$(LIBDIR) ]
	[ -f $(DESTDIR)/$(LIBDIR)/pkgconfig/aubio.pc ]
	PKG_CONFIG_PATH=$(DESTDIR)/$(LIBDIR)/pkgconfig \
	CFLAGS="-I$(DESTDIR)/$(INCLUDEDIR)" \
	LDFLAGS="-L$(DESTDIR)/$(LIBDIR)" \
		make build_python

deps_python:
	# install or upgrade python requirements
	pip install $(PIPOPTS) --requirement requirements.txt

# use pip or distutils?
install_python: install_python_with_pip
uninstall_python: uninstall_python_with_pip
#install_python: install_python_with_distutils
#uninstall_python: uninstall_python_with_distutils

install_python_with_pip:
	# install package
	pip install $(PIPOPTS) .

uninstall_python_with_pip:
	# uninstall package
	( pip show aubio | grep -l aubio > /dev/null ) && \
	pip uninstall -y -v aubio || echo "info: aubio package is not installed"

install_python_with_distutils:
	./setup.py install $(PIPOPTS) $(DISTUTILSOPTS)

uninstall_python_with_distutils:
	#./setup.py uninstall
	[ -d $(PYDESTDIR)/$(LIBDIR) ] && echo Warning: did not clean $(PYDESTDIR)/$(LIBDIR) || true

force_uninstall_python:
	# ignore failure if not installed
	-make uninstall_python

local_dylib:
	# DYLD_LIBRARY_PATH is no more on mac os
	# create links from ~/lib/lib* to build/src/lib*
	[ -f $(PWD)/build/src/libaubio.[0-9].dylib ] && ( mkdir -p ~/lib && cp -prv build/src/libaubio.[0-9].dylib ~/lib ) || true

test_python: export LD_LIBRARY_PATH=$(DESTDIR)/$(LIBDIR)
test_python: export PYTHONPATH=$(PYDESTDIR)/$(LIBDIR)
test_python: local_dylib
	# run test with installed package
	$(PYTEST)

clean_python:
	./setup.py clean

check_clean_python:
	# check cleaning a second time works
	make clean_python
	make clean_python

clean: checkwaf
	# optionnaly clean before build
	-$(WAFCMD) clean
	# remove possible left overs
	-rm -rf doc/_build

check_clean:
	# check cleaning after build works
	$(WAFCMD) clean
	# check cleaning a second time works
	$(WAFCMD) clean

distclean:
	$(WAFCMD) distclean
	-rm -rf doc/_build/
	-rm -rf doc/web/

check_distclean:
	make distclean

distcheck: checkwaf
	$(WAFCMD) distcheck $(WAFOPTS)

help:
	$(WAFCMD) --help

create_test_sounds:
	-[ -z `which $(SOX)` ] && ( echo $(SOX) could not be found) || true
	-mkdir -p $(TESTSOUNDS)
	-$(SOX) -r 44100 -b 16 -n "$(TESTSOUNDS)/44100Hz_1f_silence.wav"      trim 0 1s
	-$(SOX) -r 22050 -b 16 -n "$(TESTSOUNDS)/22050Hz_5s_brownnoise.wav"   synth 5    brownnoise      vol 0.9
	-$(SOX) -r 32000 -b 16 -n "$(TESTSOUNDS)/32000Hz_127f_sine440.wav"    synth 127s sine 440        vol 0.9
	-$(SOX) -r  8000 -b 16 -n "$(TESTSOUNDS)/8000Hz_30s_silence.wav"      trim 0 30
	-$(SOX) -r 48000 -b 32 -n "$(TESTSOUNDS)/48000Hz_60s_sweep.wav"       synth 60   sine 100-20000  vol 0.9
	-$(SOX) -r 44100 -b 16 -n "$(TESTSOUNDS)/44100Hz_44100f_sine441.wav"  synth 44100s   sine 441 	vol 0.9
	-$(SOX) -r 44100 -b 16 -n "$(TESTSOUNDS)/44100Hz_100f_sine441.wav"    synth 100s sine 441 	vol 0.9

# build only libaubio, no python-aubio
test_lib_only: clean distclean configure build install list_installed
# additionally, clean after a fresh build
test_lib_only_clean: test_lib_only uninstall check_clean check_distclean

# build libaubio, build and test python-aubio against it
test_lib_python: force_uninstall_python deps_python \
	clean_python clean distclean \
	configure build build_python \
	install install_python \
	test_python \
	list_all_installed

test_lib_python_clean: test_lib_python \
	uninstall_python uninstall \
	check_clean_python \
	check_clean \
	check_distclean

# build libaubio, install it, build python-aubio against it
test_lib_install_python: force_uninstall_python deps_python \
	clean_python distclean \
	configure build \
	install \
	build_python_extlib \
	install_python \
	test_python \
	list_all_installed

test_lib_install_python_clean: test_lib_install_python \
	uninstall_python \
	delete_install \
	check_clean_python \
	check_distclean

# build a python-aubio that includes libaubio
test_python_only: force_uninstall_python deps_python \
	clean_python clean distclean \
	build_python \
	install_python \
	test_python \
	list_installed_python

test_python_only_clean: test_python_only \
	uninstall_python \
	check_clean_python

coverage_cycle: coverage_zero_counters coverage_report

coverage_zero_counters:
	lcov --zerocounters --directory .

coverage: export CFLAGS=--coverage
coverage: export LDFLAGS=--coverage
coverage: export PYTHONPATH=$(PWD)/python/lib
coverage: export LD_LIBRARY_PATH=$(PWD)/build/src
coverage: force_uninstall_python deps_python \
	clean_python clean distclean build local_dylib
	# capture coverage after running c tests
	lcov $(LCOVOPTS) --capture --no-external --directory . \
		--output-file build/coverage_lib.info
	# build and test python
	pip install -v -e .
	# run tests, with python coverage
	coverage run `which pytest`
	# capture coverage again
	lcov $(LCOVOPTS) --capture --no-external --directory . \
		--output-file build/coverage_python.info
	# merge both coverage info files
	lcov $(LCOVOPTS) -a build/coverage_python.info -a build/coverage_lib.info \
		--output-file build/coverage.info
	# remove tests
	lcov $(LCOVOPTS) --remove build/coverage.info '*/ooura_fft8g*' \
		--output-file build/coverage_lib.info

# make sure we don't build the doc, which builds a temporary python module
coverage_report: export WAFOPTS += --disable-docs
coverage_report: coverage
	# generate report with lcov's genhtml
	genhtml build/coverage_lib.info --output-directory build/coverage_c \
		--branch-coverage --highlight --legend
	# generate python report with coverage python package
	coverage report
	coverage html -d build/coverage_python
	# show links to generated reports
	for i in $$(ls build/coverage_*/index.html); do echo file://$(PWD)/$$i; done

sphinx: configure
	$(WAFCMD) sphinx $(WAFOPTS)

doxygen: configure
	$(WAFCMD) doxygen $(WAFOPTS)

manpages: configure
	$(WAFCMD) manpages $(WAFOPTS)

html: doxygen sphinx

docs: html manpages

dist: distclean expandwaf
	$(WAFCMD) dist
