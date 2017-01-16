# -*- makefile -*-

ifeq ($(OS),)
  BUILD_OS := $(shell uname -s | tr '[:upper:]' '[:lower:]')
  OS := $(BUILD_OS)
endif

ifeq ($(CPU),)
  CPU := $(shell uname -m | sed -e 's/i[345678]86/i386/')
endif

PLATFORM = $(CPU)-$(OS)

ifeq ($(OS), sunos)
  OS = solaris
endif

SRC_DIR = .
BUILD_DIR ?= .
TEST_BUILD_DIR = .
# Set defaults to unix (linux/solaris/bsd)
PREFIX = lib
LIBEXT ?= so
LIBNAME = $(PREFIX)test.$(LIBEXT)

export MACOSX_DEPLOYMENT_TARGET=10.4

CCACHE := $(strip $(realpath $(shell which ccache 2> /dev/null)))

TEST_SRCS = $(wildcard $(SRC_DIR)/*.c)
TEST_OBJS := $(patsubst $(SRC_DIR)/%.c, $(TEST_BUILD_DIR)/%.o, $(TEST_SRCS))

#
# Compiler/linker flags from:
#   http://weblogs.java.net/blog/kellyohair/archive/2006/01/compilation_of_1.html
JFLAGS = -fno-omit-frame-pointer -fno-strict-aliasing
OFLAGS = -O2 $(JFLAGS)
WFLAGS = -W -Wall -Wno-unused -Wno-parentheses
PICFLAGS = -fPIC
SOFLAGS = -shared
LDFLAGS += $(SOFLAGS)

IFLAGS = -I"$(BUILD_DIR)"
CFLAGS = $(OFLAGS) $(WFLAGS) $(IFLAGS) $(PICFLAGS) -D_REENTRANT

ifneq ($(strip $(findstring $(OS), win32, mingw, cygwin)),)
  # For cygwin => win32-native builds, strip out cygwin deps
  ifneq ($(findstring cygwin, $(BUILD_OS)),)
    CC += -mno-cygwin -mwin32
    LDFLAGS += -mno-cygwin -Wl,--add-stdcall-alias
  endif
  PICFLAGS=
  LIBEXT=dll
  CC = gcc
endif

ifeq ($(OS), darwin)
  ifneq ($(findstring $(CPU),ppc),)
    ARCHFLAGS += -arch ppc
  endif
  ifneq ($(findstring $(CPU),i386 x86_64),)
    ARCHFLAGS += -arch i386 -arch x86_64
  endif
  CFLAGS += $(ARCHFLAGS) -DTARGET_RT_MAC_CFM=0
  CFLAGS += -fno-common
  LDFLAGS = $(ARCHFLAGS) -dynamiclib
  # link against the universal libraries on ppc machines
  LDFLAGS += -L$(MACSDK)/usr/lib
  LIBEXT = dylib
  FFI_CFLAGS += -isysroot $(MACSDK)
  PICFLAGS =
  SOFLAGS =
endif

ifeq ($(OS), linux)
  SOFLAGS += -Wl,-soname,$(LIBNAME)
endif

ifeq ($(OS), solaris)
  CC = /usr/sfw/bin/gcc -std=c99
  LD = /usr/ccs/bin/ld
  SOFLAGS = -shared -static-libgcc
endif

ifeq ($(OS), aix)
  LIBEXT = a
  SOFLAGS = -shared -static-libgcc
  PICFLAGS += -pthread
endif

ifneq ($(findstring bsd, $(OS)),)
  SOFLAGS = -shared -static-libgcc
  CFLAGS += -pthread
  LDFLAGS += -pthread
endif

ifeq ($(CPU), i386)
  MODEL = 32
endif

ifeq ($(CPU), sparcv9)
  MODEL = 64
endif

ifeq ($(CPU), amd64)
  MODEL = 64
endif

ifeq ($(CPU), x86_64)
  MODEL = 64
endif

ifeq ($(CPU), ppc64)
  MODEL = 64
endif

ifeq ($(CPU), powerpc64)
  MODEL = 64
endif

MODELFLAG =
ifneq ($(MODEL),)
  MODELFLAG = -m$(MODEL)
endif

# On platforms (linux, solaris) that support both 32bit and 64bit, force building for one or the other
ifneq ($(or $(findstring linux, $(OS)), $(findstring solaris, $(OS))),)
  # Change the CC/LD instead of CFLAGS/LDFLAGS, incase other things in the flags
  # makes the libffi build choke
  CC += $(MODELFLAG)
  LD += $(MODELFLAG)
endif

LIBTEST = $(LIBNAME)

all:	$(LIBTEST)

$(TEST_BUILD_DIR)/%.o : $(SRC_DIR)/%.c
	@mkdir -p $(@D)
	$(CCACHE) $(CC) $(CFLAGS) -c $< -o $@

$(LIBTEST):  $(TEST_OBJS)
	$(CC) -o $@ $(LDFLAGS) $(TEST_OBJS) -lm

clean::
	# nothing to do - ant will delete the build dir

debug::
	@echo "SRCS=$(TEST_SRCS)"
