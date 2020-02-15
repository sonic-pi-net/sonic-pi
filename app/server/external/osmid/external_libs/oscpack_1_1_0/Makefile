# oscpack makefile

# the source code should auto-detect endianess for most systems
# (see osc/OscHostEndianness.h)
# otherwise you need to explicitly set ENDIANESS below
# to either OSC_HOST_BIG_ENDIAN or OSC_HOST_LITTLE_ENDIAN
# Apple Mac OS X (PowerPC): OSC_HOST_BIG_ENDIAN
# Apple Mac OS X (Intel): OSC_HOST_LITTLE_ENDIAN
# Win32: OSC_HOST_LITTLE_ENDIAN
# i386 GNU/Linux: OSC_HOST_LITTLE_ENDIAN

ENDIANESS=OSC_DETECT_ENDIANESS #source code will detect using preprocessor
#ENDIANESS=OSC_HOST_LITTLE_ENDIAN

UNAME := $(shell uname)

CXX := g++
INCLUDES := -I.
COPTS  := -Wall -Wextra -O3
CDEBUG := -Wall -Wextra -g 
CXXFLAGS := $(COPTS) $(INCLUDES) -D$(ENDIANESS)

BINDIR := bin
PREFIX := /usr/local
INSTALL := install -c

#Name definitions
UNITTESTS := $(BINDIR)/OscUnitTests
SENDTESTS := $(BINDIR)/OscSendTests
RECEIVETEST := $(BINDIR)/OscReceiveTest
SIMPLESEND := $(BINDIR)/SimpleSend
SIMPLERECEIVE := $(BINDIR)/SimpleReceive
DUMP := $(BINDIR)/OscDump

INCLUDEDIR := oscpack
LIBNAME := liboscpack
LIBSONAME := $(LIBNAME).so
LIBFILENAME := $(LIBSONAME).1.1.0

# Common source groups

RECEIVESOURCES := osc/OscReceivedElements.cpp osc/OscPrintReceivedElements.cpp
SENDSOURCES := osc/OscOutboundPacketStream.cpp
NETSOURCES := ip/posix/UdpSocket.cpp ip/IpEndpointName.cpp ip/posix/NetworkingUtils.cpp
COMMONSOURCES := osc/OscTypes.cpp

RECEIVEOBJECTS := $(RECEIVESOURCES:.cpp=.o)
SENDOBJECTS := $(SENDSOURCES:.cpp=.o)
NETOBJECTS := $(NETSOURCES:.cpp=.o)
COMMONOBJECTS := $(COMMONSOURCES:.cpp=.o)

# Test source

UNITTESTSOURCES := tests/OscUnitTests.cpp
UNITTESTOBJECTS := $(UNITTESTSOURCES:.cpp=.o)

SENDTESTSSOURCES := tests/OscSendTests.cpp
SENDTESTSOBJECTS := $(SENDTESTSSOURCES:.cpp=.o)

RECEIVETESTSOURCES := tests/OscReceiveTest.cpp
RECEIVETESTOBJECTS := $(RECEIVETESTSOURCES:.cpp=.o)

# Example source

SIMPLESENDSOURCES := examples/SimpleSend.cpp
SIMPLESENDOBJECTS := $(SIMPLESENDSOURCES:.cpp=.o)

SIMPLERECEIVESOURCES := examples/SimpleReceive.cpp
SIMPLERECEIVEOBJECTS := $(SIMPLERECEIVESOURCES:.cpp=.o)

DUMPSOURCES := examples/OscDump.cpp
DUMPOBJECTS := $(DUMPSOURCES:.cpp=.o)

#Library objects

LIBOBJECTS := $(COMMONOBJECTS) $(SENDOBJECTS) $(RECEIVEOBJECTS) $(NETOBJECTS)

.PHONY: all unittests sendtests receivetest simplesend simplereceive dump library clean install install-local

all: unittests sendtests receivetest simplesend simplereceive dump

unittests : $(UNITTESTS)
sendtests: $(SENDTESTS)
receivetest : $(RECEIVETEST)
simplesend : $(SIMPLESEND)
simplereceive : $(SIMPLERECEIVE)
dump : $(DUMP)

# Build rule and common dependencies for all programs
# | specifies an order-only dependency so changes to bin dir modified date don't trigger recompile
$(UNITTESTS) $(SENDTESTS) $(RECEIVETEST) $(SIMPLESEND) $(SIMPLERECEIVE) $(DUMP) : $(COMMONOBJECTS) | $(BINDIR)
	$(CXX) -o $@ $^

# Additional dependencies for each program (make accumulates dependencies from multiple declarations)
$(UNITTESTS) : $(UNITTESTOBJECTS) $(SENDOBJECTS) $(RECEIVEOBJECTS)
$(SENDTESTS) : $(SENDTESTSOBJECTS) $(SENDOBJECTS) $(NETOBJECTS)
$(RECEIVETEST) : $(RECEIVETESTOBJECTS) $(RECEIVEOBJECTS) $(NETOBJECTS)
$(SIMPLESEND) : $(SIMPLESENDOBJECTS) $(SENDOBJECTS) $(NETOBJECTS)
$(SIMPLERECEIVE) : $(SIMPLERECEIVEOBJECTS) $(RECEIVEOBJECTS) $(NETOBJECTS)
$(DUMP) : $(DUMPOBJECTS) $(RECEIVEOBJECTS) $(NETOBJECTS)

$(BINDIR):
	mkdir $@

clean:
	rm -rf $(BINDIR) $(UNITTESTOBJECTS) $(SENDTESTSOBJECTS) $(RECEIVETESTOBJECTS) $(DUMPOBJECTS) $(LIBOBJECTS) $(SIMPLESENDOBJECTS) $(SIMPLERECEIVEOBJECTS) $(LIBFILENAME) include lib oscpack &> /dev/null

$(LIBFILENAME): $(LIBOBJECTS)
ifeq ($(UNAME), Darwin)
	#Mac OS X case
	$(CXX) -dynamiclib -Wl,-install_name,$(LIBSONAME) -o $(LIBFILENAME) $(LIBOBJECTS) -lc
else
	#GNU/Linux case
	$(CXX) -shared -Wl,-soname,$(LIBSONAME) -o $(LIBFILENAME) $(LIBOBJECTS) -lc
endif

lib: $(LIBFILENAME)

#Installs the library on a system global location
install: $(LIBFILENAME)
	@$(INSTALL) -m 755 $(LIBFILENAME) $(PREFIX)/lib/$(LIBFILENAME)
	@ln -s -f $(PREFIX)/lib/$(LIBFILENAME) $(PREFIX)/lib/$(LIBSONAME) 
	@mkdir  -p $(PREFIX)/include/oscpack/ip $(PREFIX)/include/oscpack/osc
	@$(INSTALL) -m 644 ip/*.h $(PREFIX)/include/oscpack/ip
	@$(INSTALL) -m 644 osc/*.h $(PREFIX)/include/oscpack/osc
	@echo "SUCCESS! oscpack has been installed in $(PREFIX)/lib and $(PREFIX)/include/ospack/"
ifneq ($(UNAME), Darwin)
	@echo "now doing ldconfig..."
	@ldconfig
endif

#Installs the include/lib structure locally
install-local: $(LIBFILENAME)
	@echo ""
	@echo " Installing in local directory <$(INCLUDEDIR)>"
	@echo "   > Creating symbolic link"
	@ln -s $(LIBFILENAME) $(LIBSONAME)
	@echo "   > Creating directories"
	@mkdir -p oscpack/lib
	@mkdir -p oscpack/include/ip
	@mkdir -p oscpack/include/osc
	@echo "   > Copying files"
	@mv $(LIBFILENAME) $(LIBSONAME) oscpack/lib
	@cp ip/*.h oscpack/include/ip
	@cp osc/*.h oscpack/include/osc
	@echo ""
	@echo "   > Success!"

