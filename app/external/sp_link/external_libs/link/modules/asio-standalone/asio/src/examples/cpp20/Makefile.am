AUTOMAKE_OPTIONS = subdir-objects

if SEPARATE_COMPILATION
noinst_LIBRARIES = libasio.a
libasio_a_SOURCES = ../../asio.cpp
if HAVE_OPENSSL
libasio_a_SOURCES += ../../asio_ssl.cpp
endif
LDADD = libasio.a
endif

noinst_PROGRAMS = \
	operations/c_callback_wrapper \
	operations/callback_wrapper \
	operations/composed_1 \
	operations/composed_1 \
	operations/composed_2 \
	operations/composed_3 \
	operations/composed_4 \
	operations/composed_5 \
	operations/composed_6 \
	operations/composed_7 \
	operations/composed_8

if HAVE_COROUTINES
noinst_PROGRAMS += \
	channels/throttling_proxy \
	coroutines/chat_server \
	coroutines/echo_server \
	coroutines/echo_server_with_default \
	coroutines/echo_server_with_deferred \
	coroutines/echo_server_with_deferred_default \
	coroutines/echo_server_with_as_single_default \
	coroutines/echo_server_with_as_tuple_default \
	coroutines/refactored_echo_server \
	coroutines/timeout \
	type_erasure/type_erasure
endif

AM_CXXFLAGS = -I$(srcdir)/../../../include

operations_c_callback_wrapper_SOURCES = operations/c_callback_wrapper.cpp
operations_callback_wrapper_SOURCES = operations/callback_wrapper.cpp
operations_composed_1_SOURCES = operations/composed_1.cpp
operations_composed_2_SOURCES = operations/composed_2.cpp
operations_composed_3_SOURCES = operations/composed_3.cpp
operations_composed_4_SOURCES = operations/composed_4.cpp
operations_composed_5_SOURCES = operations/composed_5.cpp
operations_composed_6_SOURCES = operations/composed_6.cpp
operations_composed_7_SOURCES = operations/composed_7.cpp
operations_composed_8_SOURCES = operations/composed_8.cpp

if HAVE_COROUTINES
channels_throttling_proxy_SOURCES = channels/throttling_proxy.cpp
coroutines_chat_server_SOURCES = coroutines/chat_server.cpp
coroutines_echo_server_SOURCES = coroutines/echo_server.cpp
coroutines_echo_server_with_default_SOURCES = coroutines/echo_server_with_default.cpp
coroutines_echo_server_with_deferred_SOURCES = coroutines/echo_server_with_deferred.cpp
coroutines_echo_server_with_deferred_default_SOURCES = coroutines/echo_server_with_deferred_default.cpp
coroutines_echo_server_with_as_single_default_SOURCES = coroutines/echo_server_with_as_single_default.cpp
coroutines_echo_server_with_as_tuple_default_SOURCES = coroutines/echo_server_with_as_tuple_default.cpp
coroutines_refactored_echo_server_SOURCES = coroutines/refactored_echo_server.cpp
coroutines_timeout_SOURCES = coroutines/timeout.cpp
type_erasure_type_erasure_SOURCES = type_erasure/main.cpp type_erasure/stdin_line_reader.cpp type_erasure/sleep.cpp
endif

EXTRA_DIST = \
	type_erasure/line_reader.hpp \
	type_erasure/stdin_line_reader.hpp \
	type_erasure/sleep.hpp

MAINTAINERCLEANFILES = \
	$(srcdir)/Makefile.in
