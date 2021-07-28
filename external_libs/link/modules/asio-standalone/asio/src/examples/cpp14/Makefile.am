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
	echo/async_tcp_echo_server \
	echo/async_udp_echo_server \
	echo/blocking_tcp_echo_client \
	echo/blocking_tcp_echo_server \
	echo/blocking_udp_echo_client \
	echo/blocking_udp_echo_server \
	executors/actor \
	executors/async_1 \
	executors/async_2 \
	executors/bank_account_1 \
	executors/bank_account_2 \
	executors/fork_join \
	executors/pipeline \
	executors/priority_scheduler \
	iostreams/http_client \
	operations/composed_1 \
	operations/composed_2 \
	operations/composed_3 \
	operations/composed_4 \
	operations/composed_5 \
	operations/composed_6 \
	operations/composed_7 \
	operations/composed_8

AM_CXXFLAGS = -I$(srcdir)/../../../include

echo_async_tcp_echo_server_SOURCES = echo/async_tcp_echo_server.cpp
echo_async_udp_echo_server_SOURCES = echo/async_udp_echo_server.cpp
echo_blocking_tcp_echo_client_SOURCES = echo/blocking_tcp_echo_client.cpp
echo_blocking_tcp_echo_server_SOURCES = echo/blocking_tcp_echo_server.cpp
echo_blocking_udp_echo_client_SOURCES = echo/blocking_udp_echo_client.cpp
echo_blocking_udp_echo_server_SOURCES = echo/blocking_udp_echo_server.cpp
executors_actor_SOURCES = executors/actor.cpp
executors_async_1_SOURCES = executors/async_1.cpp
executors_async_2_SOURCES = executors/async_2.cpp
executors_bank_account_1_SOURCES = executors/bank_account_1.cpp
executors_bank_account_2_SOURCES = executors/bank_account_2.cpp
executors_fork_join_SOURCES = executors/fork_join.cpp
executors_pipeline_SOURCES = executors/pipeline.cpp
executors_priority_scheduler_SOURCES = executors/priority_scheduler.cpp
iostreams_http_client_SOURCES = iostreams/http_client.cpp
operations_composed_1_SOURCES = operations/composed_1.cpp
operations_composed_2_SOURCES = operations/composed_2.cpp
operations_composed_3_SOURCES = operations/composed_3.cpp
operations_composed_4_SOURCES = operations/composed_4.cpp
operations_composed_5_SOURCES = operations/composed_5.cpp
operations_composed_6_SOURCES = operations/composed_6.cpp
operations_composed_7_SOURCES = operations/composed_7.cpp
operations_composed_8_SOURCES = operations/composed_8.cpp

MAINTAINERCLEANFILES = \
	$(srcdir)/Makefile.in
