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
	allocation/server \
	buffers/reference_counted \
	chat/chat_client \
	chat/chat_server \
	deferred/deferred_1 \
	deferred/deferred_2 \
	echo/async_tcp_echo_server \
	echo/async_udp_echo_server \
	echo/blocking_tcp_echo_client \
	echo/blocking_tcp_echo_server \
	echo/blocking_udp_echo_client \
	echo/blocking_udp_echo_server \
	executors/actor \
	executors/bank_account_1 \
	executors/bank_account_2 \
	executors/fork_join \
	executors/pipeline \
	executors/priority_scheduler \
	files/blocking_file_copy \
	files/async_file_copy \
	futures/daytime_client \
	handler_tracking/async_tcp_echo_server \
	http/server/http_server \
	invocation/prioritised_handlers \
	iostreams/http_client \
	multicast/receiver \
	multicast/sender \
	nonblocking/third_party_lib \
	operations/composed_1 \
	operations/composed_2 \
	operations/composed_3 \
	operations/composed_4 \
	operations/composed_5 \
	operations/composed_6 \
	operations/composed_7 \
	operations/composed_8 \
  parallel_group/ranged_wait_for_all \
  parallel_group/wait_for_all \
  parallel_group/wait_for_one \
  parallel_group/wait_for_one_error \
  parallel_group/wait_for_one_success \
	socks4/sync_client \
	timeouts/async_tcp_client \
	timeouts/blocking_tcp_client \
	timeouts/blocking_token_tcp_client \
	timeouts/blocking_udp_client \
	timeouts/server \
	timers/time_t_timer \
	type_erasure/type_erasure

if !WINDOWS_TARGET
noinst_PROGRAMS += \
	fork/daemon \
	fork/process_per_connection \
	local/connect_pair \
	local/iostream_client \
	local/stream_server \
	local/stream_client \
	local/fd_passing_stream_server \
	local/fd_passing_stream_client
endif

if HAVE_OPENSSL
noinst_PROGRAMS += \
	ssl/client \
	ssl/server
endif

if HAVE_BOOST_COROUTINE
noinst_PROGRAMS += \
  spawn/echo_server \
  spawn/parallel_grep
endif

noinst_HEADERS = \
	socks4/socks4.hpp \
	chat/chat_message.hpp

AM_CXXFLAGS = -I$(srcdir)/../../../include

allocation_server_SOURCES = allocation/server.cpp
buffers_reference_counted_SOURCES = buffers/reference_counted.cpp
chat_chat_client_SOURCES = chat/chat_client.cpp
chat_chat_server_SOURCES = chat/chat_server.cpp
deferred_deferred_1_SOURCES = deferred/deferred_1.cpp
deferred_deferred_2_SOURCES = deferred/deferred_2.cpp
echo_async_tcp_echo_server_SOURCES = echo/async_tcp_echo_server.cpp
echo_async_udp_echo_server_SOURCES = echo/async_udp_echo_server.cpp
echo_blocking_tcp_echo_client_SOURCES = echo/blocking_tcp_echo_client.cpp
echo_blocking_tcp_echo_server_SOURCES = echo/blocking_tcp_echo_server.cpp
echo_blocking_udp_echo_client_SOURCES = echo/blocking_udp_echo_client.cpp
echo_blocking_udp_echo_server_SOURCES = echo/blocking_udp_echo_server.cpp
executors_actor_SOURCES = executors/actor.cpp
executors_bank_account_1_SOURCES = executors/bank_account_1.cpp
executors_bank_account_2_SOURCES = executors/bank_account_2.cpp
executors_fork_join_SOURCES = executors/fork_join.cpp
executors_pipeline_SOURCES = executors/pipeline.cpp
executors_priority_scheduler_SOURCES = executors/priority_scheduler.cpp
files_blocking_file_copy_SOURCES = files/blocking_file_copy.cpp
files_async_file_copy_SOURCES = files/async_file_copy.cpp
futures_daytime_client_SOURCES = futures/daytime_client.cpp
handler_tracking_async_tcp_echo_server_SOURCES = handler_tracking/async_tcp_echo_server.cpp
http_server_http_server_SOURCES = \
	http/server/connection.cpp \
	http/server/connection_manager.cpp \
	http/server/main.cpp \
	http/server/mime_types.cpp \
	http/server/reply.cpp \
	http/server/request_handler.cpp \
	http/server/request_parser.cpp \
	http/server/server.cpp
invocation_prioritised_handlers_SOURCES = invocation/prioritised_handlers.cpp
iostreams_http_client_SOURCES = iostreams/http_client.cpp
multicast_receiver_SOURCES = multicast/receiver.cpp
multicast_sender_SOURCES = multicast/sender.cpp
nonblocking_third_party_lib_SOURCES = nonblocking/third_party_lib.cpp
operations_composed_1_SOURCES = operations/composed_1.cpp
operations_composed_2_SOURCES = operations/composed_2.cpp
operations_composed_3_SOURCES = operations/composed_3.cpp
operations_composed_4_SOURCES = operations/composed_4.cpp
operations_composed_5_SOURCES = operations/composed_5.cpp
operations_composed_6_SOURCES = operations/composed_6.cpp
operations_composed_7_SOURCES = operations/composed_7.cpp
operations_composed_8_SOURCES = operations/composed_8.cpp
parallel_group_ranged_wait_for_all_SOURCES = parallel_group/ranged_wait_for_all.cpp
parallel_group_wait_for_all_SOURCES = parallel_group/wait_for_all.cpp
parallel_group_wait_for_one_SOURCES = parallel_group/wait_for_one.cpp
parallel_group_wait_for_one_error_SOURCES = parallel_group/wait_for_one_error.cpp
parallel_group_wait_for_one_success_SOURCES = parallel_group/wait_for_one_success.cpp
socks4_sync_client_SOURCES = socks4/sync_client.cpp
timeouts_async_tcp_client_SOURCES = timeouts/async_tcp_client.cpp
timeouts_blocking_tcp_client_SOURCES = timeouts/blocking_tcp_client.cpp
timeouts_blocking_token_tcp_client_SOURCES = timeouts/blocking_token_tcp_client.cpp
timeouts_blocking_udp_client_SOURCES = timeouts/blocking_udp_client.cpp
timeouts_server_SOURCES = timeouts/server.cpp
timers_time_t_timer_SOURCES = timers/time_t_timer.cpp
type_erasure_type_erasure_SOURCES = type_erasure/main.cpp type_erasure/stdin_line_reader.cpp type_erasure/sleep.cpp

if !WINDOWS_TARGET
fork_daemon_SOURCES = fork/daemon.cpp
fork_process_per_connection_SOURCES = fork/process_per_connection.cpp
local_connect_pair_SOURCES = local/connect_pair.cpp
local_iostream_client_SOURCES = local/iostream_client.cpp
local_stream_server_SOURCES = local/stream_server.cpp
local_stream_client_SOURCES = local/stream_client.cpp
local_fd_passing_stream_server_SOURCES = local/fd_passing_stream_server.cpp
local_fd_passing_stream_client_SOURCES = local/fd_passing_stream_client.cpp
endif

if HAVE_OPENSSL
ssl_client_SOURCES = ssl/client.cpp
ssl_server_SOURCES = ssl/server.cpp
endif

if HAVE_BOOST_COROUTINE
spawn_echo_server_SOURCES = spawn/echo_server.cpp
spawn_echo_server_LDADD = $(LDADD) -lboost_coroutine -lboost_context -lboost_thread -lboost_chrono -lboost_system
spawn_parallel_grep_SOURCES = spawn/parallel_grep.cpp
spawn_parallel_grep_LDADD = $(LDADD) -lboost_coroutine -lboost_context -lboost_thread -lboost_chrono -lboost_system
endif

EXTRA_DIST = \
	handler_tracking/custom_tracking.hpp \
	http/server/connection.hpp \
	http/server/connection_manager.hpp \
	http/server/header.hpp \
	http/server/mime_types.hpp \
	http/server/reply.hpp \
	http/server/request.hpp \
	http/server/request_handler.hpp \
	http/server/request_parser.hpp \
	http/server/server.hpp \
	ssl/README \
	ssl/ca.pem \
	ssl/server.pem \
	ssl/dh4096.pem \
	type_erasure/line_reader.hpp \
	type_erasure/stdin_line_reader.hpp \
	type_erasure/sleep.hpp


MAINTAINERCLEANFILES = \
	$(srcdir)/Makefile.in
