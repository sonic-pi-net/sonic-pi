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
	deferred/deferred_1 \
	deferred/deferred_2 \
	deferred/deferred_3 \
	deferred/deferred_4 \
	deferred/deferred_5 \
	deferred/deferred_6 \
	deferred/deferred_7 \
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
	operations/composed_8 \
  parallel_group/parallel_sort \
  parallel_group/ranged_wait_for_all \
  parallel_group/wait_for_all \
  parallel_group/wait_for_one \
  parallel_group/wait_for_one_error \
  parallel_group/wait_for_one_success

AM_CXXFLAGS = -I$(srcdir)/../../../include

deferred_deferred_1_SOURCES = deferred/deferred_1.cpp
deferred_deferred_2_SOURCES = deferred/deferred_2.cpp
deferred_deferred_3_SOURCES = deferred/deferred_3.cpp
deferred_deferred_4_SOURCES = deferred/deferred_4.cpp
deferred_deferred_5_SOURCES = deferred/deferred_5.cpp
deferred_deferred_6_SOURCES = deferred/deferred_6.cpp
deferred_deferred_7_SOURCES = deferred/deferred_7.cpp
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
parallel_group_parallel_sort_SOURCES = parallel_group/parallel_sort.cpp
parallel_group_ranged_wait_for_all_SOURCES = parallel_group/ranged_wait_for_all.cpp
parallel_group_wait_for_all_SOURCES = parallel_group/wait_for_all.cpp
parallel_group_wait_for_one_SOURCES = parallel_group/wait_for_one.cpp
parallel_group_wait_for_one_error_SOURCES = parallel_group/wait_for_one_error.cpp
parallel_group_wait_for_one_success_SOURCES = parallel_group/wait_for_one_success.cpp

MAINTAINERCLEANFILES = \
	$(srcdir)/Makefile.in
