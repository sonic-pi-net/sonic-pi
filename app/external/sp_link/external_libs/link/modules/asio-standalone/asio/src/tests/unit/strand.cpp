//
// strand.cpp
// ~~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

// Disable autolinking for unit tests.
#if !defined(BOOST_ALL_NO_LIB)
#define BOOST_ALL_NO_LIB 1
#endif // !defined(BOOST_ALL_NO_LIB)

// Test that header file is self-contained.
#include "asio/strand.hpp"

#include <sstream>
#include "asio/executor.hpp"
#include "asio/io_context.hpp"
#include "asio/dispatch.hpp"
#include "asio/post.hpp"
#include "asio/thread.hpp"
#include "unit_test.hpp"

#if defined(ASIO_HAS_BOOST_DATE_TIME)
# include "asio/deadline_timer.hpp"
#else // defined(ASIO_HAS_BOOST_DATE_TIME)
# include "asio/steady_timer.hpp"
#endif // defined(ASIO_HAS_BOOST_DATE_TIME)

#if defined(ASIO_HAS_BOOST_BIND)
# include <boost/bind/bind.hpp>
#else // defined(ASIO_HAS_BOOST_BIND)
# include <functional>
#endif // defined(ASIO_HAS_BOOST_BIND)

using namespace asio;

#if defined(ASIO_HAS_BOOST_BIND)
namespace bindns = boost;
#else // defined(ASIO_HAS_BOOST_BIND)
namespace bindns = std;
#endif

#if defined(ASIO_HAS_BOOST_DATE_TIME)
typedef deadline_timer timer;
namespace chronons = boost::posix_time;
#elif defined(ASIO_HAS_CHRONO)
typedef steady_timer timer;
namespace chronons = asio::chrono;
#endif // defined(ASIO_HAS_BOOST_DATE_TIME)

void increment(int* count)
{
  ++(*count);
}

void increment_without_lock(strand<io_context::executor_type>* s, int* count)
{
  ASIO_CHECK(!s->running_in_this_thread());

  int original_count = *count;

  dispatch(*s, bindns::bind(increment, count));

  // No other functions are currently executing through the locking dispatcher,
  // so the previous call to dispatch should have successfully nested.
  ASIO_CHECK(*count == original_count + 1);
}

void increment_with_lock(strand<io_context::executor_type>* s, int* count)
{
  ASIO_CHECK(s->running_in_this_thread());

  int original_count = *count;

  dispatch(*s, bindns::bind(increment, count));

  // The current function already holds the strand's lock, so the
  // previous call to dispatch should have successfully nested.
  ASIO_CHECK(*count == original_count + 1);
}

void sleep_increment(io_context* ioc, int* count)
{
  timer t(*ioc, chronons::seconds(2));
  t.wait();

  ++(*count);
}

void increment_by_a(int* count, int a)
{
  (*count) += a;
}

void increment_by_a_b(int* count, int a, int b)
{
  (*count) += a + b;
}

void increment_by_a_b_c(int* count, int a, int b, int c)
{
  (*count) += a + b + c;
}

void increment_by_a_b_c_d(int* count, int a, int b, int c, int d)
{
  (*count) += a + b + c + d;
}

void start_sleep_increments(io_context* ioc,
    strand<io_context::executor_type>* s, int* count)
{
  // Give all threads a chance to start.
  timer t(*ioc, chronons::seconds(2));
  t.wait();

  // Start three increments.
  post(*s, bindns::bind(sleep_increment, ioc, count));
  post(*s, bindns::bind(sleep_increment, ioc, count));
  post(*s, bindns::bind(sleep_increment, ioc, count));
}

void throw_exception()
{
  throw 1;
}

void io_context_run(io_context* ioc)
{
  ioc->run();
}

void strand_test()
{
  io_context ioc;
  strand<io_context::executor_type> s = make_strand(ioc);
  int count = 0;

  post(ioc, bindns::bind(increment_without_lock, &s, &count));

  // No handlers can be called until run() is called.
  ASIO_CHECK(count == 0);

  ioc.run();

  // The run() call will not return until all work has finished.
  ASIO_CHECK(count == 1);

  count = 0;
  ioc.restart();
  post(s, bindns::bind(increment_with_lock, &s, &count));

  // No handlers can be called until run() is called.
  ASIO_CHECK(count == 0);

  ioc.run();

  // The run() call will not return until all work has finished.
  ASIO_CHECK(count == 1);

  count = 0;
  ioc.restart();
  post(ioc, bindns::bind(start_sleep_increments, &ioc, &s, &count));
  thread thread1(bindns::bind(io_context_run, &ioc));
  thread thread2(bindns::bind(io_context_run, &ioc));

  // Check all events run one after another even though there are two threads.
  timer timer1(ioc, chronons::seconds(3));
  timer1.wait();
  ASIO_CHECK(count == 0);
#if defined(ASIO_HAS_BOOST_DATE_TIME)
  timer1.expires_at(timer1.expires_at() + chronons::seconds(2));
#else // defined(ASIO_HAS_BOOST_DATE_TIME)
  timer1.expires_at(timer1.expiry() + chronons::seconds(2));
#endif // defined(ASIO_HAS_BOOST_DATE_TIME)
  timer1.wait();
  ASIO_CHECK(count == 1);
#if defined(ASIO_HAS_BOOST_DATE_TIME)
  timer1.expires_at(timer1.expires_at() + chronons::seconds(2));
#else // defined(ASIO_HAS_BOOST_DATE_TIME)
  timer1.expires_at(timer1.expiry() + chronons::seconds(2));
#endif // defined(ASIO_HAS_BOOST_DATE_TIME)
  timer1.wait();
  ASIO_CHECK(count == 2);

  thread1.join();
  thread2.join();

  // The run() calls will not return until all work has finished.
  ASIO_CHECK(count == 3);

  count = 0;
  int exception_count = 0;
  ioc.restart();
  post(s, throw_exception);
  post(s, bindns::bind(increment, &count));
  post(s, bindns::bind(increment, &count));
  post(s, throw_exception);
  post(s, bindns::bind(increment, &count));

  // No handlers can be called until run() is called.
  ASIO_CHECK(count == 0);
  ASIO_CHECK(exception_count == 0);

  for (;;)
  {
    try
    {
      ioc.run();
      break;
    }
    catch (int)
    {
      ++exception_count;
    }
  }

  // The run() calls will not return until all work has finished.
  ASIO_CHECK(count == 3);
  ASIO_CHECK(exception_count == 2);

  count = 0;
  ioc.restart();

  // Check for clean shutdown when handlers posted through an orphaned strand
  // are abandoned.
  {
    strand<io_context::executor_type> s2 = make_strand(ioc.get_executor());
    post(s2, bindns::bind(increment, &count));
    post(s2, bindns::bind(increment, &count));
    post(s2, bindns::bind(increment, &count));
  }

  // No handlers can be called until run() is called.
  ASIO_CHECK(count == 0);
}

void strand_conversion_test()
{
  io_context ioc;
  strand<io_context::executor_type> s1 = make_strand(ioc);

  // Converting constructors.

  strand<executor> s2(s1);
  strand<executor> s3 = strand<io_context::executor_type>(s1);

  // Converting assignment.

  s3 = s1;
  s3 = strand<io_context::executor_type>(s1);
}

void strand_query_test()
{
  io_context ioc;
  strand<io_context::executor_type> s1 = make_strand(ioc);

  ASIO_CHECK(
      &asio::query(s1, asio::execution::context)
        == &ioc);

  ASIO_CHECK(
      asio::query(s1, asio::execution::blocking)
        == asio::execution::blocking.possibly);

  ASIO_CHECK(
      asio::query(s1, asio::execution::blocking.possibly)
        == asio::execution::blocking.possibly);

  ASIO_CHECK(
      asio::query(s1, asio::execution::outstanding_work)
        == asio::execution::outstanding_work.untracked);

  ASIO_CHECK(
      asio::query(s1, asio::execution::outstanding_work.untracked)
        == asio::execution::outstanding_work.untracked);

  ASIO_CHECK(
      asio::query(s1, asio::execution::relationship)
        == asio::execution::relationship.fork);

  ASIO_CHECK(
      asio::query(s1, asio::execution::relationship.fork)
        == asio::execution::relationship.fork);

  ASIO_CHECK(
      asio::query(s1, asio::execution::mapping)
        == asio::execution::mapping.thread);

  ASIO_CHECK(
      asio::query(s1, asio::execution::allocator)
        == std::allocator<void>());
}

void strand_execute_test()
{
  io_context ioc;
  strand<io_context::executor_type> s1 = make_strand(ioc);
  int count = 0;

  s1.execute(bindns::bind(increment, &count));

  // No handlers can be called until run() is called.
  ASIO_CHECK(!ioc.stopped());
  ASIO_CHECK(count == 0);

  ioc.run();

  // The run() call will not return until all work has finished.
  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);

  count = 0;
  ioc.restart();
  asio::require(s1, asio::execution::blocking.possibly).execute(
      bindns::bind(increment, &count));

  // No handlers can be called until run() is called.
  ASIO_CHECK(!ioc.stopped());
  ASIO_CHECK(count == 0);

  ioc.run();

  // The run() call will not return until all work has finished.
  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);

  count = 0;
  ioc.restart();
  asio::require(s1, asio::execution::blocking.never).execute(
      bindns::bind(increment, &count));

  // No handlers can be called until run() is called.
  ASIO_CHECK(!ioc.stopped());
  ASIO_CHECK(count == 0);

  ioc.run();

  // The run() call will not return until all work has finished.
  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);

  count = 0;
  ioc.restart();
  ASIO_CHECK(!ioc.stopped());

  asio::require(s1,
      asio::execution::blocking.never,
      asio::execution::outstanding_work.tracked
    ).execute(bindns::bind(increment, &count));

  // No handlers can be called until run() is called.
  ASIO_CHECK(!ioc.stopped());
  ASIO_CHECK(count == 0);

  ioc.run();

  // The run() call will not return until all work has finished.
  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);

  count = 0;
  ioc.restart();
  asio::require(s1,
      asio::execution::blocking.never,
      asio::execution::outstanding_work.untracked
    ).execute(bindns::bind(increment, &count));

  // No handlers can be called until run() is called.
  ASIO_CHECK(!ioc.stopped());
  ASIO_CHECK(count == 0);

  ioc.run();

  // The run() call will not return until all work has finished.
  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);

  count = 0;
  ioc.restart();
  asio::require(s1,
      asio::execution::blocking.never,
      asio::execution::outstanding_work.untracked,
      asio::execution::relationship.fork
    ).execute(bindns::bind(increment, &count));

  // No handlers can be called until run() is called.
  ASIO_CHECK(!ioc.stopped());
  ASIO_CHECK(count == 0);

  ioc.run();

  // The run() call will not return until all work has finished.
  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);

  count = 0;
  ioc.restart();
  asio::require(s1,
      asio::execution::blocking.never,
      asio::execution::outstanding_work.untracked,
      asio::execution::relationship.continuation
    ).execute(bindns::bind(increment, &count));

  // No handlers can be called until run() is called.
  ASIO_CHECK(!ioc.stopped());
  ASIO_CHECK(count == 0);

  ioc.run();

  // The run() call will not return until all work has finished.
  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);

  count = 0;
  ioc.restart();
  asio::prefer(
      asio::require(s1,
        asio::execution::blocking.never,
        asio::execution::outstanding_work.untracked,
        asio::execution::relationship.continuation),
      asio::execution::allocator(std::allocator<void>())
    ).execute(bindns::bind(increment, &count));

  // No handlers can be called until run() is called.
  ASIO_CHECK(!ioc.stopped());
  ASIO_CHECK(count == 0);

  ioc.run();

  // The run() call will not return until all work has finished.
  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);

  count = 0;
  ioc.restart();
  asio::prefer(
      asio::require(s1,
        asio::execution::blocking.never,
        asio::execution::outstanding_work.untracked,
        asio::execution::relationship.continuation),
      asio::execution::allocator
    ).execute(bindns::bind(increment, &count));

  // No handlers can be called until run() is called.
  ASIO_CHECK(!ioc.stopped());
  ASIO_CHECK(count == 0);

  ioc.run();

  // The run() call will not return until all work has finished.
  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);
}

ASIO_TEST_SUITE
(
  "strand",
  ASIO_TEST_CASE(strand_test)
  ASIO_COMPILE_TEST_CASE(strand_conversion_test)
  ASIO_TEST_CASE(strand_query_test)
  ASIO_TEST_CASE(strand_execute_test)
)
