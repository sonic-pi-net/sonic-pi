//
// system_executor.cpp
// ~~~~~~~~~~~~~~~~~~~
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

// Prevent link dependency on the Boost.System library.
#if !defined(BOOST_SYSTEM_NO_DEPRECATED)
#define BOOST_SYSTEM_NO_DEPRECATED
#endif // !defined(BOOST_SYSTEM_NO_DEPRECATED)

// Test that header file is self-contained.
#include "asio/system_executor.hpp"

#include "asio/dispatch.hpp"
#include "asio/post.hpp"
#include "unit_test.hpp"

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

void increment(asio::detail::atomic_count* count)
{
  ++(*count);
}

void system_executor_query_test()
{
  ASIO_CHECK(
      &asio::query(system_executor(),
        asio::execution::context)
      != static_cast<const system_context*>(0));

  ASIO_CHECK(
      asio::query(system_executor(),
        asio::execution::blocking)
      == asio::execution::blocking.possibly);

  ASIO_CHECK(
      asio::query(system_executor(),
        asio::execution::blocking.possibly)
      == asio::execution::blocking.possibly);

  ASIO_CHECK(
      asio::query(system_executor(),
        asio::execution::outstanding_work)
      == asio::execution::outstanding_work.untracked);

  ASIO_CHECK(
      asio::query(system_executor(),
        asio::execution::outstanding_work.untracked)
      == asio::execution::outstanding_work.untracked);

  ASIO_CHECK(
      asio::query(system_executor(),
        asio::execution::relationship)
      == asio::execution::relationship.fork);

  ASIO_CHECK(
      asio::query(system_executor(),
        asio::execution::relationship.fork)
      == asio::execution::relationship.fork);

#if !defined(ASIO_NO_DEPRECATED)
  ASIO_CHECK(
      asio::query(system_executor(),
        asio::execution::bulk_guarantee)
      == asio::execution::bulk_guarantee.unsequenced);
#endif // !defined(ASIO_NO_DEPRECATED)

  ASIO_CHECK(
      asio::query(system_executor(),
        asio::execution::mapping)
      == asio::execution::mapping.thread);

  ASIO_CHECK(
      asio::query(system_executor(),
        asio::execution::allocator)
      == std::allocator<void>());
}

void system_executor_execute_test()
{
  asio::detail::atomic_count count(0);

  system_executor().execute(bindns::bind(increment, &count));

  asio::require(system_executor(),
      asio::execution::blocking.possibly
    ).execute(bindns::bind(increment, &count));

  asio::require(system_executor(),
      asio::execution::blocking.always
    ).execute(bindns::bind(increment, &count));

  asio::require(system_executor(),
      asio::execution::blocking.never
    ).execute(bindns::bind(increment, &count));

  asio::require(system_executor(),
      asio::execution::blocking.never,
      asio::execution::outstanding_work.untracked
    ).execute(bindns::bind(increment, &count));

  asio::require(system_executor(),
      asio::execution::blocking.never,
      asio::execution::outstanding_work.untracked,
      asio::execution::relationship.fork
    ).execute(bindns::bind(increment, &count));

  asio::require(system_executor(),
      asio::execution::blocking.never,
      asio::execution::outstanding_work.untracked,
      asio::execution::relationship.continuation
    ).execute(bindns::bind(increment, &count));

  asio::prefer(
      asio::require(system_executor(),
        asio::execution::blocking.never,
        asio::execution::outstanding_work.untracked,
        asio::execution::relationship.continuation),
      asio::execution::allocator(std::allocator<void>())
    ).execute(bindns::bind(increment, &count));

  asio::prefer(
      asio::require(system_executor(),
        asio::execution::blocking.never,
        asio::execution::outstanding_work.untracked,
        asio::execution::relationship.continuation),
      asio::execution::allocator
    ).execute(bindns::bind(increment, &count));

  asio::query(system_executor(), execution::context).join();

  ASIO_CHECK(count == 9);
}

ASIO_TEST_SUITE
(
  "system_executor",
  ASIO_TEST_CASE(system_executor_query_test)
  ASIO_TEST_CASE(system_executor_execute_test)
)
