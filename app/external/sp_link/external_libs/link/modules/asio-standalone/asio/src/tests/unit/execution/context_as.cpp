//
// context_as.cpp
// ~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2020 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

// Disable autolinking for unit tests.
#if !defined(BOOST_ALL_NO_LIB)
#define BOOST_ALL_NO_LIB 1
#endif // !defined(BOOST_ALL_NO_LIB)

// Test that header file is self-contained.
#include "asio/execution/context_as.hpp"

#include "asio/execution/any_executor.hpp"
#include "asio/io_context.hpp"
#include "asio/static_thread_pool.hpp"
#include "../unit_test.hpp"

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

void context_as_executor_query_test()
{
  static_thread_pool pool(1);

  ASIO_CHECK(
      &asio::query(pool.executor(),
        execution::context_as_t<static_thread_pool&>())
        == &pool);

  execution::any_executor<
      execution::context_as_t<static_thread_pool&>
    > ex1 = pool.executor();

  ASIO_CHECK(
      &asio::query(ex1,
        execution::context_as_t<static_thread_pool&>())
        == &pool);

  ASIO_CHECK(
      &asio::query(ex1, execution::context)
        == &pool);

  ASIO_CHECK(
      &asio::query(pool.executor(),
        execution::context_as_t<const static_thread_pool&>())
        == &pool);

  execution::any_executor<
      execution::context_as_t<const static_thread_pool&>
    > ex2 = pool.executor();

  ASIO_CHECK(
      &asio::query(ex2,
        execution::context_as_t<const static_thread_pool&>())
        == &pool);

  ASIO_CHECK(
      &asio::query(ex2, execution::context)
        == &pool);

  io_context io_ctx;

  ASIO_CHECK(
      &asio::query(io_ctx.get_executor(),
        execution::context_as_t<io_context&>())
        == &io_ctx);

  execution::any_executor<
      execution::context_as_t<io_context&>
    > ex3 = io_ctx.get_executor();

  ASIO_CHECK(
      &asio::query(ex3,
        execution::context_as_t<io_context&>())
        == &io_ctx);

  ASIO_CHECK(
      &asio::query(ex3, execution::context)
        == &io_ctx);

  ASIO_CHECK(
      &asio::query(io_ctx.get_executor(),
        execution::context_as_t<const io_context&>())
        == &io_ctx);

  execution::any_executor<
      execution::context_as_t<const io_context&>
    > ex4 = io_ctx.get_executor();

  ASIO_CHECK(
      &asio::query(ex4,
        execution::context_as_t<const io_context&>())
        == &io_ctx);

  ASIO_CHECK(
      &asio::query(ex4, execution::context)
        == &io_ctx);

  ASIO_CHECK(
      &asio::query(io_ctx.get_executor(),
        execution::context_as_t<execution_context&>())
        == &io_ctx);

  execution::any_executor<
      execution::context_as_t<execution_context&>
    > ex5 = io_ctx.get_executor();

  ASIO_CHECK(
      &asio::query(ex5,
        execution::context_as_t<execution_context&>())
        == &io_ctx);

  ASIO_CHECK(
      &asio::query(ex5, execution::context)
        == &io_ctx);

  ASIO_CHECK(
      &asio::query(io_ctx.get_executor(),
        execution::context_as_t<const execution_context&>())
        == &io_ctx);

  execution::any_executor<
      execution::context_as_t<const execution_context&>
    > ex6 = io_ctx.get_executor();

  ASIO_CHECK(
      &asio::query(ex6,
        execution::context_as_t<const execution_context&>())
        == &io_ctx);

  ASIO_CHECK(
      &asio::query(ex6, execution::context)
        == &io_ctx);
}

ASIO_TEST_SUITE
(
  "context_as",
  ASIO_TEST_CASE(context_as_executor_query_test)
)
