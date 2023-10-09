//
// co_spawn.cpp
// ~~~~~~~~~~~~
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
#include "asio/co_spawn.hpp"

#include "unit_test.hpp"

#if defined(ASIO_HAS_CO_AWAIT)

#include "asio/any_completion_handler.hpp"
#include "asio/io_context.hpp"

asio::awaitable<void> void_returning_coroutine()
{
  co_return;
}

asio::awaitable<int> int_returning_coroutine()
{
  co_return 42;
}

void test_co_spawn_with_any_completion_handler()
{
  asio::io_context ctx;

  bool called = false;
  asio::co_spawn(ctx, void_returning_coroutine(),
      asio::any_completion_handler<void(std::exception_ptr)>(
        [&](std::exception_ptr)
        {
          called = true;
        }));

  ASIO_CHECK(!called);

  ctx.run();

  ASIO_CHECK(called);

  int result = 0;
  asio::co_spawn(ctx, int_returning_coroutine(),
      asio::any_completion_handler<void(std::exception_ptr, int)>(
        [&](std::exception_ptr, int i)
        {
          result = i;
        }));

  ASIO_CHECK(result == 0);

  ctx.restart();
  ctx.run();

  ASIO_CHECK(result == 42);
}

ASIO_TEST_SUITE
(
  "co_spawn",
  ASIO_TEST_CASE(test_co_spawn_with_any_completion_handler)
)

#else // defined(ASIO_HAS_CO_AWAIT)

ASIO_TEST_SUITE
(
  "co_spawn",
  ASIO_TEST_CASE(null_test)
)

#endif // defined(ASIO_HAS_CO_AWAIT)
