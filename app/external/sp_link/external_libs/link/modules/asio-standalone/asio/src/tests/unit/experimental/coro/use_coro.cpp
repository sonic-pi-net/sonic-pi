//
// experimental/coro/use_coro.cpp
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
// Copyright (c) 2021-2023 Klemens D. Morgenstern
//                         (klemens dot morgenstern at gmx dot net)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

// Disable autolinking for unit tests.
#if !defined(BOOST_ALL_NO_LIB)
#define BOOST_ALL_NO_LIB 1
#endif // !defined(BOOST_ALL_NO_LIB)

// Test that header file is self-contained.
#include "asio/experimental/use_coro.hpp"

#include "asio/steady_timer.hpp"
#include <iostream>
#include "../../unit_test.hpp"

using namespace asio::experimental;

namespace coro {

asio::experimental::coro<void(), int>
awaiter(asio::any_io_executor exec)
{
  asio::steady_timer timer{exec};
  co_await timer.async_wait(use_coro);
  co_return 42;
}

asio::experimental::coro<void() noexcept, int>
awaiter_noexcept(asio::any_io_executor exec)
{
  asio::steady_timer timer{exec};
  auto ec = co_await timer.async_wait(asio::deferred);
  ASIO_CHECK(ec == asio::error_code{});
  co_return 42;
}

void stack_test2()
{
  bool done = false;
  asio::io_context ctx;

  auto k = awaiter(ctx.get_executor());
  auto k2 = awaiter_noexcept(ctx.get_executor());

  k.async_resume(
      [&](std::exception_ptr ex, int res)
      {
        ASIO_CHECK(!ex);
        ASIO_CHECK(res == 42);
        done = true;
      });

  k2.async_resume([&](int res)
       {
         ASIO_CHECK(res == 42);
         done = true;
       });

  ctx.run();
  ASIO_CHECK(done);
}

} // namespace coro

ASIO_TEST_SUITE
(
  "coro/use_coro",
  ASIO_TEST_CASE(::coro::stack_test2)
)
