//
// experimental/coro/stack_test.cpp
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
#include "asio/experimental/coro.hpp"

#include "asio/detached.hpp"
#include "asio/io_context.hpp"
#include <iostream>
#include "../../unit_test.hpp"

using namespace asio::experimental;

namespace coro {

asio::experimental::coro<int()>
  stack_generator(asio::any_io_executor, int i = 1)
{
  for (;;)
  {
    co_yield i;
    i *= 2;
  }
}

asio::experimental::coro<int(int)>
stack_accumulate(asio::any_io_executor exec)
{
  auto gen  = stack_generator(exec);
  int offset = 0;
  while (auto next = co_await gen) // 1, 2, 4, 8, ...
    offset  = co_yield *next + offset; // offset is delayed by one cycle
}

asio::experimental::coro<int>
main_stack_coro(asio::io_context&, bool & done)
{
  auto g = stack_accumulate(co_await asio::this_coro::executor);

  ASIO_CHECK(g.is_open());
  ASIO_CHECK(1    == (co_await g(1000)).value_or(-1));
  ASIO_CHECK(2002 == (co_await g(2000)).value_or(-1));
  ASIO_CHECK(3004 == (co_await g(3000)).value_or(-1));
  ASIO_CHECK(4008 == (co_await g(4000)).value_or(-1));
  ASIO_CHECK(5016 == (co_await g(5000)).value_or(-1));
  ASIO_CHECK(6032 == (co_await g(6000)).value_or(-1));
  ASIO_CHECK(7064 == (co_await g(7000)).value_or(-1));
  ASIO_CHECK(8128 == (co_await g(8000)).value_or(-1));
  ASIO_CHECK(9256 == (co_await g(9000)).value_or(-1));
  ASIO_CHECK(511 == (co_await g(-1)).value_or(-1));
  done = true;
}

void stack_test()
{
  bool done = false;
  asio::io_context ctx;
  auto k = main_stack_coro(ctx, done);
  k.async_resume(asio::detached);
  ctx.run();
  ASIO_CHECK(done);
}

} // namespace coro

ASIO_TEST_SUITE
(
  "coro/stack_test",
  ASIO_TEST_CASE(::coro::stack_test)
)
