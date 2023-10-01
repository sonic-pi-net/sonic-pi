//
// promise.cpp
// ~~~~~~~~~~~
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
#include "asio/experimental/promise.hpp"

#include "asio/append.hpp"
#include "asio/bind_cancellation_slot.hpp"
#include "asio/compose.hpp"
#include "asio/deferred.hpp"
#include "asio/experimental/use_promise.hpp"
#include "asio/steady_timer.hpp"
#include "../unit_test.hpp"

namespace promise {

void promise_tester()
{
  using namespace asio;
  using namespace std::chrono;

  io_context ctx;

  steady_timer timer1{ctx}, timer2{ctx};

  const auto started_when = steady_clock::now();
  timer1.expires_at(started_when + milliseconds(5000));
  timer2.expires_at(started_when + milliseconds(1000));
  auto p1 = timer1.async_wait(experimental::use_promise);

  steady_clock::time_point completed_when;
  asio::error_code ec;
  bool called = false;

  p1([&](asio::error_code ec_)
      {
        ec = ec_;
        called = true;
        completed_when = steady_clock::now();
      });

  steady_clock::time_point timer2_done;
  timer2.async_wait(
      [&](asio::error_code)
      {
        timer2_done = steady_clock::now();
        p1.cancel();
      });

  ctx.run();

  static_assert(
      asio::is_async_operation<decltype(p1)>::value,
      "promise is async_op");

  ASIO_CHECK(timer2_done + milliseconds(1) > started_when);
  ASIO_CHECK(completed_when > timer2_done);
  ASIO_CHECK(called);
  ASIO_CHECK(ec == error::operation_aborted);

  timer1.expires_after(milliseconds(0));
  auto p2 = timer1.async_wait(
      asio::append(experimental::use_promise, 123));

  ec = asio::error::would_block;
  called = false;

  p2([&](asio::error_code ec_, int i)
      {
        ASIO_CHECK(i == 123);
        ec = ec_;
        called = true;
      });

  ASIO_CHECK(ec == asio::error::would_block);
  ASIO_CHECK(!called);

  ctx.restart();
  ctx.run();

  static_assert(
      asio::is_async_operation<decltype(p2)>::value,
      "promise is async_op");

  ASIO_CHECK(!ec);
  ASIO_CHECK(called);
}

void promise_slot_tester()
{
  using namespace asio;
  using namespace std::chrono;

  io_context ctx;

  steady_timer timer1{ctx}, timer2{ctx};

  const auto started_when = steady_clock::now();
  timer1.expires_at(started_when + milliseconds(2500));
  timer2.expires_at(started_when + milliseconds(1000));
  auto p = timer1.async_wait(experimental::use_promise);

  steady_clock::time_point completed_when;
  asio::error_code ec;
  bool called = false;

  asio::cancellation_signal sig;

  p(asio::bind_cancellation_slot(
        sig.slot(),
        [&](asio::error_code ec_)
        {
          ec = ec_;
          called = true;
          completed_when = steady_clock::now();
        }));

  steady_clock::time_point timer2_done;
  timer2.async_wait(
      [&](asio::error_code)
      {
        timer2_done = steady_clock::now();
        sig.emit(asio::cancellation_type::all);
      });

  ctx.run();

  static_assert(
      asio::is_async_operation<decltype(p)>::value,
      "promise is async_op");

  ASIO_CHECK(timer2_done + milliseconds(1) > started_when);
  ASIO_CHECK(completed_when > timer2_done);
  ASIO_CHECK(called);
  ASIO_CHECK(ec == error::operation_aborted);
}

void early_completion()
{
  using namespace asio;
  using namespace std::chrono;

  io_context ctx;
  auto p = asio::post(ctx, asio::experimental::use_promise);
  ctx.run();

  ASIO_CHECK(p.completed());

  bool completed = false;
  p([&]{completed = true;});
  ASIO_CHECK(!completed);
  ctx.restart();
  ctx.run();
  ASIO_CHECK(completed);
}

struct test_cancel_impl_op
{
  asio::steady_timer & tim;
  asio::error_code &ec;
  template<typename Self>
  void operator()(Self& self)
  {
    tim.async_wait(std::forward<Self>(self));
  }

  template<typename Self>
  void operator()(Self& self, asio::error_code ec_)
  {
    ec = ec_;
    self.complete(ec_);
  }
};

template <typename CompletionToken>
ASIO_INITFN_AUTO_RESULT_TYPE(
    CompletionToken, void(asio::error_code))
test_cancel_impl(asio::steady_timer & tim,
                 asio::error_code &ec,
                 CompletionToken&& token)
{
  return asio::async_compose<CompletionToken, void(asio::error_code)>(
      test_cancel_impl_op{tim, ec}, token, tim);
}

void test_cancel()
{
  asio::io_context ctx;
  asio::steady_timer tim{ctx, std::chrono::seconds(10)};
  asio::error_code ec;

  {
    auto p = test_cancel_impl(
        tim, ec, asio::experimental::use_promise);
  }

  ctx.run();

  ASIO_CHECK_MESSAGE(
      ec == asio::error::operation_aborted,
      ec.message());
}

} // namespace promise

ASIO_TEST_SUITE
(
  "promise",
  ASIO_TEST_CASE(promise::promise_tester)
  ASIO_TEST_CASE(promise::promise_slot_tester)
  ASIO_TEST_CASE(promise::early_completion)
  ASIO_TEST_CASE(promise::test_cancel)
)
