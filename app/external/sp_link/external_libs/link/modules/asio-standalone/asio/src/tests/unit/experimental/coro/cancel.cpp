//
// experimental/coro/cancel.cpp
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
#include <iostream>
#include "asio/bind_cancellation_slot.hpp"
#include "asio/io_context.hpp"
#include "asio/steady_timer.hpp"
#include "asio/this_coro.hpp"
#include "../../unit_test.hpp"

using namespace asio::experimental;
namespace this_coro = asio::this_coro;

namespace coro {


auto coro_simple_cancel_impl(asio::io_context& ) noexcept
  -> asio::experimental::coro<void() noexcept, asio::error_code>
{
    ASIO_CHECK(
        !(co_await this_coro::cancellation_state).cancelled());

    asio::steady_timer timer{
        co_await this_coro::executor,
        std::chrono::seconds(1)};

    ASIO_CHECK(
        !(co_await this_coro::cancellation_state).cancelled());

    auto ec = co_await timer;

    ASIO_CHECK(
        (co_await this_coro::cancellation_state).cancelled());

    co_return ec;
}

void coro_simple_cancel()
{
  asio::io_context ctx;
  asio::cancellation_signal sig;

  auto k = coro_simple_cancel_impl(ctx);

  asio::error_code res_ec;
  k.async_resume(
      asio::bind_cancellation_slot(sig.slot(),
        [&](asio::error_code ec) {res_ec = ec;}));
  asio::post(ctx, [&]{sig.emit(asio::cancellation_type::all);});

  ASIO_CHECK(!res_ec);

  ctx.run();

  ASIO_CHECK(res_ec == asio::error::operation_aborted);
}

auto coro_throw_cancel_impl(asio::io_context& )
  -> asio::experimental::coro<void() , void>
{
    asio::steady_timer timer{
        co_await this_coro::executor,
        std::chrono::seconds(1)};
    co_await timer;
}

void coro_throw_cancel()
{
  asio::io_context ctx;
  asio::cancellation_signal sig;

  auto k = coro_throw_cancel_impl(ctx);

  std::exception_ptr res_ex;
  k.async_resume(
      asio::bind_cancellation_slot(sig.slot(),
        [&](std::exception_ptr ex) {res_ex = ex;}));
  asio::post(ctx, [&]{sig.emit(asio::cancellation_type::all);});

  ASIO_CHECK(!res_ex);

  ctx.run();

  ASIO_CHECK(res_ex);
  try
  {
    if (res_ex)
      std::rethrow_exception(res_ex);
  }
  catch (asio::system_error& se)
  {
    ASIO_CHECK(se.code() == asio::error::operation_aborted);
  }
}

auto coro_simple_cancel_nested_k(asio::io_context&, int& cnt) noexcept
  -> asio::experimental::coro<
      void() noexcept,
      asio::error_code>
{
  asio::steady_timer timer{
          co_await this_coro::executor,
          std::chrono::milliseconds(100)};

  ASIO_CHECK(!(co_await this_coro::cancellation_state).cancelled());
  auto ec = co_await timer;
  cnt++;
  ASIO_CHECK((co_await this_coro::cancellation_state).cancelled());

  co_return ec;
}

auto coro_simple_cancel_nested_kouter(
    asio::io_context& ctx, int& cnt) noexcept
  -> asio::experimental::coro<
      asio::error_code() noexcept,
      asio::error_code>
{
    ASIO_CHECK(cnt == 0);
    co_yield co_await coro_simple_cancel_nested_k(ctx, cnt);
    ASIO_CHECK(cnt == 1);
    auto ec = co_await coro_simple_cancel_nested_k(ctx, cnt);
    ASIO_CHECK(cnt == 2);
    co_return ec;
}

void coro_simple_cancel_nested()
{
  asio::io_context ctx;
  asio::cancellation_signal sig;

  int cnt = 0;
  auto kouter = coro_simple_cancel_nested_kouter(ctx, cnt);

  asio::error_code res_ec;
  kouter.async_resume(
      asio::bind_cancellation_slot(sig.slot(),
        [&](asio::error_code ec) {res_ec = ec;}));
  asio::post(ctx, [&]{sig.emit(asio::cancellation_type::all);});
  ASIO_CHECK(!res_ec);
  ctx.run();
  ASIO_CHECK(res_ec == asio::error::operation_aborted);

  ctx.restart();
  res_ec = {};
  kouter.async_resume(
      asio::bind_cancellation_slot(sig.slot(),
        [&](asio::error_code ec) {res_ec = ec;}));
  asio::post(ctx, [&]{sig.emit(asio::cancellation_type::all);});
  ASIO_CHECK(!res_ec);
  ctx.run();
  ASIO_CHECK(res_ec == asio::error::operation_aborted);
  ASIO_CHECK(cnt == 2);
}

} // namespace coro

ASIO_TEST_SUITE
(
  "coro/cancel",
  ASIO_TEST_CASE(::coro::coro_simple_cancel)
  ASIO_TEST_CASE(::coro::coro_throw_cancel)
  ASIO_TEST_CASE(::coro::coro_simple_cancel_nested)
)
