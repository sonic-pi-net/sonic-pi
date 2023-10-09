//
// redirect_error.cpp
// ~~~~~~~~~~~~~~~~~~
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
#include "asio/redirect_error.hpp"

#include "asio/bind_executor.hpp"
#include "asio/deferred.hpp"
#include "asio/io_context.hpp"
#include "asio/post.hpp"
#include "asio/system_timer.hpp"
#include "asio/use_future.hpp"
#include "unit_test.hpp"

struct redirect_error_handler
{
  int* count_;

  explicit redirect_error_handler(int* c)
    : count_(c)
  {
  }

  void operator()()
  {
    ++(*count_);
  }
};

void redirect_error_test()
{
  asio::io_context io1;
  asio::io_context io2;
  asio::system_timer timer1(io1);
  asio::error_code ec = asio::error::would_block;
  int count = 0;

  timer1.expires_after(asio::chrono::seconds(0));
  timer1.async_wait(
      asio::redirect_error(
        asio::bind_executor(io2.get_executor(),
          redirect_error_handler(&count)), ec));

  ASIO_CHECK(ec == asio::error::would_block);
  ASIO_CHECK(count == 0);

  io1.run();

  ASIO_CHECK(ec == asio::error::would_block);
  ASIO_CHECK(count == 0);

  io2.run();

  ASIO_CHECK(!ec);
  ASIO_CHECK(count == 1);

#if defined(ASIO_HAS_STD_TUPLE) \
  && defined(ASIO_HAS_DECLTYPE) \
  && defined(ASIO_HAS_VARIADIC_TEMPLATES)
  ec = asio::error::would_block;
  timer1.async_wait(
      asio::redirect_error(
        asio::bind_executor(io2.get_executor(),
          asio::deferred), ec))(redirect_error_handler(&count));

  ASIO_CHECK(ec == asio::error::would_block);
  ASIO_CHECK(count == 1);

  io1.restart();
  io1.run();

  ASIO_CHECK(ec == asio::error::would_block);
  ASIO_CHECK(count == 1);

  io2.restart();
  io2.run();

  ASIO_CHECK(!ec);
  ASIO_CHECK(count == 2);
#endif // defined(ASIO_HAS_STD_TUPLE)
       //   && defined(ASIO_HAS_DECLTYPE)
       //   && defined(ASIO_HAS_VARIADIC_TEMPLATES)

#if defined(ASIO_HAS_STD_FUTURE_CLASS)
  ec = asio::error::would_block;
  std::future<void> f = timer1.async_wait(
      asio::redirect_error(
        asio::bind_executor(io2.get_executor(),
          asio::use_future), ec));

  ASIO_CHECK(ec == asio::error::would_block);
  ASIO_CHECK(f.wait_for(std::chrono::seconds(0))
      == std::future_status::timeout);

  io1.restart();
  io1.run();

  ASIO_CHECK(ec == asio::error::would_block);
  ASIO_CHECK(f.wait_for(std::chrono::seconds(0))
      == std::future_status::timeout);

  io2.restart();
  io2.run();

  ASIO_CHECK(!ec);
  ASIO_CHECK(f.wait_for(std::chrono::seconds(0))
      == std::future_status::ready);
#endif // defined(ASIO_HAS_STD_FUTURE_CLASS)
}

ASIO_TEST_SUITE
(
  "redirect_error",
  ASIO_TEST_CASE(redirect_error_test)
)
