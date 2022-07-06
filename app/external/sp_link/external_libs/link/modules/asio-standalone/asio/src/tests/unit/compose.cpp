//
// compose.cpp
// ~~~~~~~~~~~
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
#include "asio/compose.hpp"

#include "unit_test.hpp"

#include "asio/io_context.hpp"
#include "asio/post.hpp"

#if defined(ASIO_HAS_BOOST_BIND)
# include <boost/bind/bind.hpp>
#else // defined(ASIO_HAS_BOOST_BIND)
# include <functional>
#endif // defined(ASIO_HAS_BOOST_BIND)

//------------------------------------------------------------------------------

class impl_0_completion_args
{
public:
  explicit impl_0_completion_args(asio::io_context& ioc)
    : ioc_(ioc),
      state_(starting)
  {
  }

  template <typename Self>
  void operator()(Self& self)
  {
    switch (state_)
    {
    case starting:
      state_ = posting;
      asio::post(ioc_, ASIO_MOVE_CAST(Self)(self));
      break;
    case posting:
      self.complete();
      break;
    default:
      break;
    }
  }

private:
  asio::io_context& ioc_;
  enum { starting, posting } state_;
};

template <typename CompletionToken>
ASIO_INITFN_RESULT_TYPE(CompletionToken, void())
async_0_completion_args(asio::io_context& ioc,
    ASIO_MOVE_ARG(CompletionToken) token)
{
  return asio::async_compose<CompletionToken, void()>(
      impl_0_completion_args(ioc), token);
}

void compose_0_args_handler(int* count)
{
  ++(*count);
}

struct compose_0_args_lvalue_handler
{
  int* count_;

  void operator()()
  {
    ++(*count_);
  }
};

void compose_0_completion_args_test()
{
#if defined(ASIO_HAS_BOOST_BIND)
  namespace bindns = boost;
#else // defined(ASIO_HAS_BOOST_BIND)
  namespace bindns = std;
#endif // defined(ASIO_HAS_BOOST_BIND)

  asio::io_context ioc;
  int count = 0;

  async_0_completion_args(ioc, bindns::bind(&compose_0_args_handler, &count));

  // No handlers can be called until run() is called.
  ASIO_CHECK(!ioc.stopped());
  ASIO_CHECK(count == 0);

  ioc.run();

  // The run() call will not return until all work has finished.
  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);

  ioc.restart();
  count = 0;

  compose_0_args_lvalue_handler lvalue_handler = { &count };
  async_0_completion_args(ioc, lvalue_handler);

  // No handlers can be called until run() is called.
  ASIO_CHECK(!ioc.stopped());
  ASIO_CHECK(count == 0);

  ioc.run();

  // The run() call will not return until all work has finished.
  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);
}

//------------------------------------------------------------------------------

class impl_1_completion_arg
{
public:
  explicit impl_1_completion_arg(asio::io_context& ioc)
    : ioc_(ioc),
      state_(starting)
  {
  }

  template <typename Self>
  void operator()(Self& self)
  {
    switch (state_)
    {
    case starting:
      state_ = posting;
      asio::post(ioc_, ASIO_MOVE_CAST(Self)(self));
      break;
    case posting:
      self.complete(42);
      break;
    default:
      break;
    }
  }

private:
  asio::io_context& ioc_;
  enum { starting, posting } state_;
};

template <typename CompletionToken>
ASIO_INITFN_RESULT_TYPE(CompletionToken, void(int))
async_1_completion_arg(asio::io_context& ioc,
    ASIO_MOVE_ARG(CompletionToken) token)
{
  return asio::async_compose<CompletionToken, void(int)>(
      impl_1_completion_arg(ioc), token);
}

void compose_1_arg_handler(int* count, int* result_out, int result)
{
  ++(*count);
  *result_out = result;
}

struct compose_1_arg_lvalue_handler
{
  int* count_;
  int* result_out_;

  void operator()(int result)
  {
    ++(*count_);
    *result_out_ = result;
  }
};

void compose_1_completion_arg_test()
{
#if defined(ASIO_HAS_BOOST_BIND)
  namespace bindns = boost;
#else // defined(ASIO_HAS_BOOST_BIND)
  namespace bindns = std;
#endif // defined(ASIO_HAS_BOOST_BIND)
  using bindns::placeholders::_1;

  asio::io_context ioc;
  int count = 0;
  int result = 0;

  async_1_completion_arg(ioc,
      bindns::bind(&compose_1_arg_handler, &count, &result, _1));

  // No handlers can be called until run() is called.
  ASIO_CHECK(!ioc.stopped());
  ASIO_CHECK(count == 0);
  ASIO_CHECK(result == 0);

  ioc.run();

  // The run() call will not return until all work has finished.
  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);
  ASIO_CHECK(result == 42);

  ioc.restart();
  count = 0;
  result = 0;

  compose_1_arg_lvalue_handler lvalue_handler = { &count, &result };
  async_1_completion_arg(ioc, lvalue_handler);

  // No handlers can be called until run() is called.
  ASIO_CHECK(!ioc.stopped());
  ASIO_CHECK(count == 0);
  ASIO_CHECK(result == 0);

  ioc.run();

  // The run() call will not return until all work has finished.
  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);
  ASIO_CHECK(result == 42);
}

//------------------------------------------------------------------------------

ASIO_TEST_SUITE
(
  "compose",
  ASIO_TEST_CASE(compose_0_completion_args_test)
  ASIO_TEST_CASE(compose_1_completion_arg_test)
)
