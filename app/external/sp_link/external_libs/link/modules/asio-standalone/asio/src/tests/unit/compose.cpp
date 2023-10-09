//
// compose.cpp
// ~~~~~~~~~~~
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
#include "asio/compose.hpp"

#include "unit_test.hpp"

#include "asio/bind_cancellation_slot.hpp"
#include "asio/cancellation_signal.hpp"
#include "asio/io_context.hpp"
#include "asio/post.hpp"
#include "asio/system_timer.hpp"

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

typedef asio::enable_terminal_cancellation default_filter;

template <typename CancellationFilter>
class impl_cancellable
{
public:
  explicit impl_cancellable(CancellationFilter cancellation_filter,
      asio::system_timer& timer)
    : cancellation_filter_(cancellation_filter),
      timer_(timer),
      state_(starting)
  {
  }

  template <typename Self>
  void operator()(Self& self,
      const asio::error_code& ec = asio::error_code())
  {
    switch (state_)
    {
    case starting:
      if (!asio::is_same<CancellationFilter, default_filter>::value)
        self.reset_cancellation_state(cancellation_filter_);
      state_ = waiting;
      timer_.expires_after(asio::chrono::milliseconds(100));
      timer_.async_wait(ASIO_MOVE_CAST(Self)(self));
      break;
    case waiting:
      self.complete(!ec);
      break;
    default:
      break;
    }
  }

private:
  CancellationFilter cancellation_filter_;
  asio::system_timer& timer_;
  enum { starting, waiting } state_;
};

template <typename CancellationFilter, typename CompletionToken>
ASIO_INITFN_RESULT_TYPE(CompletionToken, void(bool))
async_cancellable(CancellationFilter cancellation_filter,
    asio::system_timer& timer,
    ASIO_MOVE_ARG(CompletionToken) token)
{
  return asio::async_compose<CompletionToken, void(bool)>(
      impl_cancellable<CancellationFilter>(cancellation_filter, timer), token);
}

void compose_partial_cancellation_handler(
    int* count, bool* result_out, bool result)
{
  ++(*count);
  *result_out = result;
}

void compose_default_cancellation_test()
{
#if defined(ASIO_HAS_BOOST_BIND)
  namespace bindns = boost;
#else // defined(ASIO_HAS_BOOST_BIND)
  namespace bindns = std;
#endif // defined(ASIO_HAS_BOOST_BIND)
  using bindns::placeholders::_1;

  asio::io_context ioc;
  asio::system_timer timer(ioc);
  asio::cancellation_signal signal;
  int count = 0;
  bool result = false;

  async_cancellable(default_filter(), timer,
      bindns::bind(&compose_partial_cancellation_handler,
        &count, &result, _1));

  ioc.run();

  // No cancellation, operation completes successfully.

  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);
  ASIO_CHECK(result == true);

  ioc.restart();
  count = 0;
  result = 0;

  async_cancellable(default_filter(), timer,
      asio::bind_cancellation_slot(signal.slot(),
        bindns::bind(&compose_partial_cancellation_handler,
        &count, &result, _1)));

  // Total cancellation unsupported. Operation completes successfully.
  signal.emit(asio::cancellation_type::total);

  ioc.run();

  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);
  ASIO_CHECK(result == true);

  ioc.restart();
  count = 0;
  result = 0;

  async_cancellable(default_filter(), timer,
      asio::bind_cancellation_slot(signal.slot(),
        bindns::bind(&compose_partial_cancellation_handler,
        &count, &result, _1)));

  // Partial cancellation unsupported. Operation completes successfully.
  signal.emit(asio::cancellation_type::partial);

  ioc.run();

  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);
  ASIO_CHECK(result == true);

  ioc.restart();
  count = 0;
  result = 0;

  async_cancellable(default_filter(), timer,
      asio::bind_cancellation_slot(signal.slot(),
        bindns::bind(&compose_partial_cancellation_handler,
        &count, &result, _1)));

  // Terminal cancellation works. Operation completes with failure.
  signal.emit(asio::cancellation_type::terminal);

  ioc.run();

  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);
  ASIO_CHECK(result == false);
}

void compose_partial_cancellation_test()
{
#if defined(ASIO_HAS_BOOST_BIND)
  namespace bindns = boost;
#else // defined(ASIO_HAS_BOOST_BIND)
  namespace bindns = std;
#endif // defined(ASIO_HAS_BOOST_BIND)
  using bindns::placeholders::_1;

  asio::io_context ioc;
  asio::system_timer timer(ioc);
  asio::cancellation_signal signal;
  int count = 0;
  bool result = false;

  async_cancellable(asio::enable_partial_cancellation(), timer,
      bindns::bind(&compose_partial_cancellation_handler,
        &count, &result, _1));

  ioc.run();

  // No cancellation, operation completes successfully.

  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);
  ASIO_CHECK(result == true);

  ioc.restart();
  count = 0;
  result = 0;

  async_cancellable(asio::enable_partial_cancellation(), timer,
      asio::bind_cancellation_slot(signal.slot(),
        bindns::bind(&compose_partial_cancellation_handler,
        &count, &result, _1)));

  // Total cancellation unsupported. Operation completes successfully.
  signal.emit(asio::cancellation_type::total);

  ioc.run();

  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);
  ASIO_CHECK(result == true);

  ioc.restart();
  count = 0;
  result = 0;

  async_cancellable(asio::enable_partial_cancellation(), timer,
      asio::bind_cancellation_slot(signal.slot(),
        bindns::bind(&compose_partial_cancellation_handler,
        &count, &result, _1)));

  // Partial cancellation works. Operation completes with failure.
  signal.emit(asio::cancellation_type::partial);

  ioc.run();

  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);
  ASIO_CHECK(result == false);

  ioc.restart();
  count = 0;
  result = 0;

  async_cancellable(asio::enable_partial_cancellation(), timer,
      asio::bind_cancellation_slot(signal.slot(),
        bindns::bind(&compose_partial_cancellation_handler,
        &count, &result, _1)));

  // Terminal cancellation works. Operation completes with failure.
  signal.emit(asio::cancellation_type::terminal);

  ioc.run();

  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);
  ASIO_CHECK(result == false);
}

void compose_total_cancellation_test()
{
#if defined(ASIO_HAS_BOOST_BIND)
  namespace bindns = boost;
#else // defined(ASIO_HAS_BOOST_BIND)
  namespace bindns = std;
#endif // defined(ASIO_HAS_BOOST_BIND)
  using bindns::placeholders::_1;

  asio::io_context ioc;
  asio::system_timer timer(ioc);
  asio::cancellation_signal signal;
  int count = 0;
  bool result = false;

  async_cancellable(asio::enable_total_cancellation(), timer,
      bindns::bind(&compose_partial_cancellation_handler,
        &count, &result, _1));

  ioc.run();

  // No cancellation, operation completes successfully.

  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);
  ASIO_CHECK(result == true);

  ioc.restart();
  count = 0;
  result = 0;

  async_cancellable(asio::enable_total_cancellation(), timer,
      asio::bind_cancellation_slot(signal.slot(),
        bindns::bind(&compose_partial_cancellation_handler,
        &count, &result, _1)));

  // Total cancellation works. Operation completes with failure.
  signal.emit(asio::cancellation_type::total);

  ioc.run();

  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);
  ASIO_CHECK(result == false);

  ioc.restart();
  count = 0;
  result = 0;

  async_cancellable(asio::enable_total_cancellation(), timer,
      asio::bind_cancellation_slot(signal.slot(),
        bindns::bind(&compose_partial_cancellation_handler,
        &count, &result, _1)));

  // Partial cancellation works. Operation completes with failure.
  signal.emit(asio::cancellation_type::partial);

  ioc.run();

  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);
  ASIO_CHECK(result == false);

  ioc.restart();
  count = 0;
  result = 0;

  async_cancellable(asio::enable_total_cancellation(), timer,
      asio::bind_cancellation_slot(signal.slot(),
        bindns::bind(&compose_partial_cancellation_handler,
        &count, &result, _1)));

  // Terminal cancellation works. Operation completes with failure.
  signal.emit(asio::cancellation_type::terminal);

  ioc.run();

  ASIO_CHECK(ioc.stopped());
  ASIO_CHECK(count == 1);
  ASIO_CHECK(result == false);
}

//------------------------------------------------------------------------------

ASIO_TEST_SUITE
(
  "compose",
  ASIO_TEST_CASE(compose_0_completion_args_test)
  ASIO_TEST_CASE(compose_1_completion_arg_test)
  ASIO_TEST_CASE(compose_default_cancellation_test)
  ASIO_TEST_CASE(compose_partial_cancellation_test)
  ASIO_TEST_CASE(compose_total_cancellation_test)
)
