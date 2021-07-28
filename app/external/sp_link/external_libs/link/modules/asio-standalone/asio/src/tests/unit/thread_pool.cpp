//
// thread_pool.cpp
// ~~~~~~~~~~~~~~~
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
#include "asio/thread_pool.hpp"

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

void increment(int* count)
{
  ++(*count);
}

void decrement_to_zero(thread_pool* pool, int* count)
{
  if (*count > 0)
  {
    --(*count);

    int before_value = *count;
    asio::post(*pool, bindns::bind(decrement_to_zero, pool, count));

    // Handler execution cannot nest, so count value should remain unchanged.
    ASIO_CHECK(*count == before_value);
  }
}

void nested_decrement_to_zero(thread_pool* pool, int* count)
{
  if (*count > 0)
  {
    --(*count);

    asio::dispatch(*pool,
        bindns::bind(nested_decrement_to_zero, pool, count));

    // Handler execution is nested, so count value should now be zero.
    ASIO_CHECK(*count == 0);
  }
}

void thread_pool_test()
{
  thread_pool pool(1);

  int count1 = 0;
  asio::post(pool, bindns::bind(increment, &count1));

  int count2 = 10;
  asio::post(pool, bindns::bind(decrement_to_zero, &pool, &count2));

  int count3 = 10;
  asio::post(pool, bindns::bind(nested_decrement_to_zero, &pool, &count3));

  pool.wait();

  ASIO_CHECK(count1 == 1);
  ASIO_CHECK(count2 == 0);
  ASIO_CHECK(count3 == 0);
}

class test_service : public asio::execution_context::service
{
public:
  typedef test_service key_type;

  test_service(asio::execution_context& ctx)
    : asio::execution_context::service(ctx)
  {
  }

private:
  virtual void shutdown() {}
};

void thread_pool_service_test()
{
  asio::thread_pool pool1(1);
  asio::thread_pool pool2(1);
  asio::thread_pool pool3(1);

  // Implicit service registration.

  asio::use_service<test_service>(pool1);

  ASIO_CHECK(asio::has_service<test_service>(pool1));

  test_service* svc1 = new test_service(pool1);
  try
  {
    asio::add_service(pool1, svc1);
    ASIO_ERROR("add_service did not throw");
  }
  catch (asio::service_already_exists&)
  {
  }
  delete svc1;

  // Explicit service registration.

  test_service& svc2 = asio::make_service<test_service>(pool2);

  ASIO_CHECK(asio::has_service<test_service>(pool2));
  ASIO_CHECK(&asio::use_service<test_service>(pool2) == &svc2);

  test_service* svc3 = new test_service(pool2);
  try
  {
    asio::add_service(pool2, svc3);
    ASIO_ERROR("add_service did not throw");
  }
  catch (asio::service_already_exists&)
  {
  }
  delete svc3;

  // Explicit registration with invalid owner.

  test_service* svc4 = new test_service(pool2);
  try
  {
    asio::add_service(pool3, svc4);
    ASIO_ERROR("add_service did not throw");
  }
  catch (asio::invalid_service_owner&)
  {
  }
  delete svc4;

  ASIO_CHECK(!asio::has_service<test_service>(pool3));
}

void thread_pool_executor_query_test()
{
  thread_pool pool(1);

  ASIO_CHECK(
      &asio::query(pool.executor(),
        asio::execution::context)
      == &pool);

  ASIO_CHECK(
      asio::query(pool.executor(),
        asio::execution::blocking)
      == asio::execution::blocking.possibly);

  ASIO_CHECK(
      asio::query(pool.executor(),
        asio::execution::blocking.possibly)
      == asio::execution::blocking.possibly);

  ASIO_CHECK(
      asio::query(pool.executor(),
        asio::execution::outstanding_work)
      == asio::execution::outstanding_work.untracked);

  ASIO_CHECK(
      asio::query(pool.executor(),
        asio::execution::outstanding_work.untracked)
      == asio::execution::outstanding_work.untracked);

  ASIO_CHECK(
      asio::query(pool.executor(),
        asio::execution::relationship)
      == asio::execution::relationship.fork);

  ASIO_CHECK(
      asio::query(pool.executor(),
        asio::execution::relationship.fork)
      == asio::execution::relationship.fork);

  ASIO_CHECK(
      asio::query(pool.executor(),
        asio::execution::bulk_guarantee)
      == asio::execution::bulk_guarantee.parallel);

  ASIO_CHECK(
      asio::query(pool.executor(),
        asio::execution::mapping)
      == asio::execution::mapping.thread);

  ASIO_CHECK(
      asio::query(pool.executor(),
        asio::execution::allocator)
      == std::allocator<void>());

  ASIO_CHECK(
      asio::query(pool.executor(),
        asio::execution::occupancy)
      == 1);
}

void thread_pool_executor_execute_test()
{
  int count = 0;
  thread_pool pool(1);

  asio::execution::execute(pool.executor(),
      bindns::bind(increment, &count));

  asio::execution::execute(
      asio::require(pool.executor(),
        asio::execution::blocking.possibly),
      bindns::bind(increment, &count));

  asio::execution::execute(
      asio::require(pool.executor(),
        asio::execution::blocking.always),
      bindns::bind(increment, &count));

  asio::execution::execute(
      asio::require(pool.executor(),
        asio::execution::blocking.never),
      bindns::bind(increment, &count));

  asio::execution::execute(
      asio::require(pool.executor(),
        asio::execution::blocking.never,
        asio::execution::outstanding_work.tracked),
      bindns::bind(increment, &count));

  asio::execution::execute(
      asio::require(pool.executor(),
        asio::execution::blocking.never,
        asio::execution::outstanding_work.untracked),
      bindns::bind(increment, &count));

  asio::execution::execute(
      asio::require(pool.executor(),
        asio::execution::blocking.never,
        asio::execution::outstanding_work.untracked,
        asio::execution::relationship.fork),
      bindns::bind(increment, &count));

  asio::execution::execute(
      asio::require(pool.executor(),
        asio::execution::blocking.never,
        asio::execution::outstanding_work.untracked,
        asio::execution::relationship.continuation),
      bindns::bind(increment, &count));

  asio::execution::execute(
      asio::prefer(
        asio::require(pool.executor(),
          asio::execution::blocking.never,
          asio::execution::outstanding_work.untracked,
          asio::execution::relationship.continuation),
        asio::execution::allocator(std::allocator<void>())),
      bindns::bind(increment, &count));

  asio::execution::execute(
      asio::prefer(
        asio::require(pool.executor(),
          asio::execution::blocking.never,
          asio::execution::outstanding_work.untracked,
          asio::execution::relationship.continuation),
        asio::execution::allocator),
      bindns::bind(increment, &count));

  pool.wait();

  ASIO_CHECK(count == 10);
}

struct receiver
{
  int* count_;

  receiver(int* count)
    : count_(count)
  {
  }

  receiver(const receiver& other) ASIO_NOEXCEPT
    : count_(other.count_)
  {
  }

#if defined(ASIO_HAS_MOVE)
  receiver(receiver&& other) ASIO_NOEXCEPT
    : count_(other.count_)
  {
    other.count_ = 0;
  }
#endif // defined(ASIO_HAS_MOVE)

  void set_value() ASIO_NOEXCEPT
  {
    ++(*count_);
  }

  template <typename E>
  void set_error(ASIO_MOVE_ARG(E) e) ASIO_NOEXCEPT
  {
    (void)e;
  }

  void set_done() ASIO_NOEXCEPT
  {
  }
};

namespace asio {
namespace traits {

#if !defined(ASIO_HAS_DEDUCED_SET_VALUE_MEMBER_TRAIT)

template <>
struct set_value_member<receiver, void()>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);
  typedef void result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_SET_VALUE_MEMBER_TRAIT)

#if !defined(ASIO_HAS_DEDUCED_SET_ERROR_MEMBER_TRAIT)

template <typename E>
struct set_error_member<receiver, E>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);
  typedef void result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_SET_ERROR_MEMBER_TRAIT)

#if !defined(ASIO_HAS_DEDUCED_SET_DONE_MEMBER_TRAIT)

template <>
struct set_done_member<receiver>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);
  typedef void result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_SET_DONE_MEMBER_TRAIT)

} // namespace traits
} // namespace asio

void thread_pool_scheduler_test()
{
  int count = 0;
  receiver r(&count);
  thread_pool pool(1);

  asio::execution::submit(
    asio::execution::schedule(pool.scheduler()), r);

  asio::execution::submit(
      asio::require(
        asio::execution::schedule(pool.executor()),
        asio::execution::blocking.possibly), r);

  asio::execution::submit(
      asio::require(
        asio::execution::schedule(pool.executor()),
        asio::execution::blocking.always), r);

  asio::execution::submit(
      asio::require(
        asio::execution::schedule(pool.executor()),
        asio::execution::blocking.never), r);

  asio::execution::submit(
      asio::require(
        asio::execution::schedule(pool.executor()),
        asio::execution::blocking.never,
        asio::execution::outstanding_work.tracked), r);

  asio::execution::submit(
      asio::require(
        asio::execution::schedule(pool.executor()),
        asio::execution::blocking.never,
        asio::execution::outstanding_work.untracked), r);

  asio::execution::submit(
      asio::require(
        asio::execution::schedule(pool.executor()),
        asio::execution::blocking.never,
        asio::execution::outstanding_work.untracked,
        asio::execution::relationship.fork), r);

  asio::execution::submit(
      asio::require(
        asio::execution::schedule(pool.executor()),
        asio::execution::blocking.never,
        asio::execution::outstanding_work.untracked,
        asio::execution::relationship.continuation), r);

  asio::execution::submit(
      asio::prefer(
        asio::require(
          asio::execution::schedule(pool.executor()),
          asio::execution::blocking.never,
          asio::execution::outstanding_work.untracked,
          asio::execution::relationship.continuation),
        asio::execution::allocator(std::allocator<void>())), r);

  asio::execution::submit(
      asio::prefer(
        asio::require(
          asio::execution::schedule(pool.executor()),
          asio::execution::blocking.never,
          asio::execution::outstanding_work.untracked,
          asio::execution::relationship.continuation),
        asio::execution::allocator), r);

  pool.wait();

  ASIO_CHECK(count == 10);
}

void thread_pool_executor_bulk_execute_test()
{
  int count = 0;
  thread_pool pool(1);

  pool.executor().bulk_execute(
      bindns::bind(increment, &count), 2);

  asio::require(pool.executor(),
    asio::execution::blocking.possibly).bulk_execute(
      bindns::bind(increment, &count), 2);

  asio::require(pool.executor(),
    asio::execution::blocking.always).bulk_execute(
      bindns::bind(increment, &count), 2);

  asio::require(pool.executor(),
    asio::execution::blocking.never).bulk_execute(
      bindns::bind(increment, &count), 2);

  asio::require(pool.executor(),
    asio::execution::blocking.never,
    asio::execution::outstanding_work.tracked).bulk_execute(
      bindns::bind(increment, &count), 2);

  asio::require(pool.executor(),
    asio::execution::blocking.never,
    asio::execution::outstanding_work.untracked).bulk_execute(
      bindns::bind(increment, &count), 2);

  asio::require(pool.executor(),
    asio::execution::blocking.never,
    asio::execution::outstanding_work.untracked,
    asio::execution::relationship.fork).bulk_execute(
      bindns::bind(increment, &count), 2);

  asio::require(pool.executor(),
    asio::execution::blocking.never,
    asio::execution::outstanding_work.untracked,
    asio::execution::relationship.continuation).bulk_execute(
      bindns::bind(increment, &count), 2);

  asio::prefer(
    asio::require(pool.executor(),
      asio::execution::blocking.never,
      asio::execution::outstanding_work.untracked,
      asio::execution::relationship.continuation),
    asio::execution::allocator(std::allocator<void>())).bulk_execute(
      bindns::bind(increment, &count), 2);

  asio::prefer(
    asio::require(pool.executor(),
      asio::execution::blocking.never,
      asio::execution::outstanding_work.untracked,
      asio::execution::relationship.continuation),
    asio::execution::allocator).bulk_execute(
      bindns::bind(increment, &count), 2);

  pool.wait();

  ASIO_CHECK(count == 20);
}

ASIO_TEST_SUITE
(
  "thread_pool",
  ASIO_TEST_CASE(thread_pool_test)
  ASIO_TEST_CASE(thread_pool_service_test)
  ASIO_TEST_CASE(thread_pool_executor_query_test)
  ASIO_TEST_CASE(thread_pool_executor_execute_test)
  ASIO_TEST_CASE(thread_pool_executor_bulk_execute_test)
  ASIO_TEST_CASE(thread_pool_scheduler_test)
)
