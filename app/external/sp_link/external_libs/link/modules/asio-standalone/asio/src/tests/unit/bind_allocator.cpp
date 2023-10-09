//
// bind_allocator.cpp
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
#include "asio/bind_allocator.hpp"

#include "asio/io_context.hpp"
#include "asio/steady_timer.hpp"
#include "unit_test.hpp"

#if defined(ASIO_HAS_BOOST_DATE_TIME)
# include "asio/deadline_timer.hpp"
#else // defined(ASIO_HAS_BOOST_DATE_TIME)
# include "asio/steady_timer.hpp"
#endif // defined(ASIO_HAS_BOOST_DATE_TIME)

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

#if defined(ASIO_HAS_BOOST_DATE_TIME)
typedef deadline_timer timer;
namespace chronons = boost::posix_time;
#elif defined(ASIO_HAS_CHRONO)
typedef steady_timer timer;
namespace chronons = asio::chrono;
#endif // defined(ASIO_HAS_BOOST_DATE_TIME)

template <typename T>
class test_allocator
{
public:
  typedef T value_type;

  explicit test_allocator(int* allocations)
    : allocations_(allocations)
  {
  }

  template <typename U>
  test_allocator(const test_allocator<U>& other)
    : allocations_(other.allocations_)
  {
  }

  template <typename U>
  struct rebind
  {
    typedef test_allocator<U> other;
  };

  bool operator==(const test_allocator&) const
  {
    return true;
  }

  bool operator!=(const test_allocator&) const
  {
    return false;
  }

  T* allocate(std::size_t n) const
  {
    ++(*allocations_);
    return static_cast<T*>(::operator new(sizeof(T) * n));
  }

  void deallocate(T* p, std::size_t /*n*/) const
  {
    --(*allocations_);
    ::operator delete(p);
  }

//private:
  int* allocations_;
};

void increment(int* count)
{
  ++(*count);
}

void bind_allocator_to_function_object_test()
{
  io_context ioc;

  int count = 0;
  int allocations = 0;

  timer t(ioc, chronons::seconds(1));
  t.async_wait(
      bind_allocator(
        test_allocator<int>(&allocations),
        bindns::bind(&increment, &count)));

  ASIO_CHECK(count == 0);
  ASIO_CHECK(allocations == 1);

  ioc.run();

  ASIO_CHECK(count == 1);
  ASIO_CHECK(allocations == 0);
}

struct incrementer_token_v1
{
  explicit incrementer_token_v1(int* c) : count(c) {}
  int* count;
};

struct incrementer_handler_v1
{
  explicit incrementer_handler_v1(incrementer_token_v1 t) : count(t.count) {}
  void operator()(asio::error_code){ increment(count); }
  int* count;
};

namespace asio {

template <>
class async_result<incrementer_token_v1, void(asio::error_code)>
{
public:
  typedef incrementer_handler_v1 completion_handler_type;
  typedef void return_type;
  explicit async_result(completion_handler_type&) {}
  return_type get() {}
};

} // namespace asio

void bind_allocator_to_completion_token_v1_test()
{
  io_context ioc;

  int count = 0;
  int allocations = 0;

  timer t(ioc, chronons::seconds(1));
  t.async_wait(
      bind_allocator(
        test_allocator<int>(&allocations),
        incrementer_token_v1(&count)));

  ASIO_CHECK(count == 0);
  ASIO_CHECK(allocations == 1);

  ioc.run();

  ASIO_CHECK(count == 1);
  ASIO_CHECK(allocations == 0);
}

struct incrementer_token_v2
{
  explicit incrementer_token_v2(int* c) : count(c) {}
  int* count;
};

namespace asio {

template <>
class async_result<incrementer_token_v2, void(asio::error_code)>
{
public:
  typedef void return_type;

#if defined(ASIO_HAS_VARIADIC_TEMPLATES)

  template <typename Initiation, typename... Args>
  static void initiate(Initiation initiation,
      incrementer_token_v2 token, ASIO_MOVE_ARG(Args)... args)
  {
    initiation(bindns::bind(&increment, token.count),
        ASIO_MOVE_CAST(Args)(args)...);
  }

#else // defined(ASIO_HAS_VARIADIC_TEMPLATES)

  template <typename Initiation>
  static void initiate(Initiation initiation, incrementer_token_v2 token)
  {
    initiation(bindns::bind(&increment, token.count));
  }

#define ASIO_PRIVATE_INITIATE_DEF(n) \
  template <typename Initiation, ASIO_VARIADIC_TPARAMS(n)> \
  static return_type initiate(Initiation initiation, \
      incrementer_token_v2 token, ASIO_VARIADIC_MOVE_PARAMS(n)) \
  { \
    initiation(bindns::bind(&increment, token.count), \
        ASIO_VARIADIC_MOVE_ARGS(n)); \
  } \
  /**/
  ASIO_VARIADIC_GENERATE(ASIO_PRIVATE_INITIATE_DEF)
#undef ASIO_PRIVATE_INITIATE_DEF

#endif // defined(ASIO_HAS_VARIADIC_TEMPLATES)
};

} // namespace asio

void bind_allocator_to_completion_token_v2_test()
{
  io_context ioc;

  int count = 0;
  int allocations = 0;

  timer t(ioc, chronons::seconds(1));
  t.async_wait(
      bind_allocator(
        test_allocator<int>(&allocations),
        incrementer_token_v2(&count)));

  ASIO_CHECK(count == 0);
  ASIO_CHECK(allocations == 1);

  ioc.run();

  ASIO_CHECK(count == 1);
  ASIO_CHECK(allocations == 0);
}

ASIO_TEST_SUITE
(
  "bind_allocator",
  ASIO_TEST_CASE(bind_allocator_to_function_object_test)
  ASIO_TEST_CASE(bind_allocator_to_completion_token_v1_test)
  ASIO_TEST_CASE(bind_allocator_to_completion_token_v2_test)
)
