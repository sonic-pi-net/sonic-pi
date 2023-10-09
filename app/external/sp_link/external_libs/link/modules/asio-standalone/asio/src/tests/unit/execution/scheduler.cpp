//
// scheduler.cpp
// ~~~~~~~~~~~~~
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
#include "asio/execution/scheduler.hpp"

#include "../unit_test.hpp"

#if !defined(ASIO_NO_DEPRECATED)

namespace exec = asio::execution;

struct not_a_scheduler
{
};

struct executor
{
  executor()
  {
  }

  executor(const executor&) ASIO_NOEXCEPT
  {
  }

#if defined(ASIO_HAS_MOVE)
  executor(executor&&) ASIO_NOEXCEPT
  {
  }
#endif // defined(ASIO_HAS_MOVE)

  template <typename F>
  void execute(ASIO_MOVE_ARG(F) f) const ASIO_NOEXCEPT
  {
    (void)f;
  }

  bool operator==(const executor&) const ASIO_NOEXCEPT
  {
    return true;
  }

  bool operator!=(const executor&) const ASIO_NOEXCEPT
  {
    return false;
  }
};

namespace asio {
namespace traits {

#if !defined(ASIO_HAS_DEDUCED_EXECUTE_MEMBER_TRAIT)

template <typename F>
struct execute_member<executor, F>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);
  typedef void result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_SET_ERROR_MEMBER_TRAIT)
#if !defined(ASIO_HAS_DEDUCED_EQUALITY_COMPARABLE_TRAIT)

template <>
struct equality_comparable<executor>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);
};

#endif // !defined(ASIO_HAS_DEDUCED_EQUALITY_COMPARABLE_TRAIT)

} // namespace traits
} // namespace asio

void test_is_scheduler()
{
  ASIO_CHECK(!exec::is_scheduler<void>::value);
  ASIO_CHECK(!exec::is_scheduler<not_a_scheduler>::value);
  ASIO_CHECK(exec::is_scheduler<executor>::value);
}

ASIO_TEST_SUITE
(
  "scheduler",
  ASIO_TEST_CASE(test_is_scheduler)
)

#else // !defined(ASIO_NO_DEPRECATED)

ASIO_TEST_SUITE
(
  "scheduler",
  ASIO_TEST_CASE(null_test)
)

#endif // !defined(ASIO_NO_DEPRECATED)
