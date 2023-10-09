//
// execute.cpp
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
#include "asio/execution/execute.hpp"
#include "asio/execution/sender.hpp"
#include "asio/execution/submit.hpp"

#include "asio/execution/invocable_archetype.hpp"
#include "../unit_test.hpp"

#if defined(ASIO_HAS_BOOST_BIND)
# include <boost/bind/bind.hpp>
#else // defined(ASIO_HAS_BOOST_BIND)
# include <functional>
#endif // defined(ASIO_HAS_BOOST_BIND)

#if !defined(ASIO_NO_DEPRECATED)

namespace exec = asio::execution;

struct no_execute
{
};

struct const_member_execute
{
  template <typename F>
  void execute(ASIO_MOVE_ARG(F) f) const
  {
    typename asio::decay<F>::type tmp(ASIO_MOVE_CAST(F)(f));
    tmp();
  }
};

#if !defined(ASIO_HAS_DEDUCED_EXECUTE_MEMBER_TRAIT)

namespace asio {
namespace traits {

template <typename F>
struct execute_member<const_member_execute, F>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef void result_type;
};

} // namespace traits
} // namespace asio

#endif // !defined(ASIO_HAS_DEDUCED_EXECUTE_MEMBER_TRAIT)

struct free_execute_const_executor
{
  template <typename F>
  friend void execute(const free_execute_const_executor&,
      ASIO_MOVE_ARG(F) f)
  {
    typename asio::decay<F>::type tmp(ASIO_MOVE_CAST(F)(f));
    tmp();
  }
};

#if !defined(ASIO_HAS_DEDUCED_EXECUTE_FREE_TRAIT)

namespace asio {
namespace traits {

template <typename F>
struct execute_free<free_execute_const_executor, F>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef void result_type;
};

} // namespace traits
} // namespace asio

#endif // !defined(ASIO_HAS_DEDUCED_EXECUTE_FREE_TRAIT)

#if defined(ASIO_HAS_MOVE)

// Support for rvalue references is required in order to use the execute
// customisation point with non-const member functions and free functions
// taking non-const arguments.

struct non_const_member_execute
{
  template <typename F>
  void execute(ASIO_MOVE_ARG(F) f)
  {
    typename asio::decay<F>::type tmp(ASIO_MOVE_CAST(F)(f));
    tmp();
  }
};

#if !defined(ASIO_HAS_DEDUCED_EXECUTE_MEMBER_TRAIT)

namespace asio {
namespace traits {

template <typename F>
struct execute_member<non_const_member_execute, F>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef void result_type;
};

template <typename F>
struct execute_member<const non_const_member_execute, F>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = false);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef void result_type;
};

template <typename F>
struct execute_member<const non_const_member_execute&, F>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = false);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef void result_type;
};

} // namespace traits
} // namespace asio

#endif // !defined(ASIO_HAS_DEDUCED_EXECUTE_MEMBER_TRAIT)

struct free_execute_non_const_executor
{
  template <typename F>
  friend void execute(free_execute_non_const_executor&,
      ASIO_MOVE_ARG(F) f)
  {
    typename asio::decay<F>::type tmp(ASIO_MOVE_CAST(F)(f));
    tmp();
  }
};

#if !defined(ASIO_HAS_DEDUCED_EXECUTE_FREE_TRAIT)

namespace asio {
namespace traits {

template <typename F>
struct execute_free<free_execute_non_const_executor, F>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef void result_type;
};

template <typename F>
struct execute_free<const free_execute_non_const_executor, F>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = false);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef void result_type;
};

template <typename F>
struct execute_free<const free_execute_non_const_executor&, F>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = false);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef void result_type;
};

} // namespace traits
} // namespace asio

#endif // !defined(ASIO_HAS_DEDUCED_EXECUTE_FREE_TRAIT)

#endif // defined(ASIO_HAS_MOVE)

struct operation_state
{
  void start() ASIO_NOEXCEPT
  {
  }
};

namespace asio {
namespace traits {

#if !defined(ASIO_HAS_DEDUCED_START_MEMBER_TRAIT)

template <>
struct start_member<operation_state>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);
  typedef void result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_START_MEMBER_TRAIT)

} // namespace traits
} // namespace asio

struct sender : exec::sender_base
{
  sender()
  {
  }

  template <typename R>
  operation_state connect(ASIO_MOVE_ARG(R) r) const
  {
    (void)r;
    return operation_state();
  }

  template <typename R>
  void submit(ASIO_MOVE_ARG(R) r) const
  {
    exec::set_value(ASIO_MOVE_CAST(R)(r));
  }
};

namespace asio {
namespace traits {

#if !defined(ASIO_HAS_DEDUCED_CONNECT_MEMBER_TRAIT)

template <typename R>
struct connect_member<const sender, R>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef operation_state result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_CONNECT_MEMBER_TRAIT)

#if !defined(ASIO_HAS_DEDUCED_SUBMIT_MEMBER_TRAIT)

template <typename R>
struct submit_member<const sender, R>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef void result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_SUBMIT_MEMBER_TRAIT)

} // namespace traits
} // namespace asio

void test_can_execute()
{
  ASIO_CONSTEXPR bool b1 = exec::can_execute<
      no_execute&, exec::invocable_archetype>::value;
  ASIO_CHECK(b1 == false);

  ASIO_CONSTEXPR bool b2 = exec::can_execute<
      const no_execute&, exec::invocable_archetype>::value;
  ASIO_CHECK(b2 == false);

  ASIO_CONSTEXPR bool b3 = exec::can_execute<
      const_member_execute&, exec::invocable_archetype>::value;
  ASIO_CHECK(b3 == true);

  ASIO_CONSTEXPR bool b4 = exec::can_execute<
      const const_member_execute&, exec::invocable_archetype>::value;
  ASIO_CHECK(b4 == true);

  ASIO_CONSTEXPR bool b5 = exec::can_execute<
      free_execute_const_executor&, exec::invocable_archetype>::value;
  ASIO_CHECK(b5 == true);

  ASIO_CONSTEXPR bool b6 = exec::can_execute<
      const free_execute_const_executor&, exec::invocable_archetype>::value;
  ASIO_CHECK(b6 == true);

#if defined(ASIO_HAS_MOVE)
  ASIO_CONSTEXPR bool b7 = exec::can_execute<
      non_const_member_execute&, exec::invocable_archetype>::value;
  ASIO_CHECK(b7 == true);

  ASIO_CONSTEXPR bool b8 = exec::can_execute<
      const non_const_member_execute&, exec::invocable_archetype>::value;
  ASIO_CHECK(b8 == false);

  ASIO_CONSTEXPR bool b9 = exec::can_execute<
      free_execute_non_const_executor&, exec::invocable_archetype>::value;
  ASIO_CHECK(b9 == true);

  ASIO_CONSTEXPR bool b10 = exec::can_execute<
      const free_execute_non_const_executor&, exec::invocable_archetype>::value;
  ASIO_CHECK(b10 == false);
#endif // defined(ASIO_HAS_MOVE)

  ASIO_CONSTEXPR bool b11 = exec::can_execute<
      sender&, exec::invocable_archetype>::value;
  ASIO_CHECK(b11 == true);

  ASIO_CONSTEXPR bool b12 = exec::can_execute<
      const sender&, exec::invocable_archetype>::value;
  ASIO_CHECK(b12 == true);
}

void increment(int* count)
{
  ++(*count);
}

void test_execute()
{
#if defined(ASIO_HAS_BOOST_BIND)
  namespace bindns = boost;
#else // defined(ASIO_HAS_BOOST_BIND)
  namespace bindns = std;
#endif // defined(ASIO_HAS_BOOST_BIND)

  int count = 0;
  const_member_execute ex1 = {};
  exec::execute(ex1, bindns::bind(&increment, &count));
  ASIO_CHECK(count == 1);

  count = 0;
  const const_member_execute ex2 = {};
  exec::execute(ex2, bindns::bind(&increment, &count));
  ASIO_CHECK(count == 1);

  count = 0;
  exec::execute(const_member_execute(), bindns::bind(&increment, &count));
  ASIO_CHECK(count == 1);

  count = 0;
  free_execute_const_executor ex3 = {};
  exec::execute(ex3, bindns::bind(&increment, &count));
  ASIO_CHECK(count == 1);

  count = 0;
  const free_execute_const_executor ex4 = {};
  exec::execute(ex4, bindns::bind(&increment, &count));
  ASIO_CHECK(count == 1);

  count = 0;
  exec::execute(free_execute_const_executor(),
      bindns::bind(&increment, &count));
  ASIO_CHECK(count == 1);

#if defined(ASIO_HAS_MOVE)
  count = 0;
  non_const_member_execute ex5 = {};
  exec::execute(ex5, bindns::bind(&increment, &count));
  ASIO_CHECK(count == 1);

  count = 0;
  free_execute_non_const_executor ex6 = {};
  exec::execute(ex6, bindns::bind(&increment, &count));
  ASIO_CHECK(count == 1);
#endif // defined(ASIO_HAS_MOVE)

  count = 0;
  sender ex7;
  exec::execute(ex3, bindns::bind(&increment, &count));
  ASIO_CHECK(count == 1);

  count = 0;
  const sender ex8;
  exec::execute(ex4, bindns::bind(&increment, &count));
  ASIO_CHECK(count == 1);
}

ASIO_TEST_SUITE
(
  "execute",
  ASIO_TEST_CASE(test_can_execute)
  ASIO_TEST_CASE(test_execute)
)

#else // !defined(ASIO_NO_DEPRECATED)

ASIO_TEST_SUITE
(
  "execute",
  ASIO_TEST_CASE(null_test)
)

#endif // !defined(ASIO_NO_DEPRECATED)
