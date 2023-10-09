//
// sender.cpp
// ~~~~~~~~~~
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
#include "asio/execution/sender.hpp"

#include "asio/error_code.hpp"
#include "../unit_test.hpp"

#if !defined(ASIO_NO_DEPRECATED)

namespace exec = asio::execution;

struct not_a_sender
{
};

struct sender_using_base :
  asio::execution::sender_base
{
  sender_using_base()
  {
  }
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

#if defined(ASIO_HAS_DEDUCED_EXECUTION_IS_TYPED_SENDER_TRAIT)

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

struct typed_sender
{
  template <
      template <typename...> class Tuple,
      template <typename...> class Variant>
  using value_types = Variant<Tuple<int>>;

  template <template <typename...> class Variant>
  using error_types = Variant<asio::error_code>;

  ASIO_STATIC_CONSTEXPR(bool, sends_done = true);

  typed_sender()
  {
  }

  template <typename R>
  operation_state connect(ASIO_MOVE_ARG(R) r) const
  {
    (void)r;
    return operation_state();
  }
};

namespace asio {
namespace traits {

#if !defined(ASIO_HAS_DEDUCED_CONNECT_MEMBER_TRAIT)

template <typename R>
struct connect_member<const typed_sender, R>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef operation_state result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_CONNECT_MEMBER_TRAIT)

} // namespace traits
} // namespace asio

#endif // defined(ASIO_HAS_DEDUCED_EXECUTION_IS_TYPED_SENDER_TRAIT)

template <typename T>
bool is_unspecialised(T*, ...)
{
  return false;
}

template <typename T>
bool is_unspecialised(T*,
    typename asio::void_type<
      typename exec::sender_traits<
        T>::asio_execution_sender_traits_base_is_unspecialised
    >::type*)
{
  return true;
}

void test_sender_traits()
{
  not_a_sender s1;
  ASIO_CHECK(is_unspecialised(&s1, static_cast<void*>(0)));

  sender_using_base s2;
  ASIO_CHECK(!is_unspecialised(&s2, static_cast<void*>(0)));

  executor s3;
  ASIO_CHECK(!is_unspecialised(&s3, static_cast<void*>(0)));

#if defined(ASIO_HAS_DEDUCED_EXECUTION_IS_TYPED_SENDER_TRAIT)
  typed_sender s4;
  ASIO_CHECK(!is_unspecialised(&s4, static_cast<void*>(0)));
#endif // defined(ASIO_HAS_DEDUCED_EXECUTION_IS_TYPED_SENDER_TRAIT)
}

void test_is_sender()
{
  ASIO_CHECK(!exec::is_sender<void>::value);
  ASIO_CHECK(!exec::is_sender<not_a_sender>::value);
  ASIO_CHECK(exec::is_sender<sender_using_base>::value);
  ASIO_CHECK(exec::is_sender<executor>::value);

#if defined(ASIO_HAS_DEDUCED_EXECUTION_IS_TYPED_SENDER_TRAIT)
  ASIO_CHECK(exec::is_sender<typed_sender>::value);
#endif // defined(ASIO_HAS_DEDUCED_EXECUTION_IS_TYPED_SENDER_TRAIT)
}

void test_is_typed_sender()
{
  ASIO_CHECK(!exec::is_typed_sender<void>::value);
  ASIO_CHECK(!exec::is_typed_sender<not_a_sender>::value);
  ASIO_CHECK(!exec::is_typed_sender<sender_using_base>::value);

#if defined(ASIO_HAS_DEDUCED_EXECUTION_IS_TYPED_SENDER_TRAIT)
  ASIO_CHECK(exec::is_typed_sender<executor>::value);
  ASIO_CHECK(exec::is_typed_sender<typed_sender>::value);
#endif // defined(ASIO_HAS_DEDUCED_EXECUTION_IS_TYPED_SENDER_TRAIT)
}

ASIO_TEST_SUITE
(
  "sender",
  ASIO_TEST_CASE(test_sender_traits)
  ASIO_TEST_CASE(test_is_sender)
  ASIO_TEST_CASE(test_is_typed_sender)
)

#else // !defined(ASIO_NO_DEPRECATED)

ASIO_TEST_SUITE
(
  "sender",
  ASIO_TEST_CASE(null_test)
)

#endif // !defined(ASIO_NO_DEPRECATED)
