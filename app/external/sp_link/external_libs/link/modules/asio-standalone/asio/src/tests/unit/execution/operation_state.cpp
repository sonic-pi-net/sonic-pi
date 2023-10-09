//
// operation_state.cpp
// ~~~~~~~~~~~~~~~~~~~
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
#include "asio/execution/operation_state.hpp"

#include <string>
#include "asio/error_code.hpp"
#include "../unit_test.hpp"

#if !defined(ASIO_NO_DEPRECATED)

struct not_an_operation_state_1
{
};

struct not_an_operation_state_2
{
  void start()
  {
  }
};

namespace asio {
namespace traits {

#if !defined(ASIO_HAS_DEDUCED_START_MEMBER_TRAIT)

template <>
struct start_member<not_an_operation_state_2>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef void result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_START_MEMBER_TRAIT)

} // namespace traits
} // namespace asio

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

void is_operation_state_test()
{
  ASIO_CHECK((
      !asio::execution::is_operation_state<
        void
      >::value));

  ASIO_CHECK((
      !asio::execution::is_operation_state<
        not_an_operation_state_1
      >::value));

  ASIO_CHECK((
      !asio::execution::is_operation_state<
        not_an_operation_state_2
      >::value));

  ASIO_CHECK((
      asio::execution::is_operation_state<
        operation_state
      >::value));
}

ASIO_TEST_SUITE
(
  "operation_state",
  ASIO_TEST_CASE(is_operation_state_test)
)

#else // !defined(ASIO_NO_DEPRECATED)

ASIO_TEST_SUITE
(
  "operation_state",
  ASIO_TEST_CASE(null_test)
)

#endif // !defined(ASIO_NO_DEPRECATED)
