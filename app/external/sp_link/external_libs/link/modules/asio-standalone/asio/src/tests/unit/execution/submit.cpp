//
// submit.cpp
// ~~~~~~~~~~
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
#include "asio/execution/submit.hpp"

#include "asio/error_code.hpp"
#include "../unit_test.hpp"

namespace exec = asio::execution;

static int call_count = 0;

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

struct no_submit_1
{
};

struct no_submit_2 : exec::sender_base
{
};

struct no_submit_3
{
  template <typename R>
  void submit(ASIO_MOVE_ARG(R) r)
  {
    (void)r;
  }
};

#if !defined(ASIO_HAS_DEDUCED_SUBMIT_MEMBER_TRAIT)

namespace asio {
namespace traits {

template <typename R>
struct submit_member<no_submit_3, R>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef void result_type;
};

} // namespace traits
} // namespace asio

#endif // !defined(ASIO_HAS_DEDUCED_SUBMIT_MEMBER_TRAIT)

struct const_member_submit : exec::sender_base
{
  const_member_submit()
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
    (void)r;
    ++call_count;
  }
};

namespace asio {
namespace traits {

#if !defined(ASIO_HAS_DEDUCED_CONNECT_MEMBER_TRAIT)

template <typename R>
struct connect_member<const const_member_submit, R>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef operation_state result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_CONNECT_MEMBER_TRAIT)

#if !defined(ASIO_HAS_DEDUCED_SUBMIT_MEMBER_TRAIT)

template <typename R>
struct submit_member<const const_member_submit, R>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef void result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_SUBMIT_MEMBER_TRAIT)

} // namespace traits
} // namespace asio

struct free_submit_const_receiver : exec::sender_base
{
  free_submit_const_receiver()
  {
  }

  template <typename R>
  friend operation_state connect(
      const free_submit_const_receiver&, ASIO_MOVE_ARG(R) r)
  {
    (void)r;
    return operation_state();
  }

  template <typename R>
  friend void submit(
      const free_submit_const_receiver&, ASIO_MOVE_ARG(R) r)
  {
    (void)r;
    ++call_count;
  }
};

namespace asio {
namespace traits {

#if !defined(ASIO_HAS_DEDUCED_CONNECT_FREE_TRAIT)

template <typename R>
struct connect_free<const free_submit_const_receiver, R>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef operation_state result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_CONNECT_FREE_TRAIT)

#if !defined(ASIO_HAS_DEDUCED_SUBMIT_FREE_TRAIT)

template <typename R>
struct submit_free<const free_submit_const_receiver, R>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef void result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_SUBMIT_FREE_TRAIT)

} // namespace traits
} // namespace asio

struct non_const_member_submit : exec::sender_base
{
  non_const_member_submit()
  {
  }

  template <typename R>
  operation_state connect(ASIO_MOVE_ARG(R) r)
  {
    (void)r;
    return operation_state();
  }

  template <typename R>
  void submit(ASIO_MOVE_ARG(R) r)
  {
    (void)r;
    ++call_count;
  }
};

namespace asio {
namespace traits {

#if !defined(ASIO_HAS_DEDUCED_CONNECT_MEMBER_TRAIT)

template <typename R>
struct connect_member<non_const_member_submit, R>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef operation_state result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_CONNECT_MEMBER_TRAIT)

#if !defined(ASIO_HAS_DEDUCED_SUBMIT_MEMBER_TRAIT)

template <typename R>
struct submit_member<non_const_member_submit, R>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef void result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_SUBMIT_MEMBER_TRAIT)

} // namespace traits
} // namespace asio

struct free_submit_non_const_receiver : exec::sender_base
{
  free_submit_non_const_receiver()
  {
  }

  template <typename R>
  friend operation_state connect(
      free_submit_non_const_receiver&, ASIO_MOVE_ARG(R) r)
  {
    (void)r;
    return operation_state();
  }

  template <typename R>
  friend void submit(
      free_submit_non_const_receiver&, ASIO_MOVE_ARG(R) r)
  {
    (void)r;
    ++call_count;
  }
};

namespace asio {
namespace traits {

#if !defined(ASIO_HAS_DEDUCED_CONNECT_FREE_TRAIT)

template <typename R>
struct connect_free<free_submit_non_const_receiver, R>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef operation_state result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_CONNECT_FREE_TRAIT)

#if !defined(ASIO_HAS_DEDUCED_SUBMIT_FREE_TRAIT)

template <typename R>
struct submit_free<free_submit_non_const_receiver, R>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef void result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_SUBMIT_FREE_TRAIT)

} // namespace traits
} // namespace asio

struct receiver
{
  receiver()
  {
  }

  receiver(const receiver&)
  {
  }

#if defined(ASIO_HAS_MOVE)
  receiver(receiver&&) ASIO_NOEXCEPT
  {
  }
#endif // defined(ASIO_HAS_MOVE)

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
    ++call_count;
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

void test_can_submit()
{
  ASIO_CONSTEXPR bool b1 = exec::can_submit<
      no_submit_1&, receiver>::value;
  ASIO_CHECK(b1 == false);

  ASIO_CONSTEXPR bool b2 = exec::can_submit<
      const no_submit_1&, receiver>::value;
  ASIO_CHECK(b2 == false);

  ASIO_CONSTEXPR bool b3 = exec::can_submit<
      no_submit_2&, receiver>::value;
  ASIO_CHECK(b3 == false);

  ASIO_CONSTEXPR bool b4 = exec::can_submit<
      const no_submit_2&, receiver>::value;
  ASIO_CHECK(b4 == false);

  ASIO_CONSTEXPR bool b5 = exec::can_submit<
      no_submit_3&, receiver>::value;
  ASIO_CHECK(b5 == false);

  ASIO_CONSTEXPR bool b6 = exec::can_submit<
      const no_submit_3&, receiver>::value;
  ASIO_CHECK(b6 == false);

  ASIO_CONSTEXPR bool b7 = exec::can_submit<
      const_member_submit&, receiver>::value;
  ASIO_CHECK(b7 == true);

  ASIO_CONSTEXPR bool b8 = exec::can_submit<
      const const_member_submit&, receiver>::value;
  ASIO_CHECK(b8 == true);

  ASIO_CONSTEXPR bool b9 = exec::can_submit<
      free_submit_const_receiver&, receiver>::value;
  ASIO_CHECK(b9 == true);

  ASIO_CONSTEXPR bool b10 = exec::can_submit<
      const free_submit_const_receiver&, receiver>::value;
  ASIO_CHECK(b10 == true);

  ASIO_CONSTEXPR bool b11 = exec::can_submit<
      non_const_member_submit&, receiver>::value;
  ASIO_CHECK(b11 == true);

  ASIO_CONSTEXPR bool b12 = exec::can_submit<
      const non_const_member_submit&, receiver>::value;
  ASIO_CHECK(b12 == false);

  ASIO_CONSTEXPR bool b13 = exec::can_submit<
      free_submit_non_const_receiver&, receiver>::value;
  ASIO_CHECK(b13 == true);

  ASIO_CONSTEXPR bool b14 = exec::can_submit<
      const free_submit_non_const_receiver&, receiver>::value;
  ASIO_CHECK(b14 == false);

  ASIO_CONSTEXPR bool b15 = exec::can_submit<
      executor&, receiver>::value;
  ASIO_CHECK(b15 == true);

  ASIO_CONSTEXPR bool b16 = exec::can_submit<
      const executor&, receiver>::value;
  ASIO_CHECK(b16 == true);
}

void increment(int* count)
{
  ++(*count);
}

void test_submit()
{
  receiver r;

  call_count = 0;
  const_member_submit s1;
  exec::submit(s1, r);
  ASIO_CHECK(call_count == 1);

  call_count = 0;
  const const_member_submit s2;
  exec::submit(s2, r);
  ASIO_CHECK(call_count == 1);

  call_count = 0;
  exec::submit(const_member_submit(), r);
  ASIO_CHECK(call_count == 1);

  call_count = 0;
  free_submit_const_receiver s3;
  exec::submit(s3, r);
  ASIO_CHECK(call_count == 1);

  call_count = 0;
  const free_submit_const_receiver s4;
  exec::submit(s4, r);
  ASIO_CHECK(call_count == 1);

  call_count = 0;
  exec::submit(free_submit_const_receiver(), r);
  ASIO_CHECK(call_count == 1);

  call_count = 0;
  non_const_member_submit s5;
  exec::submit(s5, r);
  ASIO_CHECK(call_count == 1);

  call_count = 0;
  free_submit_non_const_receiver s6;
  exec::submit(s6, r);
  ASIO_CHECK(call_count == 1);

  call_count = 0;
  executor s7;
  exec::submit(s7, r);
  ASIO_CHECK(call_count == 1);

  call_count = 0;
  const executor s8;
  exec::submit(s8, r);
  ASIO_CHECK(call_count == 1);

  call_count = 0;
  exec::submit(executor(), r);
  ASIO_CHECK(call_count == 1);
}

ASIO_TEST_SUITE
(
  "submit",
  ASIO_TEST_CASE(test_can_submit)
  ASIO_TEST_CASE(test_submit)
)
