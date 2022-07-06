//
// receiver.cpp
// ~~~~~~~~~~~~
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
#include "asio/execution/receiver.hpp"

#include <string>
#include "asio/error_code.hpp"
#include "../unit_test.hpp"

struct not_a_receiver
{
};

struct receiver
{
  receiver()
  {
  }

  receiver(const receiver&)
  {
  }

#if defined(ASIO_HAS_MOVE)
  receiver(receiver&&)
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

struct receiver_of_0
{
  receiver_of_0()
  {
  }

  receiver_of_0(const receiver_of_0&)
  {
  }

#if defined(ASIO_HAS_MOVE)
  receiver_of_0(receiver_of_0&&)
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

  void set_value()
  {
  }
};

namespace asio {
namespace traits {

#if !defined(ASIO_HAS_DEDUCED_SET_ERROR_MEMBER_TRAIT)

template <typename E>
struct set_error_member<receiver_of_0, E>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);
  typedef void result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_SET_ERROR_MEMBER_TRAIT)
#if !defined(ASIO_HAS_DEDUCED_SET_DONE_MEMBER_TRAIT)

template <>
struct set_done_member<receiver_of_0>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);
  typedef void result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_SET_DONE_MEMBER_TRAIT)
#if !defined(ASIO_HAS_DEDUCED_SET_DONE_MEMBER_TRAIT)

template <>
struct set_value_member<receiver_of_0, void()>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef void result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_SET_DONE_MEMBER_TRAIT)

} // namespace traits
} // namespace asio

struct receiver_of_1
{
  receiver_of_1()
  {
  }

  receiver_of_1(const receiver_of_1&)
  {
  }

#if defined(ASIO_HAS_MOVE)
  receiver_of_1(receiver_of_1&&)
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

  void set_value(int) ASIO_NOEXCEPT
  {
  }
};

namespace asio {
namespace traits {

#if !defined(ASIO_HAS_DEDUCED_SET_ERROR_MEMBER_TRAIT)

template <typename E>
struct set_error_member<receiver_of_1, E>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);
  typedef void result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_SET_ERROR_MEMBER_TRAIT)
#if !defined(ASIO_HAS_DEDUCED_SET_DONE_MEMBER_TRAIT)

template <>
struct set_done_member<receiver_of_1>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);
  typedef void result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_SET_DONE_MEMBER_TRAIT)
#if !defined(ASIO_HAS_DEDUCED_SET_DONE_MEMBER_TRAIT)

template <>
struct set_value_member<receiver_of_1, void(int)>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);
  typedef void result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_SET_DONE_MEMBER_TRAIT)

} // namespace traits
} // namespace asio

struct receiver_of_2
{
  receiver_of_2()
  {
  }

  receiver_of_2(const receiver_of_2&)
  {
  }

#if defined(ASIO_HAS_MOVE)
  receiver_of_2(receiver_of_2&&)
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

  void set_value(int, std::string)
  {
  }
};

namespace asio {
namespace traits {

#if !defined(ASIO_HAS_DEDUCED_SET_ERROR_MEMBER_TRAIT)

template <typename E>
struct set_error_member<receiver_of_2, E>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);
  typedef void result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_SET_ERROR_MEMBER_TRAIT)
#if !defined(ASIO_HAS_DEDUCED_SET_DONE_MEMBER_TRAIT)

template <>
struct set_done_member<receiver_of_2>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);
  typedef void result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_SET_DONE_MEMBER_TRAIT)
#if !defined(ASIO_HAS_DEDUCED_SET_DONE_MEMBER_TRAIT)

template <>
struct set_value_member<receiver_of_2, void(int, std::string)>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef void result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_SET_DONE_MEMBER_TRAIT)

} // namespace traits
} // namespace asio

void is_receiver_test()
{
  ASIO_CHECK((
      !asio::execution::is_receiver<
        void,
        asio::error_code
      >::value));

  ASIO_CHECK((
      !asio::execution::is_receiver<
        not_a_receiver,
        asio::error_code
      >::value));

  ASIO_CHECK((
      asio::execution::is_receiver<
        receiver,
        asio::error_code
      >::value));

  ASIO_CHECK((
      asio::execution::is_receiver<
        receiver_of_0,
        asio::error_code
      >::value));

  ASIO_CHECK((
      asio::execution::is_receiver<
        receiver_of_1,
        asio::error_code
      >::value));

  ASIO_CHECK((
      asio::execution::is_receiver<
        receiver_of_2,
        asio::error_code
      >::value));
}

void is_receiver_of_test()
{
  ASIO_CHECK((
      !asio::execution::is_receiver_of<
        void
      >::value));

  ASIO_CHECK((
      !asio::execution::is_receiver_of<
        void,
        int
      >::value));

  ASIO_CHECK((
      !asio::execution::is_receiver_of<
        not_a_receiver
      >::value));

  ASIO_CHECK((
      !asio::execution::is_receiver_of<
        not_a_receiver,
        int
      >::value));

  ASIO_CHECK((
      !asio::execution::is_receiver_of<
        not_a_receiver,
        int,
        std::string
      >::value));

  ASIO_CHECK((
      !asio::execution::is_receiver_of<
        receiver
      >::value));

  ASIO_CHECK((
      !asio::execution::is_receiver_of<
        receiver,
        int
      >::value));

  ASIO_CHECK((
      !asio::execution::is_receiver_of<
        receiver,
        int,
        std::string
      >::value));

  ASIO_CHECK((
      asio::execution::is_receiver_of<
        receiver_of_0
      >::value));

  ASIO_CHECK((
      !asio::execution::is_receiver_of<
        receiver_of_0,
        int
      >::value));

  ASIO_CHECK((
      !asio::execution::is_receiver_of<
        receiver_of_0,
        int,
        std::string
      >::value));

  ASIO_CHECK((
      !asio::execution::is_receiver_of<
        receiver_of_1
      >::value));

  ASIO_CHECK((
      asio::execution::is_receiver_of<
        receiver_of_1,
        int
      >::value));

  ASIO_CHECK((
      !asio::execution::is_receiver_of<
        receiver_of_1,
        int,
        std::string
      >::value));

  ASIO_CHECK((
      !asio::execution::is_receiver_of<
        receiver_of_2
      >::value));

  ASIO_CHECK((
      !asio::execution::is_receiver_of<
        receiver_of_2,
        int
      >::value));

  ASIO_CHECK((
      asio::execution::is_receiver_of<
        receiver_of_2,
        int,
        std::string
      >::value));
}

void is_nothrow_receiver_of_test()
{
  ASIO_CHECK((
      !asio::execution::is_nothrow_receiver_of<
        void
      >::value));

  ASIO_CHECK((
      !asio::execution::is_nothrow_receiver_of<
        void,
        int
      >::value));

  ASIO_CHECK((
      !asio::execution::is_nothrow_receiver_of<
        not_a_receiver
      >::value));

  ASIO_CHECK((
      !asio::execution::is_nothrow_receiver_of<
        not_a_receiver,
        int
      >::value));

  ASIO_CHECK((
      !asio::execution::is_nothrow_receiver_of<
        not_a_receiver,
        int,
        std::string
      >::value));

  ASIO_CHECK((
      !asio::execution::is_nothrow_receiver_of<
        receiver
      >::value));

  ASIO_CHECK((
      !asio::execution::is_nothrow_receiver_of<
        receiver,
        int
      >::value));

  ASIO_CHECK((
      !asio::execution::is_nothrow_receiver_of<
        receiver,
        int,
        std::string
      >::value));

  ASIO_CHECK((
      !asio::execution::is_nothrow_receiver_of<
        receiver_of_0
      >::value));

  ASIO_CHECK((
      !asio::execution::is_nothrow_receiver_of<
        receiver_of_0,
        int
      >::value));

  ASIO_CHECK((
      !asio::execution::is_nothrow_receiver_of<
        receiver_of_0,
        int,
        std::string
      >::value));

  ASIO_CHECK((
      !asio::execution::is_nothrow_receiver_of<
        receiver_of_1
      >::value));

  ASIO_CHECK((
      asio::execution::is_nothrow_receiver_of<
        receiver_of_1,
        int
      >::value));

  ASIO_CHECK((
      !asio::execution::is_nothrow_receiver_of<
        receiver_of_1,
        int,
        std::string
      >::value));

  ASIO_CHECK((
      !asio::execution::is_nothrow_receiver_of<
        receiver_of_2
      >::value));

  ASIO_CHECK((
      !asio::execution::is_nothrow_receiver_of<
        receiver_of_2,
        int
      >::value));

  ASIO_CHECK((
      !asio::execution::is_nothrow_receiver_of<
        receiver_of_2,
        int,
        std::string
      >::value));
}

ASIO_TEST_SUITE
(
  "receiver",
  ASIO_TEST_CASE(is_receiver_test)
  ASIO_TEST_CASE(is_receiver_of_test)
  ASIO_TEST_CASE(is_nothrow_receiver_of_test)
)
