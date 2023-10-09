//
// any_executor.cpp
// ~~~~~~~~~~~~~~~~
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
#include "asio/execution/any_executor.hpp"

#include <cstring>
#include "asio/thread_pool.hpp"
#include "../unit_test.hpp"

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

static bool next_nothrow_new_fails = false;

void* operator new(std::size_t n,
    const std::nothrow_t&) ASIO_NOEXCEPT_OR_NOTHROW
{
  if (next_nothrow_new_fails)
  {
    next_nothrow_new_fails = false;
    return 0;
  }
  return ::operator new(n);
}

struct fat_executor
{
  fat_executor(int id)
    : id_(id)
  {
    std::memset(data_, 0, sizeof(data_));
  }

  template <typename F>
  void execute(const F&) const
  {
  }

  std::size_t query(execution::occupancy_t) const
  {
    return 1;
  }

  friend bool operator==(const fat_executor& a,
      const fat_executor& b) ASIO_NOEXCEPT
  {
    return a.id_ == b.id_;
  }

  friend bool operator!=(const fat_executor& a,
      const fat_executor& b) ASIO_NOEXCEPT
  {
    return a.id_ != b.id_;
  }

  int id_;
  unsigned char data_[1024];
};

namespace asio {
namespace traits {

#if !defined(ASIO_HAS_DEDUCED_EXECUTE_MEMBER_TRAIT)

template <typename F>
struct execute_member<fat_executor, F>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);
  typedef void result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_EXECUTE_MEMBER_TRAIT)

#if !defined(ASIO_HAS_DEDUCED_QUERY_MEMBER_TRAIT)

template <>
struct query_member<fat_executor, execution::occupancy_t>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = false);
  typedef std::size_t result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_QUERY_MEMBER_TRAIT)

#if !defined(ASIO_HAS_DEDUCED_EQUALITY_COMPARABLE_TRAIT)

template <>
struct equality_comparable<fat_executor>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);
};

#endif // !defined(ASIO_HAS_DEDUCED_EQUALITY_COMPARABLE_TRAIT)

} // namespace traits
} // namespace asio

void increment(int* count)
{
  ++(*count);
}

void any_executor_construction_test()
{
  typedef execution::any_executor<> ex_no_props_t;

  typedef execution::any_executor<
      execution::blocking_t
    > ex_one_prop_t;

  typedef execution::any_executor<
      execution::blocking_t,
      execution::occupancy_t
    > ex_two_props_t;

  thread_pool pool(1);
  asio::nullptr_t null_ptr = asio::nullptr_t();

  ex_two_props_t ex_two_props_1;

  ASIO_CHECK(ex_two_props_1.target<void>() == 0);
  ASIO_CHECK(ex_two_props_1 == null_ptr);

  ex_two_props_t ex_two_props_2(null_ptr);

  ASIO_CHECK(ex_two_props_2.target<void>() == 0);
  ASIO_CHECK(ex_two_props_2 == null_ptr);
  ASIO_CHECK(ex_two_props_2 == ex_two_props_1);

  ex_two_props_t ex_two_props_3(pool.executor());

  ASIO_CHECK(ex_two_props_3.target<void>() != 0);
  ASIO_CHECK(ex_two_props_3 != null_ptr);
  ASIO_CHECK(ex_two_props_3 != ex_two_props_1);

  ex_two_props_t ex_two_props_4(ex_two_props_1);

  ASIO_CHECK(ex_two_props_4.target<void>() == 0);
  ASIO_CHECK(ex_two_props_4 == null_ptr);
  ASIO_CHECK(ex_two_props_4 == ex_two_props_1);

  ex_two_props_t ex_two_props_5(ex_two_props_3);

  ASIO_CHECK(ex_two_props_5.target<void>() != 0);
  ASIO_CHECK(ex_two_props_5 != null_ptr);
  ASIO_CHECK(ex_two_props_5 == ex_two_props_3);

  ex_two_props_t ex_two_props_6 = fat_executor(1);

  ASIO_CHECK(ex_two_props_6.target<void>() != 0);
  ASIO_CHECK(ex_two_props_6 != null_ptr);
  ASIO_CHECK(ex_two_props_6 != ex_two_props_1);

  ex_two_props_t ex_two_props_7 = fat_executor(1);

  ASIO_CHECK(ex_two_props_7.target<void>() != 0);
  ASIO_CHECK(ex_two_props_7 != null_ptr);
  ASIO_CHECK(ex_two_props_7 != ex_two_props_1);
  ASIO_CHECK(ex_two_props_7 == ex_two_props_6);

  ex_two_props_t ex_two_props_8 = fat_executor(2);

  ASIO_CHECK(ex_two_props_8.target<void>() != 0);
  ASIO_CHECK(ex_two_props_8 != null_ptr);
  ASIO_CHECK(ex_two_props_8 != ex_two_props_1);
  ASIO_CHECK(ex_two_props_8 != ex_two_props_6);
  ASIO_CHECK(ex_two_props_8 != ex_two_props_7);

  ex_two_props_t ex_two_props_9(ex_two_props_6);

  ASIO_CHECK(ex_two_props_9.target<void>() != 0);
  ASIO_CHECK(ex_two_props_9 != null_ptr);
  ASIO_CHECK(ex_two_props_9 != ex_two_props_1);
  ASIO_CHECK(ex_two_props_9 == ex_two_props_6);
  ASIO_CHECK(ex_two_props_9 == ex_two_props_7);
  ASIO_CHECK(ex_two_props_9 != ex_two_props_8);

#if defined(ASIO_HAS_MOVE)
  ex_two_props_t ex_two_props_10(std::move(ex_two_props_1));

  ASIO_CHECK(ex_two_props_10.target<void>() == 0);
  ASIO_CHECK(ex_two_props_10 == null_ptr);
  ASIO_CHECK(ex_two_props_1.target<void>() == 0);
  ASIO_CHECK(ex_two_props_1 == null_ptr);

  ex_two_props_t ex_two_props_11(std::move(ex_two_props_3));

  ASIO_CHECK(ex_two_props_11.target<void>() != 0);
  ASIO_CHECK(ex_two_props_11 != null_ptr);
  ASIO_CHECK(ex_two_props_3.target<void>() == 0);
  ASIO_CHECK(ex_two_props_3 == null_ptr);
  ASIO_CHECK(ex_two_props_11 == ex_two_props_5);

  ex_two_props_t ex_two_props_12(std::move(ex_two_props_7));

  ASIO_CHECK(ex_two_props_12.target<void>() != 0);
  ASIO_CHECK(ex_two_props_12 != null_ptr);
  ASIO_CHECK(ex_two_props_7.target<void>() == 0);
  ASIO_CHECK(ex_two_props_7 == null_ptr);
  ASIO_CHECK(ex_two_props_12 == ex_two_props_6);
  ASIO_CHECK(ex_two_props_12 != ex_two_props_8);
#endif // defined(ASIO_HAS_MOVE)

  ex_one_prop_t ex_one_prop_1;

  ASIO_CHECK(ex_one_prop_1.target<void>() == 0);
  ASIO_CHECK(ex_one_prop_1 == null_ptr);

  ex_one_prop_t ex_one_prop_2(null_ptr);

  ASIO_CHECK(ex_one_prop_2.target<void>() == 0);
  ASIO_CHECK(ex_one_prop_2 == null_ptr);
  ASIO_CHECK(ex_one_prop_2 == ex_one_prop_1);

  ex_one_prop_t ex_one_prop_3(pool.executor());

  ASIO_CHECK(ex_one_prop_3.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_3 != null_ptr);
  ASIO_CHECK(ex_one_prop_3 != ex_one_prop_1);

  ex_one_prop_t ex_one_prop_4(ex_one_prop_1);

  ASIO_CHECK(ex_one_prop_4.target<void>() == 0);
  ASIO_CHECK(ex_one_prop_4 == null_ptr);
  ASIO_CHECK(ex_one_prop_4 == ex_one_prop_1);

  ex_one_prop_t ex_one_prop_5(ex_one_prop_3);

  ASIO_CHECK(ex_one_prop_5.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_5 != null_ptr);
  ASIO_CHECK(ex_one_prop_5 == ex_one_prop_3);

  ex_one_prop_t ex_one_prop_6 = fat_executor(1);

  ASIO_CHECK(ex_one_prop_6.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_6 != null_ptr);
  ASIO_CHECK(ex_one_prop_6 != ex_one_prop_1);

  ex_one_prop_t ex_one_prop_7 = fat_executor(1);

  ASIO_CHECK(ex_one_prop_7.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_7 != null_ptr);
  ASIO_CHECK(ex_one_prop_7 != ex_one_prop_1);
  ASIO_CHECK(ex_one_prop_7 == ex_one_prop_6);

  ex_one_prop_t ex_one_prop_8 = fat_executor(2);

  ASIO_CHECK(ex_one_prop_8.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_8 != null_ptr);
  ASIO_CHECK(ex_one_prop_8 != ex_one_prop_1);
  ASIO_CHECK(ex_one_prop_8 != ex_one_prop_6);
  ASIO_CHECK(ex_one_prop_8 != ex_one_prop_7);

  ex_one_prop_t ex_one_prop_9(ex_one_prop_6);

  ASIO_CHECK(ex_one_prop_9.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_9 != null_ptr);
  ASIO_CHECK(ex_one_prop_9 != ex_one_prop_1);
  ASIO_CHECK(ex_one_prop_9 == ex_one_prop_6);
  ASIO_CHECK(ex_one_prop_9 == ex_one_prop_7);
  ASIO_CHECK(ex_one_prop_9 != ex_one_prop_8);

#if defined(ASIO_HAS_MOVE)
  ex_one_prop_t ex_one_prop_10(std::move(ex_one_prop_1));

  ASIO_CHECK(ex_one_prop_10.target<void>() == 0);
  ASIO_CHECK(ex_one_prop_10 == null_ptr);
  ASIO_CHECK(ex_one_prop_1.target<void>() == 0);
  ASIO_CHECK(ex_one_prop_1 == null_ptr);

  ex_one_prop_t ex_one_prop_11(std::move(ex_one_prop_3));

  ASIO_CHECK(ex_one_prop_11.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_11 != null_ptr);
  ASIO_CHECK(ex_one_prop_3.target<void>() == 0);
  ASIO_CHECK(ex_one_prop_3 == null_ptr);
  ASIO_CHECK(ex_one_prop_11 == ex_one_prop_5);

  ex_one_prop_t ex_one_prop_12(std::move(ex_one_prop_7));

  ASIO_CHECK(ex_one_prop_12.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_12 != null_ptr);
  ASIO_CHECK(ex_one_prop_7.target<void>() == 0);
  ASIO_CHECK(ex_one_prop_7 == null_ptr);
  ASIO_CHECK(ex_one_prop_12 == ex_one_prop_6);
  ASIO_CHECK(ex_one_prop_12 != ex_one_prop_8);
#endif // defined(ASIO_HAS_MOVE)

  ex_one_prop_t ex_one_prop_13(ex_two_props_1);

  ASIO_CHECK(ex_one_prop_13.target<void>() == 0);
  ASIO_CHECK(ex_one_prop_13 == null_ptr);

  ex_one_prop_t ex_one_prop_14(ex_two_props_5);

  ASIO_CHECK(ex_one_prop_14.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_14 != null_ptr);

  ex_one_prop_t ex_one_prop_15(ex_two_props_9);

  ASIO_CHECK(ex_one_prop_15.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_15 != null_ptr);

  ex_no_props_t ex_no_props_1;

  ASIO_CHECK(ex_no_props_1.target<void>() == 0);
  ASIO_CHECK(ex_no_props_1 == null_ptr);

  ex_no_props_t ex_no_props_2(null_ptr);

  ASIO_CHECK(ex_no_props_2.target<void>() == 0);
  ASIO_CHECK(ex_no_props_2 == null_ptr);
  ASIO_CHECK(ex_no_props_2 == ex_no_props_1);

  ex_no_props_t ex_no_props_3(pool.executor());

  ASIO_CHECK(ex_no_props_3.target<void>() != 0);
  ASIO_CHECK(ex_no_props_3 != null_ptr);
  ASIO_CHECK(ex_no_props_3 != ex_no_props_1);

  ex_no_props_t ex_no_props_4(ex_no_props_1);

  ASIO_CHECK(ex_no_props_4.target<void>() == 0);
  ASIO_CHECK(ex_no_props_4 == null_ptr);
  ASIO_CHECK(ex_no_props_4 == ex_no_props_1);

  ex_no_props_t ex_no_props_5(ex_no_props_3);

  ASIO_CHECK(ex_no_props_5.target<void>() != 0);
  ASIO_CHECK(ex_no_props_5 != null_ptr);
  ASIO_CHECK(ex_no_props_5 == ex_no_props_3);

  ex_no_props_t ex_no_props_6 = fat_executor(1);

  ASIO_CHECK(ex_no_props_6.target<void>() != 0);
  ASIO_CHECK(ex_no_props_6 != null_ptr);
  ASIO_CHECK(ex_no_props_6 != ex_no_props_1);

  ex_no_props_t ex_no_props_7 = fat_executor(1);

  ASIO_CHECK(ex_no_props_7.target<void>() != 0);
  ASIO_CHECK(ex_no_props_7 != null_ptr);
  ASIO_CHECK(ex_no_props_7 != ex_no_props_1);
  ASIO_CHECK(ex_no_props_7 == ex_no_props_6);

  ex_no_props_t ex_no_props_8 = fat_executor(2);

  ASIO_CHECK(ex_no_props_8.target<void>() != 0);
  ASIO_CHECK(ex_no_props_8 != null_ptr);
  ASIO_CHECK(ex_no_props_8 != ex_no_props_1);
  ASIO_CHECK(ex_no_props_8 != ex_no_props_6);
  ASIO_CHECK(ex_no_props_8 != ex_no_props_7);

  ex_no_props_t ex_no_props_9(ex_no_props_6);

  ASIO_CHECK(ex_no_props_9.target<void>() != 0);
  ASIO_CHECK(ex_no_props_9 != null_ptr);
  ASIO_CHECK(ex_no_props_9 != ex_no_props_1);
  ASIO_CHECK(ex_no_props_9 == ex_no_props_6);
  ASIO_CHECK(ex_no_props_9 == ex_no_props_7);
  ASIO_CHECK(ex_no_props_9 != ex_no_props_8);

#if defined(ASIO_HAS_MOVE)
  ex_no_props_t ex_no_props_10(std::move(ex_no_props_1));

  ASIO_CHECK(ex_no_props_10.target<void>() == 0);
  ASIO_CHECK(ex_no_props_10 == null_ptr);
  ASIO_CHECK(ex_no_props_1.target<void>() == 0);
  ASIO_CHECK(ex_no_props_1 == null_ptr);

  ex_no_props_t ex_no_props_11(std::move(ex_no_props_3));

  ASIO_CHECK(ex_no_props_11.target<void>() != 0);
  ASIO_CHECK(ex_no_props_11 != null_ptr);
  ASIO_CHECK(ex_no_props_3.target<void>() == 0);
  ASIO_CHECK(ex_no_props_3 == null_ptr);
  ASIO_CHECK(ex_no_props_11 == ex_no_props_5);

  ex_no_props_t ex_no_props_12(std::move(ex_no_props_7));

  ASIO_CHECK(ex_no_props_12.target<void>() != 0);
  ASIO_CHECK(ex_no_props_12 != null_ptr);
  ASIO_CHECK(ex_no_props_7.target<void>() == 0);
  ASIO_CHECK(ex_no_props_7 == null_ptr);
  ASIO_CHECK(ex_no_props_12 == ex_no_props_6);
  ASIO_CHECK(ex_no_props_12 != ex_no_props_8);
#endif // defined(ASIO_HAS_MOVE)

  ex_no_props_t ex_no_props_13(ex_two_props_1);

  ASIO_CHECK(ex_no_props_13.target<void>() == 0);
  ASIO_CHECK(ex_no_props_13 == null_ptr);

  ex_no_props_t ex_no_props_14(ex_two_props_5);

  ASIO_CHECK(ex_no_props_14.target<void>() != 0);
  ASIO_CHECK(ex_no_props_14 != null_ptr);

  ex_no_props_t ex_no_props_15(ex_two_props_9);

  ASIO_CHECK(ex_no_props_15.target<void>() != 0);
  ASIO_CHECK(ex_no_props_15 != null_ptr);

  ex_no_props_t ex_no_props_16(ex_one_prop_1);

  ASIO_CHECK(ex_no_props_16.target<void>() == 0);
  ASIO_CHECK(ex_no_props_16 == null_ptr);

  ex_no_props_t ex_no_props_17(ex_one_prop_5);

  ASIO_CHECK(ex_no_props_17.target<void>() != 0);
  ASIO_CHECK(ex_no_props_17 != null_ptr);

  ex_no_props_t ex_no_props_18(ex_one_prop_9);

  ASIO_CHECK(ex_no_props_18.target<void>() != 0);
  ASIO_CHECK(ex_no_props_18 != null_ptr);
}

void any_executor_nothrow_construction_test()
{
  typedef execution::any_executor<> ex_no_props_t;

  typedef execution::any_executor<
      execution::blocking_t
    > ex_one_prop_t;

  typedef execution::any_executor<
      execution::blocking_t,
      execution::occupancy_t
    > ex_two_props_t;

  thread_pool pool(1);
  asio::nullptr_t null_ptr = asio::nullptr_t();

  ex_two_props_t ex_two_props_1;

  ASIO_CHECK(ex_two_props_1.target<void>() == 0);
  ASIO_CHECK(ex_two_props_1 == null_ptr);

  ex_two_props_t ex_two_props_2(null_ptr);

  ASIO_CHECK(ex_two_props_2.target<void>() == 0);
  ASIO_CHECK(ex_two_props_2 == null_ptr);
  ASIO_CHECK(ex_two_props_2 == ex_two_props_1);

  ex_two_props_t ex_two_props_3(std::nothrow, pool.executor());

  ASIO_CHECK(ex_two_props_3.target<void>() != 0);
  ASIO_CHECK(ex_two_props_3 != null_ptr);
  ASIO_CHECK(ex_two_props_3 != ex_two_props_1);

  ex_two_props_t ex_two_props_4(std::nothrow, ex_two_props_1);

  ASIO_CHECK(ex_two_props_4.target<void>() == 0);
  ASIO_CHECK(ex_two_props_4 == null_ptr);
  ASIO_CHECK(ex_two_props_4 == ex_two_props_1);

  ex_two_props_t ex_two_props_5(std::nothrow, ex_two_props_3);

  ASIO_CHECK(ex_two_props_5.target<void>() != 0);
  ASIO_CHECK(ex_two_props_5 != null_ptr);
  ASIO_CHECK(ex_two_props_5 == ex_two_props_3);

  ex_two_props_t ex_two_props_6(std::nothrow, fat_executor(1));

  ASIO_CHECK(ex_two_props_6.target<void>() != 0);
  ASIO_CHECK(ex_two_props_6 != null_ptr);
  ASIO_CHECK(ex_two_props_6 != ex_two_props_1);

  ex_two_props_t ex_two_props_7(std::nothrow, fat_executor(1));

  ASIO_CHECK(ex_two_props_7.target<void>() != 0);
  ASIO_CHECK(ex_two_props_7 != null_ptr);
  ASIO_CHECK(ex_two_props_7 != ex_two_props_1);
  ASIO_CHECK(ex_two_props_7 == ex_two_props_6);

  ex_two_props_t ex_two_props_8(std::nothrow, fat_executor(2));

  ASIO_CHECK(ex_two_props_8.target<void>() != 0);
  ASIO_CHECK(ex_two_props_8 != null_ptr);
  ASIO_CHECK(ex_two_props_8 != ex_two_props_1);
  ASIO_CHECK(ex_two_props_8 != ex_two_props_6);
  ASIO_CHECK(ex_two_props_8 != ex_two_props_7);

  ex_two_props_t ex_two_props_9(std::nothrow, ex_two_props_6);

  ASIO_CHECK(ex_two_props_9.target<void>() != 0);
  ASIO_CHECK(ex_two_props_9 != null_ptr);
  ASIO_CHECK(ex_two_props_9 != ex_two_props_1);
  ASIO_CHECK(ex_two_props_9 == ex_two_props_6);
  ASIO_CHECK(ex_two_props_9 == ex_two_props_7);
  ASIO_CHECK(ex_two_props_9 != ex_two_props_8);

#if defined(ASIO_HAS_MOVE)
  ex_two_props_t ex_two_props_10(std::nothrow, std::move(ex_two_props_1));

  ASIO_CHECK(ex_two_props_10.target<void>() == 0);
  ASIO_CHECK(ex_two_props_10 == null_ptr);
  ASIO_CHECK(ex_two_props_1.target<void>() == 0);
  ASIO_CHECK(ex_two_props_1 == null_ptr);

  ex_two_props_t ex_two_props_11(std::nothrow, std::move(ex_two_props_3));

  ASIO_CHECK(ex_two_props_11.target<void>() != 0);
  ASIO_CHECK(ex_two_props_11 != null_ptr);
  ASIO_CHECK(ex_two_props_3.target<void>() == 0);
  ASIO_CHECK(ex_two_props_3 == null_ptr);
  ASIO_CHECK(ex_two_props_11 == ex_two_props_5);

  ex_two_props_t ex_two_props_12(std::nothrow, std::move(ex_two_props_7));

  ASIO_CHECK(ex_two_props_12.target<void>() != 0);
  ASIO_CHECK(ex_two_props_12 != null_ptr);
  ASIO_CHECK(ex_two_props_7.target<void>() == 0);
  ASIO_CHECK(ex_two_props_7 == null_ptr);
  ASIO_CHECK(ex_two_props_12 == ex_two_props_6);
  ASIO_CHECK(ex_two_props_12 != ex_two_props_8);
#endif // defined(ASIO_HAS_MOVE)

  next_nothrow_new_fails = true;
  ex_two_props_t ex_two_props_13(std::nothrow, fat_executor(3));

  ASIO_CHECK(ex_two_props_13.target<void>() == 0);
  ASIO_CHECK(ex_two_props_13 == null_ptr);
  ASIO_CHECK(ex_two_props_13 == ex_two_props_1);

  ex_one_prop_t ex_one_prop_1;

  ASIO_CHECK(ex_one_prop_1.target<void>() == 0);
  ASIO_CHECK(ex_one_prop_1 == null_ptr);

  ex_one_prop_t ex_one_prop_2(null_ptr);

  ASIO_CHECK(ex_one_prop_2.target<void>() == 0);
  ASIO_CHECK(ex_one_prop_2 == null_ptr);
  ASIO_CHECK(ex_one_prop_2 == ex_one_prop_1);

  ex_one_prop_t ex_one_prop_3(std::nothrow, pool.executor());

  ASIO_CHECK(ex_one_prop_3.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_3 != null_ptr);
  ASIO_CHECK(ex_one_prop_3 != ex_one_prop_1);

  ex_one_prop_t ex_one_prop_4(std::nothrow, ex_one_prop_1);

  ASIO_CHECK(ex_one_prop_4.target<void>() == 0);
  ASIO_CHECK(ex_one_prop_4 == null_ptr);
  ASIO_CHECK(ex_one_prop_4 == ex_one_prop_1);

  ex_one_prop_t ex_one_prop_5(std::nothrow, ex_one_prop_3);

  ASIO_CHECK(ex_one_prop_5.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_5 != null_ptr);
  ASIO_CHECK(ex_one_prop_5 == ex_one_prop_3);

  ex_one_prop_t ex_one_prop_6 = fat_executor(1);

  ASIO_CHECK(ex_one_prop_6.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_6 != null_ptr);
  ASIO_CHECK(ex_one_prop_6 != ex_one_prop_1);

  ex_one_prop_t ex_one_prop_7(std::nothrow, fat_executor(1));

  ASIO_CHECK(ex_one_prop_7.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_7 != null_ptr);
  ASIO_CHECK(ex_one_prop_7 != ex_one_prop_1);
  ASIO_CHECK(ex_one_prop_7 == ex_one_prop_6);

  ex_one_prop_t ex_one_prop_8(std::nothrow, fat_executor(2));

  ASIO_CHECK(ex_one_prop_8.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_8 != null_ptr);
  ASIO_CHECK(ex_one_prop_8 != ex_one_prop_1);
  ASIO_CHECK(ex_one_prop_8 != ex_one_prop_6);
  ASIO_CHECK(ex_one_prop_8 != ex_one_prop_7);

  ex_one_prop_t ex_one_prop_9(std::nothrow, ex_one_prop_6);

  ASIO_CHECK(ex_one_prop_9.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_9 != null_ptr);
  ASIO_CHECK(ex_one_prop_9 != ex_one_prop_1);
  ASIO_CHECK(ex_one_prop_9 == ex_one_prop_6);
  ASIO_CHECK(ex_one_prop_9 == ex_one_prop_7);
  ASIO_CHECK(ex_one_prop_9 != ex_one_prop_8);

#if defined(ASIO_HAS_MOVE)
  ex_one_prop_t ex_one_prop_10(std::nothrow, std::move(ex_one_prop_1));

  ASIO_CHECK(ex_one_prop_10.target<void>() == 0);
  ASIO_CHECK(ex_one_prop_10 == null_ptr);
  ASIO_CHECK(ex_one_prop_1.target<void>() == 0);
  ASIO_CHECK(ex_one_prop_1 == null_ptr);

  ex_one_prop_t ex_one_prop_11(std::nothrow, std::move(ex_one_prop_3));

  ASIO_CHECK(ex_one_prop_11.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_11 != null_ptr);
  ASIO_CHECK(ex_one_prop_3.target<void>() == 0);
  ASIO_CHECK(ex_one_prop_3 == null_ptr);
  ASIO_CHECK(ex_one_prop_11 == ex_one_prop_5);

  ex_one_prop_t ex_one_prop_12(std::nothrow, std::move(ex_one_prop_7));

  ASIO_CHECK(ex_one_prop_12.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_12 != null_ptr);
  ASIO_CHECK(ex_one_prop_7.target<void>() == 0);
  ASIO_CHECK(ex_one_prop_7 == null_ptr);
  ASIO_CHECK(ex_one_prop_12 == ex_one_prop_6);
  ASIO_CHECK(ex_one_prop_12 != ex_one_prop_8);
#endif // defined(ASIO_HAS_MOVE)

  ex_one_prop_t ex_one_prop_13(std::nothrow, ex_two_props_1);

  ASIO_CHECK(ex_one_prop_13.target<void>() == 0);
  ASIO_CHECK(ex_one_prop_13 == null_ptr);

  ex_one_prop_t ex_one_prop_14(std::nothrow, ex_two_props_5);

  ASIO_CHECK(ex_one_prop_14.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_14 != null_ptr);

  ex_one_prop_t ex_one_prop_15(std::nothrow, ex_two_props_9);

  ASIO_CHECK(ex_one_prop_15.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_15 != null_ptr);

  next_nothrow_new_fails = true;
  ex_one_prop_t ex_one_prop_16(std::nothrow, fat_executor(3));

  ASIO_CHECK(ex_one_prop_16.target<void>() == 0);
  ASIO_CHECK(ex_one_prop_16 == null_ptr);
  ASIO_CHECK(ex_one_prop_16 == ex_one_prop_1);

  ex_no_props_t ex_no_props_1;

  ASIO_CHECK(ex_no_props_1.target<void>() == 0);
  ASIO_CHECK(ex_no_props_1 == null_ptr);

  ex_no_props_t ex_no_props_2(null_ptr);

  ASIO_CHECK(ex_no_props_2.target<void>() == 0);
  ASIO_CHECK(ex_no_props_2 == null_ptr);
  ASIO_CHECK(ex_no_props_2 == ex_no_props_1);

  ex_no_props_t ex_no_props_3(std::nothrow, pool.executor());

  ASIO_CHECK(ex_no_props_3.target<void>() != 0);
  ASIO_CHECK(ex_no_props_3 != null_ptr);
  ASIO_CHECK(ex_no_props_3 != ex_no_props_1);

  ex_no_props_t ex_no_props_4(std::nothrow, ex_no_props_1);

  ASIO_CHECK(ex_no_props_4.target<void>() == 0);
  ASIO_CHECK(ex_no_props_4 == null_ptr);
  ASIO_CHECK(ex_no_props_4 == ex_no_props_1);

  ex_no_props_t ex_no_props_5(std::nothrow, ex_no_props_3);

  ASIO_CHECK(ex_no_props_5.target<void>() != 0);
  ASIO_CHECK(ex_no_props_5 != null_ptr);
  ASIO_CHECK(ex_no_props_5 == ex_no_props_3);

  ex_no_props_t ex_no_props_6(std::nothrow, fat_executor(1));

  ASIO_CHECK(ex_no_props_6.target<void>() != 0);
  ASIO_CHECK(ex_no_props_6 != null_ptr);
  ASIO_CHECK(ex_no_props_6 != ex_no_props_1);

  ex_no_props_t ex_no_props_7(std::nothrow, fat_executor(1));

  ASIO_CHECK(ex_no_props_7.target<void>() != 0);
  ASIO_CHECK(ex_no_props_7 != null_ptr);
  ASIO_CHECK(ex_no_props_7 != ex_no_props_1);
  ASIO_CHECK(ex_no_props_7 == ex_no_props_6);

  ex_no_props_t ex_no_props_8(std::nothrow, fat_executor(2));

  ASIO_CHECK(ex_no_props_8.target<void>() != 0);
  ASIO_CHECK(ex_no_props_8 != null_ptr);
  ASIO_CHECK(ex_no_props_8 != ex_no_props_1);
  ASIO_CHECK(ex_no_props_8 != ex_no_props_6);
  ASIO_CHECK(ex_no_props_8 != ex_no_props_7);

  ex_no_props_t ex_no_props_9(std::nothrow, ex_no_props_6);

  ASIO_CHECK(ex_no_props_9.target<void>() != 0);
  ASIO_CHECK(ex_no_props_9 != null_ptr);
  ASIO_CHECK(ex_no_props_9 != ex_no_props_1);
  ASIO_CHECK(ex_no_props_9 == ex_no_props_6);
  ASIO_CHECK(ex_no_props_9 == ex_no_props_7);
  ASIO_CHECK(ex_no_props_9 != ex_no_props_8);

#if defined(ASIO_HAS_MOVE)
  ex_no_props_t ex_no_props_10(std::nothrow, std::move(ex_no_props_1));

  ASIO_CHECK(ex_no_props_10.target<void>() == 0);
  ASIO_CHECK(ex_no_props_10 == null_ptr);
  ASIO_CHECK(ex_no_props_1.target<void>() == 0);
  ASIO_CHECK(ex_no_props_1 == null_ptr);

  ex_no_props_t ex_no_props_11(std::nothrow, std::move(ex_no_props_3));

  ASIO_CHECK(ex_no_props_11.target<void>() != 0);
  ASIO_CHECK(ex_no_props_11 != null_ptr);
  ASIO_CHECK(ex_no_props_3.target<void>() == 0);
  ASIO_CHECK(ex_no_props_3 == null_ptr);
  ASIO_CHECK(ex_no_props_11 == ex_no_props_5);

  ex_no_props_t ex_no_props_12(std::nothrow, std::move(ex_no_props_7));

  ASIO_CHECK(ex_no_props_12.target<void>() != 0);
  ASIO_CHECK(ex_no_props_12 != null_ptr);
  ASIO_CHECK(ex_no_props_7.target<void>() == 0);
  ASIO_CHECK(ex_no_props_7 == null_ptr);
  ASIO_CHECK(ex_no_props_12 == ex_no_props_6);
  ASIO_CHECK(ex_no_props_12 != ex_no_props_8);
#endif // defined(ASIO_HAS_MOVE)

  ex_no_props_t ex_no_props_13(std::nothrow, ex_two_props_1);

  ASIO_CHECK(ex_no_props_13.target<void>() == 0);
  ASIO_CHECK(ex_no_props_13 == null_ptr);

  ex_no_props_t ex_no_props_14(std::nothrow, ex_two_props_5);

  ASIO_CHECK(ex_no_props_14.target<void>() != 0);
  ASIO_CHECK(ex_no_props_14 != null_ptr);

  ex_no_props_t ex_no_props_15(std::nothrow, ex_two_props_9);

  ASIO_CHECK(ex_no_props_15.target<void>() != 0);
  ASIO_CHECK(ex_no_props_15 != null_ptr);

  ex_no_props_t ex_no_props_16(std::nothrow, ex_one_prop_1);

  ASIO_CHECK(ex_no_props_16.target<void>() == 0);
  ASIO_CHECK(ex_no_props_16 == null_ptr);

  ex_no_props_t ex_no_props_17(std::nothrow, ex_one_prop_5);

  ASIO_CHECK(ex_no_props_17.target<void>() != 0);
  ASIO_CHECK(ex_no_props_17 != null_ptr);

  ex_no_props_t ex_no_props_18(std::nothrow, ex_one_prop_9);

  ASIO_CHECK(ex_no_props_18.target<void>() != 0);
  ASIO_CHECK(ex_no_props_18 != null_ptr);

  next_nothrow_new_fails = true;
  ex_no_props_t ex_no_props_19(std::nothrow, fat_executor(3));

  ASIO_CHECK(ex_no_props_19.target<void>() == 0);
  ASIO_CHECK(ex_no_props_19 == null_ptr);
  ASIO_CHECK(ex_no_props_19 == ex_no_props_1);
}

void any_executor_assignment_test()
{
  typedef execution::any_executor<> ex_no_props_t;

  typedef execution::any_executor<
      execution::blocking_t
    > ex_one_prop_t;

  typedef execution::any_executor<
      execution::blocking_t,
      execution::occupancy_t
    > ex_two_props_t;

  thread_pool pool(1);
  asio::nullptr_t null_ptr = asio::nullptr_t();

  ex_two_props_t ex_two_props_1;

  ex_two_props_t ex_two_props_2;
  ex_two_props_2 = null_ptr;

  ASIO_CHECK(ex_two_props_2.target<void>() == 0);

  ex_two_props_t ex_two_props_3;
  ex_two_props_3 = pool.executor();

  ASIO_CHECK(ex_two_props_3.target<void>() != 0);

  ex_two_props_t ex_two_props_4;
  ex_two_props_4 = ex_two_props_1;

  ASIO_CHECK(ex_two_props_4.target<void>() == 0);
  ASIO_CHECK(ex_two_props_4 == ex_two_props_1);

  ex_two_props_4 = ex_two_props_3;

  ASIO_CHECK(ex_two_props_4.target<void>() != 0);
  ASIO_CHECK(ex_two_props_4 == ex_two_props_3);

  ex_two_props_t ex_two_props_5;
  ex_two_props_5 = fat_executor(1);

  ASIO_CHECK(ex_two_props_5.target<void>() != 0);
  ASIO_CHECK(ex_two_props_5 != null_ptr);
  ASIO_CHECK(ex_two_props_5 != ex_two_props_1);

  ex_two_props_t ex_two_props_6;
  ex_two_props_6 = fat_executor(1);

  ASIO_CHECK(ex_two_props_6.target<void>() != 0);
  ASIO_CHECK(ex_two_props_6 != null_ptr);
  ASIO_CHECK(ex_two_props_6 != ex_two_props_1);
  ASIO_CHECK(ex_two_props_6 == ex_two_props_5);

  ex_two_props_6 = fat_executor(2);

  ASIO_CHECK(ex_two_props_6.target<void>() != 0);
  ASIO_CHECK(ex_two_props_6 != null_ptr);
  ASIO_CHECK(ex_two_props_6 != ex_two_props_1);
  ASIO_CHECK(ex_two_props_6 != ex_two_props_5);

  ex_two_props_t ex_two_props_7;
  ex_two_props_7 = ex_two_props_5;

  ASIO_CHECK(ex_two_props_7.target<void>() != 0);
  ASIO_CHECK(ex_two_props_7 != null_ptr);
  ASIO_CHECK(ex_two_props_7 != ex_two_props_1);
  ASIO_CHECK(ex_two_props_7 == ex_two_props_5);
  ASIO_CHECK(ex_two_props_7 != ex_two_props_6);

#if defined(ASIO_HAS_MOVE)
  ex_two_props_t ex_two_props_8;
  ex_two_props_8 = std::move(ex_two_props_1);

  ASIO_CHECK(ex_two_props_8.target<void>() == 0);
  ASIO_CHECK(ex_two_props_1.target<void>() == 0);

  ex_two_props_8 = std::move(ex_two_props_3);

  ASIO_CHECK(ex_two_props_8.target<void>() != 0);
  ASIO_CHECK(ex_two_props_3.target<void>() == 0);
  ASIO_CHECK(ex_two_props_8 == ex_two_props_4);

  ex_two_props_8 = std::move(ex_two_props_5);

  ASIO_CHECK(ex_two_props_8.target<void>() != 0);
  ASIO_CHECK(ex_two_props_5.target<void>() == 0);
  ASIO_CHECK(ex_two_props_8 == ex_two_props_7);
#endif // defined(ASIO_HAS_MOVE)

  ex_one_prop_t ex_one_prop_1;

  ex_one_prop_t ex_one_prop_2;
  ex_one_prop_2 = null_ptr;

  ASIO_CHECK(ex_one_prop_2.target<void>() == 0);

  ex_one_prop_t ex_one_prop_3;
  ex_one_prop_3 = pool.executor();

  ASIO_CHECK(ex_one_prop_3.target<void>() != 0);

  ex_one_prop_t ex_one_prop_4;
  ex_one_prop_4 = ex_one_prop_1;

  ASIO_CHECK(ex_one_prop_4.target<void>() == 0);
  ASIO_CHECK(ex_one_prop_4 == ex_one_prop_1);

  ex_one_prop_4 = ex_one_prop_3;

  ASIO_CHECK(ex_one_prop_4.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_4 == ex_one_prop_3);

  ex_one_prop_t ex_one_prop_5;
  ex_one_prop_5 = fat_executor(1);

  ASIO_CHECK(ex_one_prop_5.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_5 != null_ptr);
  ASIO_CHECK(ex_one_prop_5 != ex_one_prop_1);

  ex_one_prop_t ex_one_prop_6;
  ex_one_prop_6 = fat_executor(1);

  ASIO_CHECK(ex_one_prop_6.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_6 != null_ptr);
  ASIO_CHECK(ex_one_prop_6 != ex_one_prop_1);
  ASIO_CHECK(ex_one_prop_6 == ex_one_prop_5);

  ex_one_prop_6 = fat_executor(2);

  ASIO_CHECK(ex_one_prop_6.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_6 != null_ptr);
  ASIO_CHECK(ex_one_prop_6 != ex_one_prop_1);
  ASIO_CHECK(ex_one_prop_6 != ex_one_prop_5);

  ex_one_prop_t ex_one_prop_7;
  ex_one_prop_7 = ex_one_prop_5;

  ASIO_CHECK(ex_one_prop_7.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_7 != null_ptr);
  ASIO_CHECK(ex_one_prop_7 != ex_one_prop_1);
  ASIO_CHECK(ex_one_prop_7 == ex_one_prop_5);
  ASIO_CHECK(ex_one_prop_7 != ex_one_prop_6);

#if defined(ASIO_HAS_MOVE)
  ex_one_prop_t ex_one_prop_8;
  ex_one_prop_8 = std::move(ex_one_prop_1);

  ASIO_CHECK(ex_one_prop_8.target<void>() == 0);
  ASIO_CHECK(ex_one_prop_1.target<void>() == 0);

  ex_one_prop_8 = std::move(ex_one_prop_3);

  ASIO_CHECK(ex_one_prop_8.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_3.target<void>() == 0);
  ASIO_CHECK(ex_one_prop_8 == ex_one_prop_4);

  ex_one_prop_8 = std::move(ex_one_prop_5);

  ASIO_CHECK(ex_one_prop_8.target<void>() != 0);
  ASIO_CHECK(ex_one_prop_5.target<void>() == 0);
  ASIO_CHECK(ex_one_prop_8 == ex_one_prop_7);
#endif // defined(ASIO_HAS_MOVE)

  ex_one_prop_t ex_one_prop_9;
  ex_one_prop_9 = ex_two_props_1;

  ASIO_CHECK(ex_one_prop_9.target<void>() == 0);

  ex_one_prop_9 = ex_two_props_4;

  ASIO_CHECK(ex_one_prop_9.target<void>() != 0);

  ex_one_prop_9 = ex_two_props_7;

  ASIO_CHECK(ex_one_prop_9.target<void>() != 0);

  ex_no_props_t ex_no_props_1;

  ex_no_props_t ex_no_props_2;
  ex_no_props_2 = null_ptr;

  ASIO_CHECK(ex_no_props_2.target<void>() == 0);

  ex_no_props_t ex_no_props_3;
  ex_no_props_3 = pool.executor();

  ASIO_CHECK(ex_no_props_3.target<void>() != 0);

  ex_no_props_t ex_no_props_4;
  ex_no_props_4 = ex_no_props_1;

  ASIO_CHECK(ex_no_props_4.target<void>() == 0);
  ASIO_CHECK(ex_no_props_4 == ex_no_props_1);

  ex_no_props_4 = ex_no_props_3;

  ASIO_CHECK(ex_no_props_4.target<void>() != 0);
  ASIO_CHECK(ex_no_props_4 == ex_no_props_3);

  ex_no_props_t ex_no_props_5;
  ex_no_props_5 = fat_executor(1);

  ASIO_CHECK(ex_no_props_5.target<void>() != 0);
  ASIO_CHECK(ex_no_props_5 != null_ptr);
  ASIO_CHECK(ex_no_props_5 != ex_no_props_1);

  ex_no_props_t ex_no_props_6;
  ex_no_props_6 = fat_executor(1);

  ASIO_CHECK(ex_no_props_6.target<void>() != 0);
  ASIO_CHECK(ex_no_props_6 != null_ptr);
  ASIO_CHECK(ex_no_props_6 != ex_no_props_1);
  ASIO_CHECK(ex_no_props_6 == ex_no_props_5);

  ex_no_props_6 = fat_executor(2);

  ASIO_CHECK(ex_no_props_6.target<void>() != 0);
  ASIO_CHECK(ex_no_props_6 != null_ptr);
  ASIO_CHECK(ex_no_props_6 != ex_no_props_1);
  ASIO_CHECK(ex_no_props_6 != ex_no_props_5);

  ex_no_props_t ex_no_props_7;
  ex_no_props_7 = ex_no_props_5;

  ASIO_CHECK(ex_no_props_7.target<void>() != 0);
  ASIO_CHECK(ex_no_props_7 != null_ptr);
  ASIO_CHECK(ex_no_props_7 != ex_no_props_1);
  ASIO_CHECK(ex_no_props_7 == ex_no_props_5);
  ASIO_CHECK(ex_no_props_7 != ex_no_props_6);

#if defined(ASIO_HAS_MOVE)
  ex_no_props_t ex_no_props_8;
  ex_no_props_8 = std::move(ex_no_props_1);

  ASIO_CHECK(ex_no_props_8.target<void>() == 0);
  ASIO_CHECK(ex_no_props_1.target<void>() == 0);

  ex_no_props_8 = std::move(ex_no_props_3);

  ASIO_CHECK(ex_no_props_8.target<void>() != 0);
  ASIO_CHECK(ex_no_props_3.target<void>() == 0);
  ASIO_CHECK(ex_no_props_8 == ex_no_props_4);

  ex_no_props_8 = std::move(ex_no_props_5);

  ASIO_CHECK(ex_no_props_8.target<void>() != 0);
  ASIO_CHECK(ex_no_props_5.target<void>() == 0);
  ASIO_CHECK(ex_no_props_8 == ex_no_props_7);
#endif // defined(ASIO_HAS_MOVE)

  ex_no_props_t ex_no_props_9;
  ex_no_props_9 = ex_two_props_1;

  ASIO_CHECK(ex_no_props_9.target<void>() == 0);

  ex_no_props_9 = ex_two_props_4;

  ASIO_CHECK(ex_no_props_9.target<void>() != 0);

  ex_no_props_9 = ex_two_props_7;

  ASIO_CHECK(ex_no_props_9.target<void>() != 0);

  ex_no_props_9 = ex_one_prop_1;

  ASIO_CHECK(ex_no_props_9.target<void>() == 0);

  ex_no_props_9 = ex_one_prop_4;

  ASIO_CHECK(ex_no_props_9.target<void>() != 0);

  ex_no_props_9 = ex_one_prop_7;

  ASIO_CHECK(ex_no_props_9.target<void>() != 0);
}

void any_executor_swap_test()
{
  typedef execution::any_executor<> ex_no_props_t;

  typedef execution::any_executor<
      execution::blocking_t
    > ex_one_prop_t;

  typedef execution::any_executor<
      execution::blocking_t,
      execution::occupancy_t
    > ex_two_props_t;

  thread_pool pool1(1);
  thread_pool pool2(1);

  ex_no_props_t ex_no_props_1(pool1.executor());
  ex_no_props_t ex_no_props_2(pool2.executor());

  ex_no_props_t ex_no_props_3(ex_no_props_1);
  ex_no_props_t ex_no_props_4(ex_no_props_2);

  ASIO_CHECK(ex_no_props_3 == ex_no_props_1);
  ASIO_CHECK(ex_no_props_4 == ex_no_props_2);

  ex_no_props_3.swap(ex_no_props_4);

  ASIO_CHECK(ex_no_props_3 == ex_no_props_2);
  ASIO_CHECK(ex_no_props_4 == ex_no_props_1);

  execution::swap(ex_no_props_3, ex_no_props_4);

  ASIO_CHECK(ex_no_props_3 == ex_no_props_1);
  ASIO_CHECK(ex_no_props_4 == ex_no_props_2);

  ex_one_prop_t ex_one_prop_1(pool1.executor());
  ex_one_prop_t ex_one_prop_2(pool2.executor());

  ex_one_prop_t ex_one_prop_3(ex_one_prop_1);
  ex_one_prop_t ex_one_prop_4(ex_one_prop_2);

  ASIO_CHECK(ex_one_prop_3 == ex_one_prop_1);
  ASIO_CHECK(ex_one_prop_4 == ex_one_prop_2);

  ex_one_prop_3.swap(ex_one_prop_4);

  ASIO_CHECK(ex_one_prop_3 == ex_one_prop_2);
  ASIO_CHECK(ex_one_prop_4 == ex_one_prop_1);

  execution::swap(ex_one_prop_3, ex_one_prop_4);

  ASIO_CHECK(ex_one_prop_3 == ex_one_prop_1);
  ASIO_CHECK(ex_one_prop_4 == ex_one_prop_2);

  ex_two_props_t ex_two_props_1(pool1.executor());
  ex_two_props_t ex_two_props_2(pool2.executor());

  ex_two_props_t ex_two_props_3(ex_two_props_1);
  ex_two_props_t ex_two_props_4(ex_two_props_2);

  ASIO_CHECK(ex_two_props_3 == ex_two_props_1);
  ASIO_CHECK(ex_two_props_4 == ex_two_props_2);

  ex_two_props_3.swap(ex_two_props_4);

  ASIO_CHECK(ex_two_props_3 == ex_two_props_2);
  ASIO_CHECK(ex_two_props_4 == ex_two_props_1);

  execution::swap(ex_two_props_3, ex_two_props_4);

  ASIO_CHECK(ex_two_props_3 == ex_two_props_1);
  ASIO_CHECK(ex_two_props_4 == ex_two_props_2);
}

void any_executor_query_test()
{
  thread_pool pool(1);
  execution::any_executor<
      execution::blocking_t,
      execution::outstanding_work_t,
      execution::relationship_t,
      execution::mapping_t::thread_t,
      execution::occupancy_t>
    ex(pool.executor());

  ASIO_CHECK(
      asio::query(ex, asio::execution::blocking)
        == asio::execution::blocking.possibly);

  ASIO_CHECK(
      asio::query(ex, asio::execution::blocking.possibly)
        == asio::execution::blocking.possibly);

  ASIO_CHECK(
      asio::query(ex, asio::execution::outstanding_work)
        == asio::execution::outstanding_work.untracked);

  ASIO_CHECK(
      asio::query(ex, asio::execution::outstanding_work.untracked)
        == asio::execution::outstanding_work.untracked);

  ASIO_CHECK(
      asio::query(ex, asio::execution::relationship)
        == asio::execution::relationship.fork);

  ASIO_CHECK(
      asio::query(ex, asio::execution::relationship.fork)
        == asio::execution::relationship.fork);

  ASIO_CHECK(
      asio::query(ex, asio::execution::mapping)
        == asio::execution::mapping.thread);

  ASIO_CHECK(
      asio::query(ex, asio::execution::occupancy)
        == 1);
}

void any_executor_execute_test()
{
  int count = 0;
  thread_pool pool(1);
  execution::any_executor<
      execution::blocking_t::possibly_t,
      execution::blocking_t::never_t,
      execution::outstanding_work_t::untracked_t,
      execution::outstanding_work_t::tracked_t,
      execution::relationship_t::continuation_t>
    ex(pool.executor());

  ex.execute(bindns::bind(increment, &count));

  asio::require(ex, asio::execution::blocking.possibly).execute(
      bindns::bind(increment, &count));

  asio::require(ex, asio::execution::blocking.never).execute(
      bindns::bind(increment, &count));

  asio::require(ex,
      asio::execution::blocking.never,
      asio::execution::outstanding_work.tracked
    ).execute(bindns::bind(increment, &count));

  asio::require(ex,
      asio::execution::blocking.never,
      asio::execution::outstanding_work.untracked
    ).execute(bindns::bind(increment, &count));

  asio::require(ex,
      asio::execution::blocking.never,
      asio::execution::outstanding_work.untracked,
      asio::execution::relationship.continuation
    ).execute(bindns::bind(increment, &count));

  pool.wait();

  ASIO_CHECK(count == 6);
}

ASIO_TEST_SUITE
(
  "any_executor",
  ASIO_TEST_CASE(any_executor_construction_test)
  ASIO_TEST_CASE(any_executor_nothrow_construction_test)
  ASIO_TEST_CASE(any_executor_assignment_test)
  ASIO_TEST_CASE(any_executor_swap_test)
  ASIO_TEST_CASE(any_executor_query_test)
  ASIO_TEST_CASE(any_executor_execute_test)
)
