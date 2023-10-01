//
// relationship.cpp
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
#include "asio/execution/relationship.hpp"

#include "asio/prefer.hpp"
#include "asio/query.hpp"
#include "asio/require.hpp"
#include "../unit_test.hpp"

namespace exec = asio::execution;

typedef exec::relationship_t s;
typedef exec::relationship_t::fork_t n1;
typedef exec::relationship_t::continuation_t n2;

struct ex_nq_nr
{
  template <typename F>
  void execute(const F&) const
  {
  }

  friend bool operator==(const ex_nq_nr&, const ex_nq_nr&) ASIO_NOEXCEPT
  {
    return true;
  }

  friend bool operator!=(const ex_nq_nr&, const ex_nq_nr&) ASIO_NOEXCEPT
  {
    return false;
  }
};

#if !defined(ASIO_HAS_DEDUCED_EXECUTION_IS_EXECUTOR_TRAIT)

namespace asio {
namespace execution {

template <>
struct is_executor<ex_nq_nr> : asio::true_type
{
};

} // namespace execution
} // namespace asio

#endif // !defined(ASIO_HAS_DEDUCED_EXECUTION_IS_EXECUTOR_TRAIT)

template <typename ResultType, typename ParamType, typename Result>
struct ex_cq_nr
{
  static ASIO_CONSTEXPR ResultType query(ParamType) ASIO_NOEXCEPT
  {
    return Result();
  }

  template <typename F>
  void execute(const F&) const
  {
  }

  friend bool operator==(const ex_cq_nr&, const ex_cq_nr&) ASIO_NOEXCEPT
  {
    return true;
  }

  friend bool operator!=(const ex_cq_nr&, const ex_cq_nr&) ASIO_NOEXCEPT
  {
    return false;
  }
};

#if !defined(ASIO_HAS_DEDUCED_EXECUTION_IS_EXECUTOR_TRAIT)

namespace asio {
namespace execution {

template <typename ResultType, typename ParamType, typename Result>
struct is_executor<ex_cq_nr<ResultType, ParamType, Result> >
  : asio::true_type
{
};

} // namespace execution
} // namespace asio

#endif // !defined(ASIO_HAS_DEDUCED_EXECUTION_IS_EXECUTOR_TRAIT)

namespace asio {
namespace traits {

#if !defined(ASIO_HAS_DEDUCED_QUERY_STATIC_CONSTEXPR_MEMBER_TRAIT)

template <typename ResultType, typename ParamType,
  typename Result, typename Param>
struct query_static_constexpr_member<
  ex_cq_nr<ResultType, ParamType, Result>, Param,
  typename asio::enable_if<
    asio::is_convertible<Param, ParamType>::value
  >::type>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);

  typedef Result result_type; // Must return raw result type.

  static ASIO_CONSTEXPR result_type value()
  {
    return Result();
  }
};

#endif // !defined(ASIO_HAS_DEDUCED_QUERY_STATIC_CONSTEXPR_MEMBER_TRAIT)

} // namespace traits
} // namespace asio

template <typename ResultType, typename ParamType, typename Result>
struct ex_mq_nr
{
  ResultType query(ParamType) const ASIO_NOEXCEPT
  {
    return Result();
  }

  template <typename F>
  void execute(const F&) const
  {
  }

  friend bool operator==(const ex_mq_nr&, const ex_mq_nr&) ASIO_NOEXCEPT
  {
    return true;
  }

  friend bool operator!=(const ex_mq_nr&, const ex_mq_nr&) ASIO_NOEXCEPT
  {
    return false;
  }
};

#if !defined(ASIO_HAS_DEDUCED_EXECUTION_IS_EXECUTOR_TRAIT)

namespace asio {
namespace execution {

template <typename ResultType, typename ParamType, typename Result>
struct is_executor<ex_mq_nr<ResultType, ParamType, Result> >
  : asio::true_type
{
};

} // namespace execution
} // namespace asio

#endif // !defined(ASIO_HAS_DEDUCED_EXECUTION_IS_EXECUTOR_TRAIT)

namespace asio {
namespace traits {

#if !defined(ASIO_HAS_DEDUCED_QUERY_MEMBER_TRAIT)

template <typename ResultType, typename ParamType,
  typename Result, typename Param>
struct query_member<
  ex_mq_nr<ResultType, ParamType, Result>, Param,
  typename asio::enable_if<
    asio::is_convertible<Param, ParamType>::value
  >::type>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);

  typedef ResultType result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_QUERY_MEMBER_TRAIT)

} // namespace traits
} // namespace asio

template <typename ResultType, typename ParamType, typename Result>
struct ex_fq_nr
{
  friend ResultType query(const ex_fq_nr&, ParamType) ASIO_NOEXCEPT
  {
    return Result();
  }

  template <typename F>
  void execute(const F&) const
  {
  }

  friend bool operator==(const ex_fq_nr&, const ex_fq_nr&) ASIO_NOEXCEPT
  {
    return true;
  }

  friend bool operator!=(const ex_fq_nr&, const ex_fq_nr&) ASIO_NOEXCEPT
  {
    return false;
  }
};

#if !defined(ASIO_HAS_DEDUCED_EXECUTION_IS_EXECUTOR_TRAIT)

namespace asio {
namespace execution {

template <typename ResultType, typename ParamType, typename Result>
struct is_executor<ex_fq_nr<ResultType, ParamType, Result> >
  : asio::true_type
{
};

} // namespace execution
} // namespace asio

#endif // !defined(ASIO_HAS_DEDUCED_EXECUTION_IS_EXECUTOR_TRAIT)

namespace asio {
namespace traits {

#if !defined(ASIO_HAS_DEDUCED_QUERY_FREE_TRAIT)

template <typename ResultType, typename ParamType,
  typename Result, typename Param>
struct query_free<
  ex_fq_nr<ResultType, ParamType, Result>, Param,
  typename asio::enable_if<
    asio::is_convertible<Param, ParamType>::value
  >::type>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);

  typedef ResultType result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_QUERY_FREE_TRAIT)

} // namespace traits
} // namespace asio

template <typename CurrentType, typename OtherType>
struct ex_mq_mr
{
  CurrentType query(CurrentType) const ASIO_NOEXCEPT
  {
    return CurrentType();
  }

  CurrentType query(OtherType) const ASIO_NOEXCEPT
  {
    return CurrentType();
  }

  ex_mq_mr<CurrentType, OtherType> require(
      CurrentType) const ASIO_NOEXCEPT
  {
    return ex_mq_mr<CurrentType, OtherType>();
  }

  ex_mq_mr<OtherType, CurrentType> require(
      OtherType) const ASIO_NOEXCEPT
  {
    return ex_mq_mr<OtherType, CurrentType>();
  }

  template <typename F>
  void execute(const F&) const
  {
  }

  friend bool operator==(const ex_mq_mr&, const ex_mq_mr&) ASIO_NOEXCEPT
  {
    return true;
  }

  friend bool operator!=(const ex_mq_mr&, const ex_mq_mr&) ASIO_NOEXCEPT
  {
    return false;
  }
};

template <typename CurrentType>
struct ex_mq_mr<CurrentType, CurrentType>
{
  CurrentType query(CurrentType) const ASIO_NOEXCEPT
  {
    return CurrentType();
  }

  ex_mq_mr<CurrentType, CurrentType> require(
      CurrentType) const ASIO_NOEXCEPT
  {
    return ex_mq_mr<CurrentType, CurrentType>();
  }

  template <typename F>
  void execute(const F&) const
  {
  }

  friend bool operator==(const ex_mq_mr&, const ex_mq_mr&) ASIO_NOEXCEPT
  {
    return true;
  }

  friend bool operator!=(const ex_mq_mr&, const ex_mq_mr&) ASIO_NOEXCEPT
  {
    return false;
  }
};

#if !defined(ASIO_HAS_DEDUCED_EXECUTION_IS_EXECUTOR_TRAIT)

namespace asio {
namespace execution {

template <typename CurrentType, typename OtherType>
struct is_executor<ex_mq_mr<CurrentType, OtherType> >
  : asio::true_type
{
};

} // namespace execution
} // namespace asio

#endif // !defined(ASIO_HAS_DEDUCED_EXECUTION_IS_EXECUTOR_TRAIT)

namespace asio {
namespace traits {

#if !defined(ASIO_HAS_DEDUCED_QUERY_MEMBER_TRAIT)

template <typename CurrentType, typename OtherType, typename Param>
struct query_member<
  ex_mq_mr<CurrentType, OtherType>, Param,
  typename asio::enable_if<
    asio::is_convertible<Param, CurrentType>::value
      || asio::is_convertible<Param, OtherType>::value
  >::type>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);

  typedef CurrentType result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_QUERY_MEMBER_TRAIT)

#if !defined(ASIO_HAS_DEDUCED_REQUIRE_MEMBER_TRAIT)

template <typename CurrentType, typename OtherType, typename Param>
struct require_member<
  ex_mq_mr<CurrentType, OtherType>, Param,
  typename asio::enable_if<
    asio::is_convertible<Param, CurrentType>::value
  >::type>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);

  typedef ex_mq_mr<CurrentType, OtherType> result_type;
};

template <typename CurrentType, typename OtherType, typename Param>
struct require_member<
  ex_mq_mr<CurrentType, OtherType>, Param,
  typename asio::enable_if<
    asio::is_convertible<Param, OtherType>::value
      && !asio::is_same<CurrentType, OtherType>::value
  >::type>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);

  typedef ex_mq_mr<OtherType, CurrentType> result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_REQUIRE_MEMBER_TRAIT)

} // namespace traits
} // namespace asio

template <typename CurrentType, typename OtherType>
struct ex_fq_fr
{
  friend CurrentType query(const ex_fq_fr&, CurrentType) ASIO_NOEXCEPT
  {
    return CurrentType();
  }

  friend CurrentType query(const ex_fq_fr&, OtherType) ASIO_NOEXCEPT
  {
    return CurrentType();
  }

  friend ex_fq_fr<CurrentType, OtherType> require(
      const ex_fq_fr&, CurrentType) ASIO_NOEXCEPT
  {
    return ex_fq_fr<CurrentType, OtherType>();
  }

  friend ex_fq_fr<OtherType, CurrentType> require(
      const ex_fq_fr&, OtherType) ASIO_NOEXCEPT
  {
    return ex_fq_fr<OtherType, CurrentType>();
  }

  friend ex_fq_fr<CurrentType, OtherType> prefer(
      const ex_fq_fr&, CurrentType) ASIO_NOEXCEPT
  {
    return ex_fq_fr<CurrentType, OtherType>();
  }

  friend ex_fq_fr<OtherType, CurrentType> prefer(
      const ex_fq_fr&, OtherType) ASIO_NOEXCEPT
  {
    return ex_fq_fr<OtherType, CurrentType>();
  }

  template <typename F>
  void execute(const F&) const
  {
  }

  friend bool operator==(const ex_fq_fr&, const ex_fq_fr&) ASIO_NOEXCEPT
  {
    return true;
  }

  friend bool operator!=(const ex_fq_fr&, const ex_fq_fr&) ASIO_NOEXCEPT
  {
    return false;
  }
};

template <typename CurrentType>
struct ex_fq_fr<CurrentType, CurrentType>
{
  friend CurrentType query(const ex_fq_fr&, CurrentType) ASIO_NOEXCEPT
  {
    return CurrentType();
  }

  friend ex_fq_fr<CurrentType, CurrentType> require(
      const ex_fq_fr&, CurrentType) ASIO_NOEXCEPT
  {
    return ex_fq_fr<CurrentType, CurrentType>();
  }

  friend ex_fq_fr<CurrentType, CurrentType> prefer(
      const ex_fq_fr&, CurrentType) ASIO_NOEXCEPT
  {
    return ex_fq_fr<CurrentType, CurrentType>();
  }

  template <typename F>
  void execute(const F&) const
  {
  }

  friend bool operator==(const ex_fq_fr&, const ex_fq_fr&) ASIO_NOEXCEPT
  {
    return true;
  }

  friend bool operator!=(const ex_fq_fr&, const ex_fq_fr&) ASIO_NOEXCEPT
  {
    return false;
  }
};

#if !defined(ASIO_HAS_DEDUCED_EXECUTION_IS_EXECUTOR_TRAIT)

namespace asio {
namespace execution {

template <typename CurrentType, typename OtherType>
struct is_executor<ex_fq_fr<CurrentType, OtherType> >
  : asio::true_type
{
};

} // namespace execution
} // namespace asio

#endif // !defined(ASIO_HAS_DEDUCED_EXECUTION_IS_EXECUTOR_TRAIT)

namespace asio {
namespace traits {

#if !defined(ASIO_HAS_DEDUCED_QUERY_FREE_TRAIT)

template <typename CurrentType, typename OtherType, typename Param>
struct query_free<
  ex_fq_fr<CurrentType, OtherType>, Param,
  typename asio::enable_if<
    asio::is_convertible<Param, CurrentType>::value
      || asio::is_convertible<Param, OtherType>::value
  >::type>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);

  typedef CurrentType result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_QUERY_FREE_TRAIT)

#if !defined(ASIO_HAS_DEDUCED_REQUIRE_FREE_TRAIT)

template <typename CurrentType, typename OtherType, typename Param>
struct require_free<
  ex_fq_fr<CurrentType, OtherType>, Param,
  typename asio::enable_if<
    asio::is_convertible<Param, CurrentType>::value
  >::type>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);

  typedef ex_fq_fr<CurrentType, OtherType> result_type;
};

template <typename CurrentType, typename OtherType, typename Param>
struct require_free<
  ex_fq_fr<CurrentType, OtherType>, Param,
  typename asio::enable_if<
    asio::is_convertible<Param, OtherType>::value
      && !asio::is_same<CurrentType, OtherType>::value
  >::type>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);

  typedef ex_fq_fr<OtherType, CurrentType> result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_REQUIRE_FREE_TRAIT)

#if !defined(ASIO_HAS_DEDUCED_PREFER_FREE_TRAIT)

template <typename CurrentType, typename OtherType, typename Param>
struct prefer_free<
  ex_fq_fr<CurrentType, OtherType>, Param,
  typename asio::enable_if<
    asio::is_convertible<Param, CurrentType>::value
  >::type>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);

  typedef ex_fq_fr<CurrentType, OtherType> result_type;
};

template <typename CurrentType, typename OtherType, typename Param>
struct prefer_free<
  ex_fq_fr<CurrentType, OtherType>, Param,
  typename asio::enable_if<
    asio::is_convertible<Param, OtherType>::value
      && !asio::is_same<CurrentType, OtherType>::value
  >::type>
{
  ASIO_STATIC_CONSTEXPR(bool, is_valid = true);
  ASIO_STATIC_CONSTEXPR(bool, is_noexcept = true);

  typedef ex_fq_fr<OtherType, CurrentType> result_type;
};

#endif // !defined(ASIO_HAS_DEDUCED_PREFER_FREE_TRAIT)

} // namespace traits
} // namespace asio

template <typename Executor, typename Param, bool ExpectedResult>
void test_can_query()
{
  ASIO_CONSTEXPR bool b1 =
    asio::can_query<Executor, Param>::value;
  ASIO_CHECK(b1 == ExpectedResult);

  ASIO_CONSTEXPR bool b2 =
    asio::can_query<const Executor, Param>::value;
  ASIO_CHECK(b2 == ExpectedResult);

  ASIO_CONSTEXPR bool b3 =
    asio::can_query<Executor&, Param>::value;
  ASIO_CHECK(b3 == ExpectedResult);

  ASIO_CONSTEXPR bool b4 =
    asio::can_query<const Executor&, Param>::value;
  ASIO_CHECK(b4 == ExpectedResult);
}

template <typename Executor, typename Param, typename ExpectedResult>
void test_query()
{
  exec::relationship_t result1 = asio::query(Executor(), Param());
  ASIO_CHECK(result1 == ExpectedResult());

  Executor ex1 = {};
  exec::relationship_t result2 = asio::query(ex1, Param());
  ASIO_CHECK(result2 == ExpectedResult());

  const Executor ex2 = {};
  exec::relationship_t result3 = asio::query(ex2, Param());
  ASIO_CHECK(result3 == ExpectedResult());
}

template <typename Executor, typename Param, typename ExpectedResult>
void test_constexpr_query()
{
#if defined(ASIO_HAS_CONSTEXPR)
  constexpr Executor ex1 = {};
  constexpr exec::relationship_t result1 = asio::query(ex1, Param());
  ASIO_CHECK(result1 == ExpectedResult());
#endif // defined(ASIO_HAS_CONSTEXPR)
}

template <typename Executor, typename Param, bool ExpectedResult>
void test_can_require()
{
  ASIO_CONSTEXPR bool b1 =
    asio::can_require<Executor, Param>::value;
  ASIO_CHECK(b1 == ExpectedResult);

  ASIO_CONSTEXPR bool b2 =
    asio::can_require<const Executor, Param>::value;
  ASIO_CHECK(b2 == ExpectedResult);

  ASIO_CONSTEXPR bool b3 =
    asio::can_require<Executor&, Param>::value;
  ASIO_CHECK(b3 == ExpectedResult);

  ASIO_CONSTEXPR bool b4 =
    asio::can_require<const Executor&, Param>::value;
  ASIO_CHECK(b4 == ExpectedResult);
}

template <typename Executor, typename Param, typename ExpectedResult>
void test_require()
{
  ASIO_CHECK(
      asio::query(
        asio::require(Executor(), Param()),
        Param()) == ExpectedResult());

  Executor ex1 = {};
  ASIO_CHECK(
      asio::query(
        asio::require(ex1, Param()),
        Param()) == ExpectedResult());

  const Executor ex2 = {};
  ASIO_CHECK(
      asio::query(
        asio::require(ex2, Param()),
        Param()) == ExpectedResult());
}

template <typename Executor, typename Param, bool ExpectedResult>
void test_can_prefer()
{
  ASIO_CONSTEXPR bool b1 =
    asio::can_prefer<Executor, Param>::value;
  ASIO_CHECK(b1 == ExpectedResult);

  ASIO_CONSTEXPR bool b2 =
    asio::can_prefer<const Executor, Param>::value;
  ASIO_CHECK(b2 == ExpectedResult);

  ASIO_CONSTEXPR bool b3 =
    asio::can_prefer<Executor&, Param>::value;
  ASIO_CHECK(b3 == ExpectedResult);

  ASIO_CONSTEXPR bool b4 =
    asio::can_prefer<const Executor&, Param>::value;
  ASIO_CHECK(b4 == ExpectedResult);
}

template <typename Executor, typename Param, typename ExpectedResult>
void test_prefer()
{
  ASIO_CHECK(
      s(asio::query(
        asio::prefer(Executor(), Param()),
          s())) == s(ExpectedResult()));

  Executor ex1 = {};
  ASIO_CHECK(
      s(asio::query(
        asio::prefer(ex1, Param()),
          s())) == s(ExpectedResult()));

  const Executor ex2 = {};
  ASIO_CHECK(
      s(asio::query(
        asio::prefer(ex2, Param()),
          s())) == s(ExpectedResult()));
}

void test_vars()
{
  ASIO_CHECK(s() == exec::relationship);
  ASIO_CHECK(n1() == exec::relationship.fork);
  ASIO_CHECK(n2() == exec::relationship.continuation);
}

ASIO_TEST_SUITE
(
  "relationship",

  ASIO_TEST_CASE3(test_can_query<ex_nq_nr, s, true>)
  ASIO_TEST_CASE3(test_can_query<ex_nq_nr, n1, true>)
  ASIO_TEST_CASE3(test_can_query<ex_nq_nr, n2, false>)

  ASIO_TEST_CASE3(test_query<ex_nq_nr, s, n1>)
  ASIO_TEST_CASE3(test_query<ex_nq_nr, n1, n1>)

  ASIO_TEST_CASE3(test_constexpr_query<ex_nq_nr, s, n1>)
  ASIO_TEST_CASE3(test_constexpr_query<ex_nq_nr, n1, n1>)

  ASIO_TEST_CASE3(test_can_require<ex_nq_nr, s, false>)
  ASIO_TEST_CASE3(test_can_require<ex_nq_nr, n1, true>)
  ASIO_TEST_CASE3(test_can_require<ex_nq_nr, n2, false>)

  ASIO_TEST_CASE3(test_require<ex_nq_nr, n1, n1>)

  ASIO_TEST_CASE3(test_can_prefer<ex_nq_nr, s, false>)
  ASIO_TEST_CASE3(test_can_prefer<ex_nq_nr, n1, true>)
  ASIO_TEST_CASE3(test_can_prefer<ex_nq_nr, n2, true>)

  ASIO_TEST_CASE3(test_prefer<ex_nq_nr, n1, n1>)
  ASIO_TEST_CASE3(test_prefer<ex_nq_nr, n2, n1>)

  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<s, s, n1>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<s, s, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<s, s, n1>, n2, true>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<s, s, n2>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<s, s, n2>, n1, true>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<s, s, n2>, n2, true>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<s, n1, n1>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<s, n1, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<s, n1, n1>, n2, false>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<s, n1, n2>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<s, n1, n2>, n1, true>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<s, n1, n2>, n2, false>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<s, n2, n1>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<s, n2, n1>, n1, false>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<s, n2, n1>, n2, true>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<s, n2, n2>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<s, n2, n2>, n1, false>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<s, n2, n2>, n2, true>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<n1, s, n1>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<n1, s, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<n1, s, n1>, n2, true>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<n2, s, n2>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<n2, s, n2>, n1, true>)
  ASIO_TEST_CASE5(test_can_query<ex_cq_nr<n2, s, n2>, n2, true>)

  ASIO_TEST_CASE5(test_query<ex_cq_nr<s, s, n1>, s, n1>)
  ASIO_TEST_CASE5(test_query<ex_cq_nr<s, s, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_query<ex_cq_nr<s, s, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_query<ex_cq_nr<s, s, n2>, s, n2>)
  ASIO_TEST_CASE5(test_query<ex_cq_nr<s, s, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_query<ex_cq_nr<s, s, n2>, n2, n2>)
  ASIO_TEST_CASE5(test_query<ex_cq_nr<s, n1, n1>, s, n1>)
  ASIO_TEST_CASE5(test_query<ex_cq_nr<s, n1, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_query<ex_cq_nr<s, n1, n2>, s, n2>)
  ASIO_TEST_CASE5(test_query<ex_cq_nr<s, n1, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_query<ex_cq_nr<s, n2, n1>, s, n1>)
  ASIO_TEST_CASE5(test_query<ex_cq_nr<s, n2, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_query<ex_cq_nr<s, n2, n2>, s, n2>)
  ASIO_TEST_CASE5(test_query<ex_cq_nr<s, n2, n2>, n2, n2>)
  ASIO_TEST_CASE5(test_query<ex_cq_nr<n1, s, n1>, s, n1>)
  ASIO_TEST_CASE5(test_query<ex_cq_nr<n1, s, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_query<ex_cq_nr<n1, s, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_query<ex_cq_nr<n2, s, n2>, s, n2>)
  ASIO_TEST_CASE5(test_query<ex_cq_nr<n2, s, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_query<ex_cq_nr<n2, s, n2>, n2, n2>)

  ASIO_TEST_CASE5(test_constexpr_query<ex_cq_nr<s, s, n1>, s, n1>)
  ASIO_TEST_CASE5(test_constexpr_query<ex_cq_nr<s, s, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_constexpr_query<ex_cq_nr<s, s, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_constexpr_query<ex_cq_nr<s, s, n2>, s, n2>)
  ASIO_TEST_CASE5(test_constexpr_query<ex_cq_nr<s, s, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_constexpr_query<ex_cq_nr<s, s, n2>, n2, n2>)
  ASIO_TEST_CASE5(test_constexpr_query<ex_cq_nr<s, n1, n1>, s, n1>)
  ASIO_TEST_CASE5(test_constexpr_query<ex_cq_nr<s, n1, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_constexpr_query<ex_cq_nr<s, n1, n2>, s, n2>)
  ASIO_TEST_CASE5(test_constexpr_query<ex_cq_nr<s, n1, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_constexpr_query<ex_cq_nr<s, n2, n1>, s, n1>)
  ASIO_TEST_CASE5(test_constexpr_query<ex_cq_nr<s, n2, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_constexpr_query<ex_cq_nr<s, n2, n2>, s, n2>)
  ASIO_TEST_CASE5(test_constexpr_query<ex_cq_nr<s, n2, n2>, n2, n2>)
  ASIO_TEST_CASE5(test_constexpr_query<ex_cq_nr<n1, s, n1>, s, n1>)
  ASIO_TEST_CASE5(test_constexpr_query<ex_cq_nr<n1, s, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_constexpr_query<ex_cq_nr<n1, s, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_constexpr_query<ex_cq_nr<n2, s, n2>, s, n2>)
  ASIO_TEST_CASE5(test_constexpr_query<ex_cq_nr<n2, s, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_constexpr_query<ex_cq_nr<n2, s, n2>, n2, n2>)

  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<s, s, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<s, s, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<s, s, n1>, n2, false>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<s, s, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<s, s, n2>, n1, false>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<s, s, n2>, n2, true>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<s, n1, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<s, n1, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<s, n1, n1>, n2, false>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<s, n1, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<s, n1, n2>, n1, false>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<s, n1, n2>, n2, false>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<s, n2, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<s, n2, n1>, n1, false>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<s, n2, n1>, n2, false>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<s, n2, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<s, n2, n2>, n1, false>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<s, n2, n2>, n2, true>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<n1, s, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<n1, s, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<n1, s, n1>, n2, false>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<n2, s, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<n2, s, n2>, n1, false>)
  ASIO_TEST_CASE5(test_can_require<ex_cq_nr<n2, s, n2>, n2, true>)

  ASIO_TEST_CASE5(test_require<ex_cq_nr<s, s, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_require<ex_cq_nr<s, s, n2>, n2, n2>)
  ASIO_TEST_CASE5(test_require<ex_cq_nr<s, n1, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_require<ex_cq_nr<s, n2, n2>, n2, n2>)
  ASIO_TEST_CASE5(test_require<ex_cq_nr<n1, s, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_require<ex_cq_nr<n2, s, n2>, n2, n2>)

  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<s, s, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<s, s, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<s, s, n1>, n2, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<s, s, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<s, s, n2>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<s, s, n2>, n2, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<s, n1, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<s, n1, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<s, n1, n1>, n2, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<s, n1, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<s, n1, n2>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<s, n1, n2>, n2, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<s, n2, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<s, n2, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<s, n2, n1>, n2, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<s, n2, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<s, n2, n2>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<s, n2, n2>, n2, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<n1, s, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<n1, s, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<n1, s, n1>, n2, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<n2, s, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<n2, s, n2>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_cq_nr<n2, s, n2>, n2, true>)

  ASIO_TEST_CASE5(test_prefer<ex_cq_nr<s, s, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_cq_nr<s, s, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_cq_nr<s, s, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_prefer<ex_cq_nr<s, s, n2>, n2, n2>)
  ASIO_TEST_CASE5(test_prefer<ex_cq_nr<s, n1, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_cq_nr<s, n1, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_cq_nr<s, n1, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_prefer<ex_cq_nr<s, n1, n2>, n2, n2>)
  ASIO_TEST_CASE5(test_prefer<ex_cq_nr<s, n2, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_cq_nr<s, n2, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_cq_nr<s, n2, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_prefer<ex_cq_nr<s, n2, n2>, n2, n2>)
  ASIO_TEST_CASE5(test_prefer<ex_cq_nr<n1, s, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_cq_nr<n1, s, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_cq_nr<n2, s, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_prefer<ex_cq_nr<n2, s, n2>, n2, n2>)

  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<s, s, n1>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<s, s, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<s, s, n1>, n2, true>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<s, s, n2>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<s, s, n2>, n1, true>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<s, s, n2>, n2, true>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<s, n1, n1>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<s, n1, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<s, n1, n1>, n2, false>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<s, n1, n2>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<s, n1, n2>, n1, true>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<s, n1, n2>, n2, false>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<s, n2, n1>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<s, n2, n1>, n1, false>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<s, n2, n1>, n2, true>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<s, n2, n2>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<s, n2, n2>, n1, false>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<s, n2, n2>, n2, true>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<n1, s, n1>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<n1, s, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<n1, s, n1>, n2, true>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<n2, s, n2>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<n2, s, n2>, n1, true>)
  ASIO_TEST_CASE5(test_can_query<ex_mq_nr<n2, s, n2>, n2, true>)

  ASIO_TEST_CASE5(test_query<ex_mq_nr<s, s, n1>, s, n1>)
  ASIO_TEST_CASE5(test_query<ex_mq_nr<s, s, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_query<ex_mq_nr<s, s, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_query<ex_mq_nr<s, s, n2>, s, n2>)
  ASIO_TEST_CASE5(test_query<ex_mq_nr<s, s, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_query<ex_mq_nr<s, s, n2>, n2, n2>)
  ASIO_TEST_CASE5(test_query<ex_mq_nr<s, n1, n1>, s, n1>)
  ASIO_TEST_CASE5(test_query<ex_mq_nr<s, n1, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_query<ex_mq_nr<s, n1, n2>, s, n2>)
  ASIO_TEST_CASE5(test_query<ex_mq_nr<s, n1, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_query<ex_mq_nr<s, n2, n1>, s, n1>)
  ASIO_TEST_CASE5(test_query<ex_mq_nr<s, n2, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_query<ex_mq_nr<s, n2, n2>, s, n2>)
  ASIO_TEST_CASE5(test_query<ex_mq_nr<s, n2, n2>, n2, n2>)
  ASIO_TEST_CASE5(test_query<ex_mq_nr<n1, s, n1>, s, n1>)
  ASIO_TEST_CASE5(test_query<ex_mq_nr<n1, s, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_query<ex_mq_nr<n1, s, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_query<ex_mq_nr<n2, s, n2>, s, n2>)
  ASIO_TEST_CASE5(test_query<ex_mq_nr<n2, s, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_query<ex_mq_nr<n2, s, n2>, n2, n2>)

  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<s, s, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<s, s, n1>, n1, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<s, s, n1>, n2, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<s, s, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<s, s, n2>, n1, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<s, s, n2>, n2, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<s, n1, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<s, n1, n1>, n1, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<s, n1, n1>, n2, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<s, n1, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<s, n1, n2>, n1, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<s, n1, n2>, n2, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<s, n2, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<s, n2, n1>, n1, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<s, n2, n1>, n2, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<s, n2, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<s, n2, n2>, n1, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<s, n2, n2>, n2, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<n1, s, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<n1, s, n1>, n1, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<n1, s, n1>, n2, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<n2, s, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<n2, s, n2>, n1, false>)
  ASIO_TEST_CASE5(test_can_require<ex_mq_nr<n2, s, n2>, n2, false>)

  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<s, s, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<s, s, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<s, s, n1>, n2, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<s, s, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<s, s, n2>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<s, s, n2>, n2, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<s, n1, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<s, n1, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<s, n1, n1>, n2, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<s, n1, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<s, n1, n2>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<s, n1, n2>, n2, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<s, n2, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<s, n2, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<s, n2, n1>, n2, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<s, n2, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<s, n2, n2>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<s, n2, n2>, n2, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<n1, s, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<n1, s, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<n1, s, n1>, n2, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<n2, s, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<n2, s, n2>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_mq_nr<n2, s, n2>, n2, true>)

  ASIO_TEST_CASE5(test_prefer<ex_mq_nr<s, s, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_mq_nr<s, s, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_mq_nr<s, s, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_prefer<ex_mq_nr<s, s, n2>, n2, n2>)
  ASIO_TEST_CASE5(test_prefer<ex_mq_nr<s, n1, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_mq_nr<s, n1, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_mq_nr<s, n1, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_prefer<ex_mq_nr<s, n1, n2>, n2, n2>)
  ASIO_TEST_CASE5(test_prefer<ex_mq_nr<s, n2, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_mq_nr<s, n2, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_mq_nr<s, n2, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_prefer<ex_mq_nr<s, n2, n2>, n2, n2>)
  ASIO_TEST_CASE5(test_prefer<ex_mq_nr<n1, s, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_mq_nr<n1, s, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_mq_nr<n2, s, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_prefer<ex_mq_nr<n2, s, n2>, n2, n2>)

  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<s, s, n1>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<s, s, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<s, s, n1>, n2, true>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<s, s, n2>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<s, s, n2>, n1, true>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<s, s, n2>, n2, true>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<s, n1, n1>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<s, n1, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<s, n1, n1>, n2, false>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<s, n1, n2>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<s, n1, n2>, n1, true>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<s, n1, n2>, n2, false>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<s, n2, n1>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<s, n2, n1>, n1, false>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<s, n2, n1>, n2, true>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<s, n2, n2>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<s, n2, n2>, n1, false>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<s, n2, n2>, n2, true>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<n1, s, n1>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<n1, s, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<n1, s, n1>, n2, true>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<n2, s, n2>, s, true>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<n2, s, n2>, n1, true>)
  ASIO_TEST_CASE5(test_can_query<ex_fq_nr<n2, s, n2>, n2, true>)

  ASIO_TEST_CASE5(test_query<ex_fq_nr<s, s, n1>, s, n1>)
  ASIO_TEST_CASE5(test_query<ex_fq_nr<s, s, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_query<ex_fq_nr<s, s, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_query<ex_fq_nr<s, s, n2>, s, n2>)
  ASIO_TEST_CASE5(test_query<ex_fq_nr<s, s, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_query<ex_fq_nr<s, s, n2>, n2, n2>)
  ASIO_TEST_CASE5(test_query<ex_fq_nr<s, n1, n1>, s, n1>)
  ASIO_TEST_CASE5(test_query<ex_fq_nr<s, n1, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_query<ex_fq_nr<s, n1, n2>, s, n2>)
  ASIO_TEST_CASE5(test_query<ex_fq_nr<s, n1, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_query<ex_fq_nr<s, n2, n1>, s, n1>)
  ASIO_TEST_CASE5(test_query<ex_fq_nr<s, n2, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_query<ex_fq_nr<s, n2, n2>, s, n2>)
  ASIO_TEST_CASE5(test_query<ex_fq_nr<s, n2, n2>, n2, n2>)
  ASIO_TEST_CASE5(test_query<ex_fq_nr<n1, s, n1>, s, n1>)
  ASIO_TEST_CASE5(test_query<ex_fq_nr<n1, s, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_query<ex_fq_nr<n1, s, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_query<ex_fq_nr<n2, s, n2>, s, n2>)
  ASIO_TEST_CASE5(test_query<ex_fq_nr<n2, s, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_query<ex_fq_nr<n2, s, n2>, n2, n2>)

  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<s, s, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<s, s, n1>, n1, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<s, s, n1>, n2, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<s, s, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<s, s, n2>, n1, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<s, s, n2>, n2, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<s, n1, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<s, n1, n1>, n1, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<s, n1, n1>, n2, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<s, n1, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<s, n1, n2>, n1, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<s, n1, n2>, n2, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<s, n2, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<s, n2, n1>, n1, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<s, n2, n1>, n2, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<s, n2, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<s, n2, n2>, n1, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<s, n2, n2>, n2, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<n1, s, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<n1, s, n1>, n1, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<n1, s, n1>, n2, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<n2, s, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<n2, s, n2>, n1, false>)
  ASIO_TEST_CASE5(test_can_require<ex_fq_nr<n2, s, n2>, n2, false>)

  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<s, s, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<s, s, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<s, s, n1>, n2, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<s, s, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<s, s, n2>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<s, s, n2>, n2, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<s, n1, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<s, n1, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<s, n1, n1>, n2, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<s, n1, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<s, n1, n2>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<s, n1, n2>, n2, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<s, n2, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<s, n2, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<s, n2, n1>, n2, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<s, n2, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<s, n2, n2>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<s, n2, n2>, n2, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<n1, s, n1>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<n1, s, n1>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<n1, s, n1>, n2, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<n2, s, n2>, s, false>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<n2, s, n2>, n1, true>)
  ASIO_TEST_CASE5(test_can_prefer<ex_fq_nr<n2, s, n2>, n2, true>)

  ASIO_TEST_CASE5(test_prefer<ex_fq_nr<s, s, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_fq_nr<s, s, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_fq_nr<s, s, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_prefer<ex_fq_nr<s, s, n2>, n2, n2>)
  ASIO_TEST_CASE5(test_prefer<ex_fq_nr<s, n1, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_fq_nr<s, n1, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_fq_nr<s, n1, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_prefer<ex_fq_nr<s, n1, n2>, n2, n2>)
  ASIO_TEST_CASE5(test_prefer<ex_fq_nr<s, n2, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_fq_nr<s, n2, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_fq_nr<s, n2, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_prefer<ex_fq_nr<s, n2, n2>, n2, n2>)
  ASIO_TEST_CASE5(test_prefer<ex_fq_nr<n1, s, n1>, n1, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_fq_nr<n1, s, n1>, n2, n1>)
  ASIO_TEST_CASE5(test_prefer<ex_fq_nr<n2, s, n2>, n1, n2>)
  ASIO_TEST_CASE5(test_prefer<ex_fq_nr<n2, s, n2>, n2, n2>)

  ASIO_TEST_CASE4(test_can_query<ex_mq_mr<n1, n1>, s, true>)
  ASIO_TEST_CASE4(test_can_query<ex_mq_mr<n1, n1>, n1, true>)
  ASIO_TEST_CASE4(test_can_query<ex_mq_mr<n1, n1>, n2, false>)
  ASIO_TEST_CASE4(test_can_query<ex_mq_mr<n1, n2>, s, true>)
  ASIO_TEST_CASE4(test_can_query<ex_mq_mr<n1, n2>, n1, true>)
  ASIO_TEST_CASE4(test_can_query<ex_mq_mr<n1, n2>, n2, true>)
  ASIO_TEST_CASE4(test_can_query<ex_mq_mr<n2, n1>, s, true>)
  ASIO_TEST_CASE4(test_can_query<ex_mq_mr<n2, n1>, n1, true>)
  ASIO_TEST_CASE4(test_can_query<ex_mq_mr<n2, n1>, n2, true>)
  ASIO_TEST_CASE4(test_can_query<ex_mq_mr<n2, n2>, s, true>)
  ASIO_TEST_CASE4(test_can_query<ex_mq_mr<n2, n2>, n1, false>)
  ASIO_TEST_CASE4(test_can_query<ex_mq_mr<n2, n2>, n2, true>)

  ASIO_TEST_CASE4(test_query<ex_mq_mr<n1, n1>, s, n1>)
  ASIO_TEST_CASE4(test_query<ex_mq_mr<n1, n1>, n1, n1>)
  ASIO_TEST_CASE4(test_query<ex_mq_mr<n1, n2>, s, n1>)
  ASIO_TEST_CASE4(test_query<ex_mq_mr<n1, n2>, n1, n1>)
  ASIO_TEST_CASE4(test_query<ex_mq_mr<n2, n1>, s, n2>)
  ASIO_TEST_CASE4(test_query<ex_mq_mr<n2, n1>, n2, n2>)
  ASIO_TEST_CASE4(test_query<ex_mq_mr<n2, n2>, s, n2>)
  ASIO_TEST_CASE4(test_query<ex_mq_mr<n2, n2>, n2, n2>)

  ASIO_TEST_CASE4(test_can_require<ex_mq_mr<n1, n1>, s, false>)
  ASIO_TEST_CASE4(test_can_require<ex_mq_mr<n1, n1>, n1, true>)
  ASIO_TEST_CASE4(test_can_require<ex_mq_mr<n1, n1>, n2, false>)
  ASIO_TEST_CASE4(test_can_require<ex_mq_mr<n1, n2>, s, false>)
  ASIO_TEST_CASE4(test_can_require<ex_mq_mr<n1, n2>, n1, true>)
  ASIO_TEST_CASE4(test_can_require<ex_mq_mr<n1, n2>, n2, true>)
  ASIO_TEST_CASE4(test_can_require<ex_mq_mr<n2, n1>, s, false>)
  ASIO_TEST_CASE4(test_can_require<ex_mq_mr<n2, n1>, n1, true>)
  ASIO_TEST_CASE4(test_can_require<ex_mq_mr<n2, n1>, n2, true>)
  ASIO_TEST_CASE4(test_can_require<ex_mq_mr<n2, n2>, s, false>)
  ASIO_TEST_CASE4(test_can_require<ex_mq_mr<n2, n2>, n1, false>)
  ASIO_TEST_CASE4(test_can_require<ex_mq_mr<n2, n2>, n2, true>)

  ASIO_TEST_CASE4(test_require<ex_mq_mr<n1, n1>, n1, n1>)
  ASIO_TEST_CASE4(test_require<ex_mq_mr<n1, n2>, n1, n1>)
  ASIO_TEST_CASE4(test_require<ex_mq_mr<n1, n2>, n2, n2>)
  ASIO_TEST_CASE4(test_require<ex_mq_mr<n2, n1>, n1, n1>)
  ASIO_TEST_CASE4(test_require<ex_mq_mr<n2, n1>, n2, n2>)
  ASIO_TEST_CASE4(test_require<ex_mq_mr<n2, n2>, n2, n2>)

  ASIO_TEST_CASE4(test_can_prefer<ex_mq_mr<n1, n1>, s, false>)
  ASIO_TEST_CASE4(test_can_prefer<ex_mq_mr<n1, n1>, n1, true>)
  ASIO_TEST_CASE4(test_can_prefer<ex_mq_mr<n1, n1>, n2, true>)
  ASIO_TEST_CASE4(test_can_prefer<ex_mq_mr<n1, n2>, s, false>)
  ASIO_TEST_CASE4(test_can_prefer<ex_mq_mr<n1, n2>, n1, true>)
  ASIO_TEST_CASE4(test_can_prefer<ex_mq_mr<n1, n2>, n2, true>)
  ASIO_TEST_CASE4(test_can_prefer<ex_mq_mr<n2, n1>, s, false>)
  ASIO_TEST_CASE4(test_can_prefer<ex_mq_mr<n2, n1>, n1, true>)
  ASIO_TEST_CASE4(test_can_prefer<ex_mq_mr<n2, n1>, n2, true>)
  ASIO_TEST_CASE4(test_can_prefer<ex_mq_mr<n2, n2>, s, false>)
  ASIO_TEST_CASE4(test_can_prefer<ex_mq_mr<n2, n2>, n1, true>)
  ASIO_TEST_CASE4(test_can_prefer<ex_mq_mr<n2, n2>, n2, true>)

  ASIO_TEST_CASE4(test_prefer<ex_mq_mr<n1, n1>, n1, n1>)
  ASIO_TEST_CASE4(test_prefer<ex_mq_mr<n1, n1>, n2, n1>)
  ASIO_TEST_CASE4(test_prefer<ex_mq_mr<n1, n2>, n1, n1>)
  ASIO_TEST_CASE4(test_prefer<ex_mq_mr<n1, n2>, n2, n2>)
  ASIO_TEST_CASE4(test_prefer<ex_mq_mr<n2, n1>, n1, n1>)
  ASIO_TEST_CASE4(test_prefer<ex_mq_mr<n2, n1>, n2, n2>)
  ASIO_TEST_CASE4(test_prefer<ex_mq_mr<n2, n2>, n1, n2>)
  ASIO_TEST_CASE4(test_prefer<ex_mq_mr<n2, n2>, n2, n2>)

  ASIO_TEST_CASE4(test_can_query<ex_fq_fr<n1, n1>, s, true>)
  ASIO_TEST_CASE4(test_can_query<ex_fq_fr<n1, n1>, n1, true>)
  ASIO_TEST_CASE4(test_can_query<ex_fq_fr<n1, n1>, n2, false>)
  ASIO_TEST_CASE4(test_can_query<ex_fq_fr<n1, n2>, s, true>)
  ASIO_TEST_CASE4(test_can_query<ex_fq_fr<n1, n2>, n1, true>)
  ASIO_TEST_CASE4(test_can_query<ex_fq_fr<n1, n2>, n2, true>)
  ASIO_TEST_CASE4(test_can_query<ex_fq_fr<n2, n1>, s, true>)
  ASIO_TEST_CASE4(test_can_query<ex_fq_fr<n2, n1>, n1, true>)
  ASIO_TEST_CASE4(test_can_query<ex_fq_fr<n2, n1>, n2, true>)
  ASIO_TEST_CASE4(test_can_query<ex_fq_fr<n2, n2>, s, true>)
  ASIO_TEST_CASE4(test_can_query<ex_fq_fr<n2, n2>, n1, false>)
  ASIO_TEST_CASE4(test_can_query<ex_fq_fr<n2, n2>, n2, true>)

  ASIO_TEST_CASE4(test_query<ex_fq_fr<n1, n1>, s, n1>)
  ASIO_TEST_CASE4(test_query<ex_fq_fr<n1, n1>, n1, n1>)
  ASIO_TEST_CASE4(test_query<ex_fq_fr<n1, n2>, s, n1>)
  ASIO_TEST_CASE4(test_query<ex_fq_fr<n1, n2>, n1, n1>)
  ASIO_TEST_CASE4(test_query<ex_fq_fr<n2, n1>, s, n2>)
  ASIO_TEST_CASE4(test_query<ex_fq_fr<n2, n1>, n2, n2>)
  ASIO_TEST_CASE4(test_query<ex_fq_fr<n2, n2>, s, n2>)
  ASIO_TEST_CASE4(test_query<ex_fq_fr<n2, n2>, n2, n2>)

  ASIO_TEST_CASE4(test_can_require<ex_fq_fr<n1, n1>, s, false>)
  ASIO_TEST_CASE4(test_can_require<ex_fq_fr<n1, n1>, n1, true>)
  ASIO_TEST_CASE4(test_can_require<ex_fq_fr<n1, n1>, n2, false>)
  ASIO_TEST_CASE4(test_can_require<ex_fq_fr<n1, n2>, s, false>)
  ASIO_TEST_CASE4(test_can_require<ex_fq_fr<n1, n2>, n1, true>)
  ASIO_TEST_CASE4(test_can_require<ex_fq_fr<n1, n2>, n2, true>)
  ASIO_TEST_CASE4(test_can_require<ex_fq_fr<n2, n1>, s, false>)
  ASIO_TEST_CASE4(test_can_require<ex_fq_fr<n2, n1>, n1, true>)
  ASIO_TEST_CASE4(test_can_require<ex_fq_fr<n2, n1>, n2, true>)
  ASIO_TEST_CASE4(test_can_require<ex_fq_fr<n2, n2>, s, false>)
  ASIO_TEST_CASE4(test_can_require<ex_fq_fr<n2, n2>, n1, false>)
  ASIO_TEST_CASE4(test_can_require<ex_fq_fr<n2, n2>, n2, true>)

  ASIO_TEST_CASE4(test_require<ex_fq_fr<n1, n1>, n1, n1>)
  ASIO_TEST_CASE4(test_require<ex_fq_fr<n1, n2>, n1, n1>)
  ASIO_TEST_CASE4(test_require<ex_fq_fr<n1, n2>, n2, n2>)
  ASIO_TEST_CASE4(test_require<ex_fq_fr<n2, n1>, n1, n1>)
  ASIO_TEST_CASE4(test_require<ex_fq_fr<n2, n1>, n2, n2>)
  ASIO_TEST_CASE4(test_require<ex_fq_fr<n2, n2>, n2, n2>)

  ASIO_TEST_CASE4(test_can_prefer<ex_fq_fr<n1, n1>, s, false>)
  ASIO_TEST_CASE4(test_can_prefer<ex_fq_fr<n1, n1>, n1, true>)
  ASIO_TEST_CASE4(test_can_prefer<ex_fq_fr<n1, n1>, n2, true>)
  ASIO_TEST_CASE4(test_can_prefer<ex_fq_fr<n1, n2>, s, false>)
  ASIO_TEST_CASE4(test_can_prefer<ex_fq_fr<n1, n2>, n1, true>)
  ASIO_TEST_CASE4(test_can_prefer<ex_fq_fr<n1, n2>, n2, true>)
  ASIO_TEST_CASE4(test_can_prefer<ex_fq_fr<n2, n1>, s, false>)
  ASIO_TEST_CASE4(test_can_prefer<ex_fq_fr<n2, n1>, n1, true>)
  ASIO_TEST_CASE4(test_can_prefer<ex_fq_fr<n2, n1>, n2, true>)
  ASIO_TEST_CASE4(test_can_prefer<ex_fq_fr<n2, n2>, s, false>)
  ASIO_TEST_CASE4(test_can_prefer<ex_fq_fr<n2, n2>, n1, true>)
  ASIO_TEST_CASE4(test_can_prefer<ex_fq_fr<n2, n2>, n2, true>)

  ASIO_TEST_CASE4(test_prefer<ex_fq_fr<n1, n1>, n1, n1>)
  ASIO_TEST_CASE4(test_prefer<ex_fq_fr<n1, n1>, n2, n1>)
  ASIO_TEST_CASE4(test_prefer<ex_fq_fr<n1, n2>, n1, n1>)
  ASIO_TEST_CASE4(test_prefer<ex_fq_fr<n1, n2>, n2, n2>)
  ASIO_TEST_CASE4(test_prefer<ex_fq_fr<n2, n1>, n1, n1>)
  ASIO_TEST_CASE4(test_prefer<ex_fq_fr<n2, n1>, n2, n2>)
  ASIO_TEST_CASE4(test_prefer<ex_fq_fr<n2, n2>, n1, n2>)
  ASIO_TEST_CASE4(test_prefer<ex_fq_fr<n2, n2>, n2, n2>)

  ASIO_TEST_CASE(test_vars)
)
