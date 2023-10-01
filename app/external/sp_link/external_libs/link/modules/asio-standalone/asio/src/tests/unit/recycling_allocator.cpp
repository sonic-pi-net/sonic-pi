//
// recycling_allocator.cpp
// ~~~~~~~~~~~~~~~~~~~~~~~
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
#include "asio/recycling_allocator.hpp"

#include "unit_test.hpp"
#include <vector>
#include "asio/detail/type_traits.hpp"

void recycling_allocator_test()
{
  ASIO_CHECK((
      asio::is_same<
        asio::recycling_allocator<int>::value_type,
        int
      >::value));

  ASIO_CHECK((
      asio::is_same<
        asio::recycling_allocator<void>::value_type,
        void
      >::value));

  ASIO_CHECK((
      asio::is_same<
        asio::recycling_allocator<int>::rebind<char>::other,
        asio::recycling_allocator<char>
      >::value));

  ASIO_CHECK((
      asio::is_same<
        asio::recycling_allocator<void>::rebind<char>::other,
        asio::recycling_allocator<char>
      >::value));

  asio::recycling_allocator<int> a1;
  asio::recycling_allocator<int> a2(a1);

  ASIO_CHECK(a1 == a2);
  ASIO_CHECK(!(a1 != a2));

  asio::recycling_allocator<void> a3;
  asio::recycling_allocator<void> a4(a3);

  ASIO_CHECK(a3 == a4);
  ASIO_CHECK(!(a3 != a4));

  asio::recycling_allocator<int> a5(a4);
  (void)a5;

  asio::recycling_allocator<void> a6(a5);
  (void)a6;

  int* p = a1.allocate(42);
  ASIO_CHECK(p != 0);

  a1.deallocate(p, 42);

#if defined(ASIO_HAS_CXX11_ALLOCATORS)
  std::vector<int, asio::recycling_allocator<int> > v(42);
  ASIO_CHECK(v.size() == 42);
#endif // defined(ASIO_HAS_CXX11_ALLOCATORS)
}

ASIO_TEST_SUITE
(
  "recycling_allocator",
  ASIO_TEST_CASE(recycling_allocator_test)
)
