//
// connect_pipe.cpp
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
#include "asio/connect_pipe.hpp"

#include <string>
#include "asio/io_context.hpp"
#include "asio/read.hpp"
#include "asio/readable_pipe.hpp"
#include "asio/writable_pipe.hpp"
#include "asio/write.hpp"
#include "unit_test.hpp"

#if defined(ASIO_HAS_BOOST_BIND)
# include <boost/bind/bind.hpp>
#else // defined(ASIO_HAS_BOOST_BIND)
# include <functional>
#endif // defined(ASIO_HAS_BOOST_BIND)

//------------------------------------------------------------------------------

// connect_pipe_compile test
// ~~~~~~~~~~~~~~~~~~~~~~~~~
// The following test checks that all connect_pipe functions compile and link
// correctly. Runtime failures are ignored.

namespace connect_pipe_compile {

void test()
{
#if defined(ASIO_HAS_PIPE)
  using namespace asio;

  try
  {
    asio::io_context io_context;
    asio::error_code ec1;

    readable_pipe p1(io_context);
    writable_pipe p2(io_context);
    connect_pipe(p1, p2);

    readable_pipe p3(io_context);
    writable_pipe p4(io_context);
    connect_pipe(p3, p4, ec1);
  }
  catch (std::exception&)
  {
  }
#endif // defined(ASIO_HAS_PIPE)
}

} // namespace connect_pipe_compile

//------------------------------------------------------------------------------

// connect_pipe_runtime test
// ~~~~~~~~~~~~~~~~~~~~~~~~~
// The following test checks that connect_pipe operates correctly at runtime.

namespace connect_pipe_runtime {

static const char write_data[]
  = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

void handle_read(const asio::error_code& err,
    size_t bytes_transferred, bool* called)
{
  *called = true;
  ASIO_CHECK(!err);
  ASIO_CHECK(bytes_transferred == sizeof(write_data));
}

void handle_write(const asio::error_code& err,
    size_t bytes_transferred, bool* called)
{
  *called = true;
  ASIO_CHECK(!err);
  ASIO_CHECK(bytes_transferred == sizeof(write_data));
}

void test()
{
#if defined(ASIO_HAS_PIPE)
  using namespace std; // For memcmp.
  using namespace asio;

#if defined(ASIO_HAS_BOOST_BIND)
  namespace bindns = boost;
#else // defined(ASIO_HAS_BOOST_BIND)
  namespace bindns = std;
#endif // defined(ASIO_HAS_BOOST_BIND)
  using bindns::placeholders::_1;
  using bindns::placeholders::_2;

  try
  {
    asio::io_context io_context;
    asio::error_code ec1;
    asio::error_code ec2;

    readable_pipe p1(io_context);
    writable_pipe p2(io_context);
    connect_pipe(p1, p2);

    std::string data1 = write_data;
    asio::write(p2, asio::buffer(data1));

    std::string data2;
    data2.resize(data1.size());
    asio::read(p1, asio::buffer(data2));

    ASIO_CHECK(data1 == data2);

    char read_buffer[sizeof(write_data)];
    bool read_completed = false;
    asio::async_read(p1,
        asio::buffer(read_buffer),
        bindns::bind(handle_read,
          _1, _2, &read_completed));

    bool write_completed = false;
    asio::async_write(p2,
        asio::buffer(write_data),
        bindns::bind(handle_write,
          _1, _2, &write_completed));

    io_context.run();

    ASIO_CHECK(read_completed);
    ASIO_CHECK(write_completed);
    ASIO_CHECK(memcmp(read_buffer, write_data, sizeof(write_data)) == 0);
  }
  catch (std::exception&)
  {
    ASIO_CHECK(false);
  }
#endif // defined(ASIO_HAS_PIPE)
}

} // namespace connect_pipe_compile

//------------------------------------------------------------------------------

ASIO_TEST_SUITE
(
  "connect_pipe",
  ASIO_COMPILE_TEST_CASE(connect_pipe_compile::test)
  ASIO_TEST_CASE(connect_pipe_runtime::test)
)
