//
// experimental/channel.cpp
// ~~~~~~~~~~~~~~~~~~~~~~~~
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
#include "asio/experimental/channel.hpp"

#include <utility>
#include "asio/bind_executor.hpp"
#include "asio/bind_immediate_executor.hpp"
#include "asio/error.hpp"
#include "asio/io_context.hpp"
#include "asio/system_executor.hpp"
#include "../unit_test.hpp"

using namespace asio;
using namespace asio::experimental;

void unbuffered_channel_test()
{
  io_context ctx;

  channel<void(asio::error_code, std::string)> ch1(ctx);

  ASIO_CHECK(ch1.is_open());
  ASIO_CHECK(!ch1.ready());

  bool b1 = ch1.try_send(asio::error::eof, "hello");

  ASIO_CHECK(!b1);

  std::string s1 = "abcdefghijklmnopqrstuvwxyz";
  bool b2 = ch1.try_send(asio::error::eof, std::move(s1));

  ASIO_CHECK(!b2);
  ASIO_CHECK(!s1.empty());

  asio::error_code ec1;
  std::string s2;
  ch1.async_receive(
      [&](asio::error_code ec, std::string s)
      {
        ec1 = ec;
        s2 = std::move(s);
      });

  bool b3 = ch1.try_send(asio::error::eof, std::move(s1));

  ASIO_CHECK(b3);
  ASIO_CHECK(s1.empty());

  ctx.run();

  ASIO_CHECK(ec1 == asio::error::eof);
  ASIO_CHECK(s2 == "abcdefghijklmnopqrstuvwxyz");

  bool b4 = ch1.try_receive([](asio::error_code, std::string){});

  ASIO_CHECK(!b4);

  asio::error_code ec2 = asio::error::would_block;
  std::string s3 = "zyxwvutsrqponmlkjihgfedcba";
  ch1.async_send(asio::error::eof, std::move(s3),
      [&](asio::error_code ec)
      {
        ec2 = ec;
      });

  asio::error_code ec3;
  std::string s4;
  bool b5 = ch1.try_receive(
      [&](asio::error_code ec, std::string s)
      {
        ec3 = ec;
        s4 = s;
      });

  ASIO_CHECK(b5);
  ASIO_CHECK(ec3 == asio::error::eof);
  ASIO_CHECK(s4 == "zyxwvutsrqponmlkjihgfedcba");

  ctx.restart();
  ctx.run();

  ASIO_CHECK(!ec2);
}

void buffered_channel_test()
{
  io_context ctx;

  channel<void(asio::error_code, std::string)> ch1(ctx, 1);

  ASIO_CHECK(ch1.is_open());
  ASIO_CHECK(!ch1.ready());

  bool b1 = ch1.try_send(asio::error::eof, "hello");

  ASIO_CHECK(b1);

  std::string s1 = "abcdefghijklmnopqrstuvwxyz";
  bool b2 = ch1.try_send(asio::error::eof, std::move(s1));

  ASIO_CHECK(!b2);
  ASIO_CHECK(!s1.empty());

  asio::error_code ec1;
  std::string s2;
  ch1.async_receive(
      [&](asio::error_code ec, std::string s)
      {
        ec1 = ec;
        s2 = std::move(s);
      });

  ctx.run();

  ASIO_CHECK(ec1 == asio::error::eof);
  ASIO_CHECK(s2 == "hello");

  bool b4 = ch1.try_receive([](asio::error_code, std::string){});

  ASIO_CHECK(!b4);

  asio::error_code ec2 = asio::error::would_block;
  std::string s3 = "zyxwvutsrqponmlkjihgfedcba";
  ch1.async_send(asio::error::eof, std::move(s3),
      [&](asio::error_code ec)
      {
        ec2 = ec;
      });

  asio::error_code ec3;
  std::string s4;
  bool b5 = ch1.try_receive(
      [&](asio::error_code ec, std::string s)
      {
        ec3 = ec;
        s4 = s;
      });

  ASIO_CHECK(b5);
  ASIO_CHECK(ec3 == asio::error::eof);
  ASIO_CHECK(s4 == "zyxwvutsrqponmlkjihgfedcba");

  ctx.restart();
  ctx.run();

  ASIO_CHECK(!ec2);

  bool b6 = ch1.try_send(asio::error_code(), "goodbye");

  ASIO_CHECK(b6);

  ch1.close();

  asio::error_code ec4;
  std::string s5;
  ch1.async_receive(
      [&](asio::error_code ec, std::string s)
      {
        ec4 = ec;
        s5 = std::move(s);
      });

  ctx.restart();
  ctx.run();

  ASIO_CHECK(!ec4);
  ASIO_CHECK(s5 == "goodbye");

  asio::error_code ec5;
  std::string s6;
  ch1.async_receive(
      [&](asio::error_code ec, std::string s)
      {
        ec5 = ec;
        s6 = std::move(s);
      });

  ctx.restart();
  ctx.run();

  ASIO_CHECK(ec5 == asio::experimental::channel_errc::channel_closed);
  ASIO_CHECK(s6.empty());
}

void buffered_error_channel_test()
{
  io_context ctx;

  channel<void(asio::error_code)> ch1(ctx, 1);

  ASIO_CHECK(ch1.is_open());
  ASIO_CHECK(!ch1.ready());

  bool b1 = ch1.try_send(asio::error::eof);

  ASIO_CHECK(b1);

  bool b2 = ch1.try_send(asio::error::eof);

  ASIO_CHECK(!b2);

  asio::error_code ec1;
  ch1.async_receive(
      [&](asio::error_code ec)
      {
        ec1 = ec;
      });

  ctx.run();

  ASIO_CHECK(ec1 == asio::error::eof);

  bool b4 = ch1.try_receive([](asio::error_code){});

  ASIO_CHECK(!b4);

  asio::error_code ec2 = asio::error::would_block;
  ch1.async_send(asio::error::eof,
      [&](asio::error_code ec)
      {
        ec2 = ec;
      });

  asio::error_code ec3;
  bool b5 = ch1.try_receive(
      [&](asio::error_code ec)
      {
        ec3 = ec;
      });

  ASIO_CHECK(b5);
  ASIO_CHECK(ec3 == asio::error::eof);

  ctx.restart();
  ctx.run();

  ASIO_CHECK(!ec2);
}

void unbuffered_non_immediate_receive()
{
  io_context ctx;

  channel<void(asio::error_code, std::string)> ch1(ctx);

  asio::error_code ec1 = asio::error::would_block;
  std::string s1 = "0123456789";
  ch1.async_send(asio::error::eof, std::move(s1),
      [&](asio::error_code ec)
      {
        ec1 = ec;
      });

  ASIO_CHECK(ec1 == asio::error::would_block);

  asio::error_code ec2 = asio::error::would_block;
  std::string s2;
  ch1.async_receive(
      [&](asio::error_code ec, std::string s)
      {
        ec2 = ec;
        s2 = std::move(s);
      });

  ASIO_CHECK(ec2 == asio::error::would_block);

  ctx.run();

  ASIO_CHECK(!ec1);
  ASIO_CHECK(ec2 == asio::error::eof);
  ASIO_CHECK(s2 == "0123456789");
}

void unbuffered_immediate_receive()
{
  io_context ctx;

  channel<void(asio::error_code, std::string)> ch1(ctx);

  asio::error_code ec1 = asio::error::would_block;
  std::string s1 = "0123456789";
  ch1.async_send(asio::error::eof, std::move(s1),
      [&](asio::error_code ec)
      {
        ec1 = ec;
      });

  ASIO_CHECK(ec1 == asio::error::would_block);

  asio::error_code ec2 = asio::error::would_block;
  std::string s2;
  ch1.async_receive(
      bind_immediate_executor(system_executor(),
        [&](asio::error_code ec, std::string s)
        {
          ec2 = ec;
          s2 = std::move(s);
        }));

  ASIO_CHECK(ec1 == asio::error::would_block);
  ASIO_CHECK(ec2 == asio::error::eof);
  ASIO_CHECK(s2 == "0123456789");

  ctx.run();

  ASIO_CHECK(!ec1);
}

void unbuffered_executor_receive()
{
  io_context ctx;
  io_context ctx2;

  channel<void(asio::error_code, std::string)> ch1(ctx);

  asio::error_code ec1 = asio::error::would_block;
  std::string s1 = "0123456789";
  ch1.async_send(asio::error::eof, std::move(s1),
      [&](asio::error_code ec)
      {
        ec1 = ec;
      });

  ASIO_CHECK(ec1 == asio::error::would_block);

  asio::error_code ec2 = asio::error::would_block;
  std::string s2;
  ch1.async_receive(
      bind_executor(ctx2,
        [&](asio::error_code ec, std::string s)
        {
          ec2 = ec;
          s2 = std::move(s);
        }));

  ASIO_CHECK(ec1 == asio::error::would_block);
  ASIO_CHECK(ec2 == asio::error::would_block);

  ctx.run();

  ASIO_CHECK(!ec1);
  ASIO_CHECK(ec2 == asio::error::would_block);

  ctx2.run();

  ASIO_CHECK(ec2 == asio::error::eof);
  ASIO_CHECK(s2 == "0123456789");
}

void unbuffered_non_immediate_send()
{
  io_context ctx;

  channel<void(asio::error_code, std::string)> ch1(ctx);

  asio::error_code ec1 = asio::error::would_block;
  std::string s1;
  ch1.async_receive(
      [&](asio::error_code ec, std::string s)
      {
        ec1 = ec;
        s1 = std::move(s);
      });

  ASIO_CHECK(ec1 == asio::error::would_block);

  asio::error_code ec2 = asio::error::would_block;
  std::string s2 = "0123456789";
  ch1.async_send(asio::error::eof, std::move(s2),
      [&](asio::error_code ec)
      {
        ec2 = ec;
      });

  ASIO_CHECK(ec2 == asio::error::would_block);

  ctx.run();

  ASIO_CHECK(ec1 == asio::error::eof);
  ASIO_CHECK(s1 == "0123456789");
  ASIO_CHECK(!ec2);
}

void unbuffered_immediate_send()
{
  io_context ctx;

  channel<void(asio::error_code, std::string)> ch1(ctx);

  asio::error_code ec1 = asio::error::would_block;
  std::string s1;
  ch1.async_receive(
      [&](asio::error_code ec, std::string s)
      {
        ec1 = ec;
        s1 = std::move(s);
      });

  ASIO_CHECK(ec1 == asio::error::would_block);

  asio::error_code ec2 = asio::error::would_block;
  std::string s2 = "0123456789";
  ch1.async_send(asio::error::eof, std::move(s2),
      bind_immediate_executor(system_executor(),
        [&](asio::error_code ec)
        {
          ec2 = ec;
        }));

  ASIO_CHECK(ec1 == asio::error::would_block);
  ASIO_CHECK(!ec2);

  ctx.run();

  ASIO_CHECK(ec1 == asio::error::eof);
  ASIO_CHECK(s1 == "0123456789");
}

void unbuffered_executor_send()
{
  io_context ctx;
  io_context ctx2;

  channel<void(asio::error_code, std::string)> ch1(ctx);

  asio::error_code ec1 = asio::error::would_block;
  std::string s1;
  ch1.async_receive(
      [&](asio::error_code ec, std::string s)
      {
        ec1 = ec;
        s1 = std::move(s);
      });

  ASIO_CHECK(ec1 == asio::error::would_block);

  asio::error_code ec2 = asio::error::would_block;
  std::string s2 = "0123456789";
  ch1.async_send(asio::error::eof, std::move(s2),
      bind_executor(ctx2,
        [&](asio::error_code ec)
        {
          ec2 = ec;
        }));

  ASIO_CHECK(ec1 == asio::error::would_block);
  ASIO_CHECK(ec2 == asio::error::would_block);

  ctx.run();

  ASIO_CHECK(ec1 == asio::error::eof);
  ASIO_CHECK(s1 == "0123456789");
  ASIO_CHECK(ec2 == asio::error::would_block);

  ctx2.run();

  ASIO_CHECK(!ec2);
}

void buffered_non_immediate_receive()
{
  io_context ctx;

  channel<void(asio::error_code, std::string)> ch1(ctx, 1);

  asio::error_code ec1 = asio::error::would_block;
  std::string s1 = "0123456789";
  ch1.async_send(asio::error::eof, std::move(s1),
      [&](asio::error_code ec)
      {
        ec1 = ec;
      });

  ASIO_CHECK(ec1 == asio::error::would_block);

  ctx.run();

  ASIO_CHECK(!ec1);

  asio::error_code ec2 = asio::error::would_block;
  std::string s2;
  ch1.async_receive(
      [&](asio::error_code ec, std::string s)
      {
        ec2 = ec;
        s2 = std::move(s);
      });

  ASIO_CHECK(ec2 == asio::error::would_block);

  ctx.restart();
  ctx.run();

  ASIO_CHECK(ec2 == asio::error::eof);
  ASIO_CHECK(s2 == "0123456789");
}

void buffered_immediate_receive()
{
  io_context ctx;

  channel<void(asio::error_code, std::string)> ch1(ctx, 1);

  asio::error_code ec1 = asio::error::would_block;
  std::string s1 = "0123456789";
  ch1.async_send(asio::error::eof, std::move(s1),
      [&](asio::error_code ec)
      {
        ec1 = ec;
      });

  ASIO_CHECK(ec1 == asio::error::would_block);

  ctx.run();

  ASIO_CHECK(!ec1);

  asio::error_code ec2 = asio::error::would_block;
  std::string s2;
  ch1.async_receive(
      bind_immediate_executor(system_executor(),
        [&](asio::error_code ec, std::string s)
        {
          ec2 = ec;
          s2 = std::move(s);
        }));

  ASIO_CHECK(ec2 == asio::error::eof);
  ASIO_CHECK(s2 == "0123456789");

  ctx.restart();
  ctx.run();
}

void buffered_executor_receive()
{
  io_context ctx;
  io_context ctx2;

  channel<void(asio::error_code, std::string)> ch1(ctx, 1);

  asio::error_code ec1 = asio::error::would_block;
  std::string s1 = "0123456789";
  ch1.async_send(asio::error::eof, std::move(s1),
      [&](asio::error_code ec)
      {
        ec1 = ec;
      });

  ASIO_CHECK(ec1 == asio::error::would_block);

  ctx.run();

  ASIO_CHECK(!ec1);

  asio::error_code ec2 = asio::error::would_block;
  std::string s2;
  ch1.async_receive(
      bind_executor(ctx2,
        [&](asio::error_code ec, std::string s)
        {
          ec2 = ec;
          s2 = std::move(s);
        }));

  ASIO_CHECK(ec2 == asio::error::would_block);

  ctx.restart();
  ctx.run();

  ASIO_CHECK(ec2 == asio::error::would_block);

  ctx2.run();

  ASIO_CHECK(ec2 == asio::error::eof);
  ASIO_CHECK(s2 == "0123456789");
}

void buffered_non_immediate_send()
{
  io_context ctx;

  channel<void(asio::error_code, std::string)> ch1(ctx, 1);

  asio::error_code ec1 = asio::error::would_block;
  std::string s1 = "0123456789";
  ch1.async_send(asio::error::eof, std::move(s1),
      [&](asio::error_code ec)
      {
        ec1 = ec;
      });

  ASIO_CHECK(ec1 == asio::error::would_block);

  ctx.run();

  ASIO_CHECK(!ec1);
}

void buffered_immediate_send()
{
  io_context ctx;

  channel<void(asio::error_code, std::string)> ch1(ctx, 1);

  asio::error_code ec1 = asio::error::would_block;
  std::string s1 = "0123456789";
  ch1.async_send(asio::error::eof, std::move(s1),
      bind_immediate_executor(system_executor(),
        [&](asio::error_code ec)
        {
          ec1 = ec;
        }));

  ASIO_CHECK(!ec1);

  ctx.run();
}

void buffered_executor_send()
{
  io_context ctx;
  io_context ctx2;

  channel<void(asio::error_code, std::string)> ch1(ctx, 1);

  asio::error_code ec1 = asio::error::would_block;
  std::string s1 = "0123456789";
  ch1.async_send(asio::error::eof, std::move(s1),
      bind_executor(ctx2,
        [&](asio::error_code ec)
        {
          ec1 = ec;
        }));

  ASIO_CHECK(ec1 == asio::error::would_block);

  ctx.run();

  ASIO_CHECK(ec1 == asio::error::would_block);

  ctx2.run();

  ASIO_CHECK(!ec1);
}

ASIO_TEST_SUITE
(
  "experimental/channel",
  ASIO_TEST_CASE(unbuffered_channel_test)
  ASIO_TEST_CASE(buffered_channel_test)
  ASIO_TEST_CASE(buffered_error_channel_test)
  ASIO_TEST_CASE(unbuffered_non_immediate_receive)
  ASIO_TEST_CASE(unbuffered_immediate_receive)
  ASIO_TEST_CASE(unbuffered_executor_receive)
  ASIO_TEST_CASE(unbuffered_non_immediate_send)
  ASIO_TEST_CASE(unbuffered_immediate_send)
  ASIO_TEST_CASE(unbuffered_executor_send)
  ASIO_TEST_CASE(buffered_non_immediate_receive)
  ASIO_TEST_CASE(buffered_immediate_receive)
  ASIO_TEST_CASE(buffered_executor_receive)
  ASIO_TEST_CASE(buffered_non_immediate_send)
  ASIO_TEST_CASE(buffered_immediate_send)
  ASIO_TEST_CASE(buffered_executor_send)
)
