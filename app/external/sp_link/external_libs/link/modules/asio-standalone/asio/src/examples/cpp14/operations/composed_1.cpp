//
// composed_1.cpp
// ~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include <asio/deferred.hpp>
#include <asio/io_context.hpp>
#include <asio/ip/tcp.hpp>
#include <asio/use_future.hpp>
#include <asio/write.hpp>
#include <cstring>
#include <iostream>
#include <string>
#include <type_traits>
#include <utility>

using asio::ip::tcp;

//------------------------------------------------------------------------------

// This is the simplest example of a composed asynchronous operation, where we
// simply repackage an existing operation. The asynchronous operation
// requirements are met by delegating responsibility to the underlying
// operation.

template <typename CompletionToken>
auto async_write_message(tcp::socket& socket,
    const char* message, CompletionToken&& token)
  // The return type of the initiating function is deduced from the combination
  // of:
  //
  // - the CompletionToken type,
  // - the completion handler signature, and
  // - the asynchronous operation's initiation function object.
  //
  // When the completion token is a simple callback, the return type is void.
  // However, when the completion token is asio::yield_context (used for
  // stackful coroutines) the return type would be std::size_t, and when the
  // completion token is asio::use_future it would be std::future<std::size_t>.
  // When the completion token is asio::deferred, the return type differs for
  // each asynchronous operation.
  //
  // In C++14 we can omit the return type as it is automatically deduced from
  // the return type of our underlying asynchronous operation.
{
  // When delegating to the underlying operation we must take care to perfectly
  // forward the completion token. This ensures that our operation works
  // correctly with move-only function objects as callbacks, as well as other
  // completion token types.
  return asio::async_write(socket,
      asio::buffer(message, std::strlen(message)),
      std::forward<CompletionToken>(token));
}

//------------------------------------------------------------------------------

void test_callback()
{
  asio::io_context io_context;

  tcp::acceptor acceptor(io_context, {tcp::v4(), 55555});
  tcp::socket socket = acceptor.accept();

  // Test our asynchronous operation using a lambda as a callback.
  async_write_message(socket, "Testing callback\r\n",
      [](const std::error_code& error, std::size_t n)
      {
        if (!error)
        {
          std::cout << n << " bytes transferred\n";
        }
        else
        {
          std::cout << "Error: " << error.message() << "\n";
        }
      });

  io_context.run();
}

//------------------------------------------------------------------------------

void test_deferred()
{
  asio::io_context io_context;

  tcp::acceptor acceptor(io_context, {tcp::v4(), 55555});
  tcp::socket socket = acceptor.accept();

  // Test our asynchronous operation using the deferred completion token. This
  // token causes the operation's initiating function to package up the
  // operation with its arguments to return a function object, which may then be
  // used to launch the asynchronous operation.
  auto op = async_write_message(socket,
      "Testing deferred\r\n", asio::deferred);

  // Launch the operation using a lambda as a callback.
  std::move(op)(
      [](const std::error_code& error, std::size_t n)
      {
        if (!error)
        {
          std::cout << n << " bytes transferred\n";
        }
        else
        {
          std::cout << "Error: " << error.message() << "\n";
        }
      });

  io_context.run();
}

//------------------------------------------------------------------------------

void test_future()
{
  asio::io_context io_context;

  tcp::acceptor acceptor(io_context, {tcp::v4(), 55555});
  tcp::socket socket = acceptor.accept();

  // Test our asynchronous operation using the use_future completion token.
  // This token causes the operation's initiating function to return a future,
  // which may be used to synchronously wait for the result of the operation.
  std::future<std::size_t> f = async_write_message(
      socket, "Testing future\r\n", asio::use_future);

  io_context.run();

  try
  {
    // Get the result of the operation.
    std::size_t n = f.get();
    std::cout << n << " bytes transferred\n";
  }
  catch (const std::exception& e)
  {
    std::cout << "Error: " << e.what() << "\n";
  }
}

//------------------------------------------------------------------------------

int main()
{
  test_callback();
  test_deferred();
  test_future();
}
