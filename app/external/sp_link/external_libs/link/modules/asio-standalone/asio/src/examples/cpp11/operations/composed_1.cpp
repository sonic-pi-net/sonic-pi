//
// composed_1.cpp
// ~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2020 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

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
  // of CompletionToken type and the completion handler's signature. When the
  // completion token is a simple callback, the return type is void. However,
  // when the completion token is asio::yield_context (used for stackful
  // coroutines) the return type would be std::size_t, and when the completion
  // token is asio::use_future it would be std::future<std::size_t>.
  -> typename asio::async_result<
    typename std::decay<CompletionToken>::type,
    void(std::error_code, std::size_t)>::return_type
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
  test_future();
}
