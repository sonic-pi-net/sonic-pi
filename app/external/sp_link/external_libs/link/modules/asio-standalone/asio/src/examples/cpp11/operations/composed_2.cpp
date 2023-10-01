//
// composed_2.cpp
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

// NOTE: This example requires the new asio::async_initiate function. For
// an example that works with the Networking TS style of completion tokens,
// please see an older version of asio.

//------------------------------------------------------------------------------

// This next simplest example of a composed asynchronous operation involves
// repackaging multiple operations but choosing to invoke just one of them. All
// of these underlying operations have the same completion signature. The
// asynchronous operation requirements are met by delegating responsibility to
// the underlying operations.

// In addition to determining the mechanism by which an asynchronous operation
// delivers its result, a completion token also determines the time when the
// operation commences. For example, when the completion token is a simple
// callback the operation commences before the initiating function returns.
// However, if the completion token's delivery mechanism uses a future, we
// might instead want to defer initiation of the operation until the returned
// future object is waited upon.
//
// To enable this, when implementing an asynchronous operation we must package
// the initiation step as a function object.
struct async_write_message_initiation
{
  // The initiation function object's call operator is passed the concrete
  // completion handler produced by the completion token. This completion
  // handler matches the asynchronous operation's completion handler signature,
  // which in this example is:
  //
  //   void(std::error_code error, std::size_t)
  //
  // The initiation function object also receives any additional arguments
  // required to start the operation. (Note: We could have instead passed these
  // arguments as members in the initiaton function object. However, we should
  // prefer to propagate them as function call arguments as this allows the
  // completion token to optimise how they are passed. For example, a lazy
  // future which defers initiation would need to make a decay-copy of the
  // arguments, but when using a simple callback the arguments can be trivially
  // forwarded straight through.)
  template <typename CompletionHandler>
  void operator()(CompletionHandler&& completion_handler, tcp::socket& socket,
      const char* message, bool allow_partial_write) const
  {
    if (allow_partial_write)
    {
      // When delegating to an underlying operation we must take care to
      // perfectly forward the completion handler. This ensures that our
      // operation works correctly with move-only function objects as
      // callbacks.
      return socket.async_write_some(
          asio::buffer(message, std::strlen(message)),
          std::forward<CompletionHandler>(completion_handler));
    }
    else
    {
      // As above, we must perfectly forward the completion handler when calling
      // the alternate underlying operation.
      return asio::async_write(socket,
          asio::buffer(message, std::strlen(message)),
          std::forward<CompletionHandler>(completion_handler));
    }
  }
};

template <typename CompletionToken>
auto async_write_message(tcp::socket& socket,
    const char* message, bool allow_partial_write,
    CompletionToken&& token)
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
  // In C++11 we deduce the type from the call to asio::async_initiate.
  -> decltype(
      asio::async_initiate<
        CompletionToken, void(std::error_code, std::size_t)>(
          async_write_message_initiation(),
          token, std::ref(socket), message, allow_partial_write))
{
  // The asio::async_initiate function takes:
  //
  // - our initiation function object,
  // - the completion token,
  // - the completion handler signature, and
  // - any additional arguments we need to initiate the operation.
  //
  // It then asks the completion token to create a completion handler (i.e. a
  // callback) with the specified signature, and invoke the initiation function
  // object with this completion handler as well as the additional arguments.
  // The return value of async_initiate is the result of our operation's
  // initiating function.
  //
  // Note that we wrap non-const reference arguments in std::reference_wrapper
  // to prevent incorrect decay-copies of these objects.
  return asio::async_initiate<
    CompletionToken, void(std::error_code, std::size_t)>(
      async_write_message_initiation(),
      token, std::ref(socket), message, allow_partial_write);
}

//------------------------------------------------------------------------------

void test_callback()
{
  asio::io_context io_context;

  tcp::acceptor acceptor(io_context, {tcp::v4(), 55555});
  tcp::socket socket = acceptor.accept();

  // Test our asynchronous operation using a lambda as a callback.
  async_write_message(socket, "Testing callback\r\n", false,
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
  // operation and its arguments to return a function object, which may then be
  // used to launch the asynchronous operation.
  auto op = async_write_message(socket,
      "Testing deferred\r\n", false, asio::deferred);

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
      socket, "Testing future\r\n", false, asio::use_future);

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
