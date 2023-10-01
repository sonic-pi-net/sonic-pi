//
// callback_wrapper.cpp
// ~~~~~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include <asio.hpp>
#include <iostream>

//------------------------------------------------------------------------------

// This is a mock implementation of an API that uses a move-only function object
// for exposing a callback. The callback has the signature void(std::string).

template <typename Callback>
void read_input(const std::string& prompt, Callback cb)
{
  std::thread(
      [prompt, cb = std::move(cb)]() mutable
      {
        std::cout << prompt << ": ";
        std::cout.flush();
        std::string line;
        std::getline(std::cin, line);
        std::move(cb)(std::move(line));
      }).detach();
}

//------------------------------------------------------------------------------

// This is an asynchronous operation that wraps the callback-based API.

// The initiating function for the asynchronous operation.
template <typename CompletionToken>
auto async_read_input(const std::string& prompt, CompletionToken&& token)
{
  // Define a function object that contains the code to launch the asynchronous
  // operation. This is passed the concrete completion handler, followed by any
  // additional arguments that were passed through the call to async_initiate.
  auto init = [](auto handler, const std::string& prompt)
  {
    // According to the rules for asynchronous operations, we need to track
    // outstanding work against the handler's associated executor until the
    // asynchronous operation is complete.
    auto work = asio::make_work_guard(handler);

    // Launch the operation with a callback that will receive the result and
    // pass it through to the asynchronous operation's completion handler.
    read_input(prompt,
        [
          handler = std::move(handler),
          work = std::move(work)
        ](std::string result) mutable
        {
          // Get the handler's associated allocator. If the handler does not
          // specify an allocator, use the recycling allocator as the default.
          auto alloc = asio::get_associated_allocator(
              handler, asio::recycling_allocator<void>());

          // Dispatch the completion handler through the handler's associated
          // executor, using the handler's associated allocator.
          asio::dispatch(work.get_executor(),
              asio::bind_allocator(alloc,
                [
                  handler = std::move(handler),
                  result = std::string(result)
                ]() mutable
                {
                  std::move(handler)(result);
                }));
        });
  };

  // The async_initiate function is used to transform the supplied completion
  // token to the completion handler. When calling this function we explicitly
  // specify the completion signature of the operation. We must also return the
  // result of the call since the completion token may produce a return value,
  // such as a future.
  return asio::async_initiate<CompletionToken, void(std::string)>(
      init, // First, pass the function object that launches the operation,
      token, // then the completion token that will be transformed to a handler,
      prompt); // and, finally, any additional arguments to the function object.
}

//------------------------------------------------------------------------------

void test_callback()
{
  asio::io_context io_context;

  // Test our asynchronous operation using a lambda as a callback. We will use
  // an io_context to specify an associated executor.
  async_read_input("Enter your name",
      asio::bind_executor(io_context,
        [](const std::string& result)
        {
          std::cout << "Hello " << result << "\n";
        }));

  io_context.run();
}

//------------------------------------------------------------------------------

void test_deferred()
{
  asio::io_context io_context;

  // Test our asynchronous operation using the deferred completion token. This
  // token causes the operation's initiating function to package up the
  // operation with its arguments to return a function object, which may then be
  // used to launch the asynchronous operation.
  auto op = async_read_input("Enter your name", asio::deferred);

  // Launch our asynchronous operation using a lambda as a callback. We will use
  // an io_context to obtain an associated executor.
  std::move(op)(
      asio::bind_executor(io_context,
        [](const std::string& result)
        {
          std::cout << "Hello " << result << "\n";
        }));

  io_context.run();
}

//------------------------------------------------------------------------------

void test_future()
{
  // Test our asynchronous operation using the use_future completion token.
  // This token causes the operation's initiating function to return a future,
  // which may be used to synchronously wait for the result of the operation.
  std::future<std::string> f =
    async_read_input("Enter your name", asio::use_future);

  std::string result = f.get();
  std::cout << "Hello " << result << "\n";
}

//------------------------------------------------------------------------------

int main()
{
  test_callback();
  test_deferred();
  test_future();
}
