//
// c_callback_wrapper.cpp
// ~~~~~~~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include <asio.hpp>
#include <iostream>
#include <memory>
#include <new>

//------------------------------------------------------------------------------

// This is a mock implementation of a C-based API that uses the function pointer
// plus void* context idiom for exposing a callback.

void read_input(const char* prompt, void (*cb)(void*, const char*), void* arg)
{
  std::thread(
      [prompt = std::string(prompt), cb, arg]
      {
        std::cout << prompt << ": ";
        std::cout.flush();
        std::string line;
        std::getline(std::cin, line);
        cb(arg, line.c_str());
      }).detach();
}

//------------------------------------------------------------------------------

// This is an asynchronous operation that wraps the C-based API.

// To map our completion handler into a function pointer / void* callback, we
// need to allocate some state that will live for the duration of the
// operation. A pointer to this state will be passed to the C-based API.
template <asio::completion_handler_for<void(std::string)> Handler>
class read_input_state
{
public:
  read_input_state(Handler&& handler)
    : handler_(std::move(handler)),
      work_(asio::make_work_guard(handler_))
  {
  }

  // Create the state using the handler's associated allocator.
  static read_input_state* create(Handler&& handler)
  {
    // A unique_ptr deleter that is used to destroy uninitialised objects.
    struct deleter
    {
      // Get the handler's associated allocator type. If the handler does not
      // specify an associated allocator, we will use a recycling allocator as
      // the default. As the associated allocator is a proto-allocator, we must
      // rebind it to the correct type before we can use it to allocate objects.
      typename std::allocator_traits<
        asio::associated_allocator_t<Handler,
          asio::recycling_allocator<void>>>::template
            rebind_alloc<read_input_state> alloc;

      void operator()(read_input_state* ptr)
      {
        std::allocator_traits<decltype(alloc)>::deallocate(alloc, ptr, 1);
      }
    } d{asio::get_associated_allocator(handler,
          asio::recycling_allocator<void>())};

    // Allocate memory for the state.
    std::unique_ptr<read_input_state, deleter> uninit_ptr(
        std::allocator_traits<decltype(d.alloc)>::allocate(d.alloc, 1), d);

    // Construct the state into the newly allocated memory. This might throw.
    read_input_state* ptr =
      new (uninit_ptr.get()) read_input_state(std::move(handler));

    // Release ownership of the memory and return the newly allocated state.
    uninit_ptr.release();
    return ptr;
  }

  static void callback(void* arg, const char* result)
  {
    read_input_state* self = static_cast<read_input_state*>(arg);

    // A unique_ptr deleter that is used to destroy initialised objects.
    struct deleter
    {
      // Get the handler's associated allocator type. If the handler does not
      // specify an associated allocator, we will use a recycling allocator as
      // the default. As the associated allocator is a proto-allocator, we must
      // rebind it to the correct type before we can use it to allocate objects.
      typename std::allocator_traits<
        asio::associated_allocator_t<Handler,
          asio::recycling_allocator<void>>>::template
            rebind_alloc<read_input_state> alloc;

      void operator()(read_input_state* ptr)
      {
        std::allocator_traits<decltype(alloc)>::destroy(alloc, ptr);
        std::allocator_traits<decltype(alloc)>::deallocate(alloc, ptr, 1);
      }
    } d{asio::get_associated_allocator(self->handler_,
          asio::recycling_allocator<void>())};

    // To conform to the rules regarding asynchronous operations and memory
    // allocation, we must make a copy of the state and deallocate the memory
    // before dispatching the completion handler.
    std::unique_ptr<read_input_state, deleter> state_ptr(self, d);
    read_input_state state(std::move(*self));
    state_ptr.reset();

    // Dispatch the completion handler through the handler's associated
    // executor, using the handler's associated allocator.
    asio::dispatch(state.work_.get_executor(),
        asio::bind_allocator(d.alloc,
          [
            handler = std::move(state.handler_),
            result = std::string(result)
          ]() mutable
          {
            std::move(handler)(result);
          }));
  }

private:
  Handler handler_;

  // According to the rules for asynchronous operations, we need to track
  // outstanding work against the handler's associated executor until the
  // asynchronous operation is complete.
  asio::executor_work_guard<
    asio::associated_executor_t<Handler>> work_;
};

// The initiating function for the asynchronous operation.
template <asio::completion_token_for<void(std::string)> CompletionToken>
auto async_read_input(const std::string& prompt, CompletionToken&& token)
{
  // Define a function object that contains the code to launch the asynchronous
  // operation. This is passed the concrete completion handler, followed by any
  // additional arguments that were passed through the call to async_initiate.
  auto init = [](
      asio::completion_handler_for<void(std::string)> auto handler,
      const std::string& prompt)
  {
    // The body of the initiation function object creates the long-lived state
    // and passes it to the C-based API, along with the function pointer.
    using state_type = read_input_state<decltype(handler)>;
    read_input(prompt.c_str(), &state_type::callback,
        state_type::create(std::move(handler)));
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
  // an io_context to obtain an associated executor.
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
