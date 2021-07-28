#include <asio/associated_executor.hpp>
#include <asio/bind_executor.hpp>
#include <asio/execution.hpp>
#include <asio/static_thread_pool.hpp>
#include <iostream>
#include <string>

using asio::bind_executor;
using asio::get_associated_executor;
using asio::static_thread_pool;
namespace execution = asio::execution;

// A function to asynchronously read a single line from an input stream.
template <class IoExecutor, class Handler>
void async_getline(IoExecutor io_ex, std::istream& is, Handler handler)
{
  // Track work for the handler's associated executor.
  auto work_ex = asio::prefer(
      get_associated_executor(handler, io_ex),
      execution::outstanding_work.tracked);

  // Post a function object to do the work asynchronously.
  execution::execute(
      asio::require(io_ex, execution::blocking.never),
      [&is, work_ex, handler=std::move(handler)]() mutable
      {
        std::string line;
        std::getline(is, line);

        // Pass the result to the handler, via the associated executor.
        execution::execute(
            asio::prefer(work_ex, execution::blocking.possibly),
            [line=std::move(line), handler=std::move(handler)]() mutable
            {
              handler(std::move(line));
            });
      });
}

// A function to asynchronously read multiple lines from an input stream.
template <class IoExecutor, class Handler>
void async_getlines(IoExecutor io_ex, std::istream& is, std::string init, Handler handler)
{
  // Track work for the I/O executor.
  auto io_work_ex = asio::prefer(io_ex,
      execution::outstanding_work.tracked);

  // Track work for the handler's associated executor.
  auto handler_work_ex = asio::prefer(
      get_associated_executor(handler, io_ex),
      execution::outstanding_work.tracked);

  // Use the associated executor for each operation in the composition.
  async_getline(io_work_ex, is,
      bind_executor(handler_work_ex,
        [io_work_ex, &is, lines=std::move(init), handler=std::move(handler)]
        (std::string line) mutable
        {
          if (line.empty())
            handler(lines);
          else
            async_getlines(io_work_ex, is, lines + line + "\n", std::move(handler));
        }));
}

int main()
{
  static_thread_pool io_pool(1);
  static_thread_pool completion_pool(1);

  std::cout << "Enter text, terminating with a blank line:\n";

  async_getlines(io_pool.executor(), std::cin, "",
      bind_executor(completion_pool.executor(), [](std::string lines)
        {
          std::cout << "Lines:\n" << lines << "\n";
        }));

  io_pool.wait();
  completion_pool.wait();
}
