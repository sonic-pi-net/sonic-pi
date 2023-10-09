//
// ranged_wait_for_all.cpp
// ~~~~~~~~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include <asio.hpp>
#include <asio/experimental/parallel_group.hpp>
#include <iostream>
#include <vector>

#ifdef ASIO_HAS_POSIX_STREAM_DESCRIPTOR

int main()
{
  asio::io_context ctx;

  asio::posix::stream_descriptor out(ctx, ::dup(STDOUT_FILENO));
  asio::posix::stream_descriptor err(ctx, ::dup(STDERR_FILENO));

  using op_type = decltype(
      out.async_write_some(
        asio::buffer("", 0),
        asio::deferred
      )
    );

  std::vector<op_type> ops;

  ops.push_back(
      out.async_write_some(
        asio::buffer("first\r\n", 7),
        asio::deferred
      )
    );

  ops.push_back(
      err.async_write_some(
        asio::buffer("second\r\n", 8),
        asio::deferred
      )
    );

  asio::experimental::make_parallel_group(ops).async_wait(
      asio::experimental::wait_for_all(),
      [](
          std::vector<std::size_t> completion_order,
          std::vector<std::error_code> ec,
          std::vector<std::size_t> n
      )
      {
        for (std::size_t i = 0; i < completion_order.size(); ++i)
        {
          std::size_t idx = completion_order[i];
          std::cout << "operation " << idx << " finished: ";
          std::cout << ec[idx] << ", " << n[idx] << "\n";
        }
      }
    );

  ctx.run();
}

#else // defined(ASIO_HAS_POSIX_STREAM_DESCRIPTOR)
int main() {}
#endif // defined(ASIO_HAS_POSIX_STREAM_DESCRIPTOR)
