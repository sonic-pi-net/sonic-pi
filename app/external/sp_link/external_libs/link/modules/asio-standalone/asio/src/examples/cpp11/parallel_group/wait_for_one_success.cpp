//
// wait_for_one_error.cpp
// ~~~~~~~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include <asio.hpp>
#include <asio/experimental/deferred.hpp>
#include <asio/experimental/parallel_group.hpp>
#include <iostream>

#if defined(ASIO_HAS_POSIX_STREAM_DESCRIPTOR)

int main()
{
  asio::io_context ctx;

  asio::posix::stream_descriptor in(ctx, ::dup(STDIN_FILENO));
  asio::steady_timer timer(ctx, std::chrono::seconds(5));

  char data[1024];

  asio::experimental::make_parallel_group(
      in.async_read_some(
        asio::buffer(data),
        asio::deferred),
      timer.async_wait(
        asio::deferred)
    ).async_wait(
      asio::experimental::wait_for_one_success(),
      [](
          std::array<std::size_t, 2> completion_order,
          std::error_code ec1, std::size_t n1,
          std::error_code ec2
      )
      {
        switch (completion_order[0])
        {
        case 0:
          {
            std::cout << "descriptor finished: " << ec1 << ", " << n1 << "\n";
          }
          break;
        case 1:
          {
            std::cout << "timer finished: " << ec2 << "\n";
          }
          break;
        }
      }
    );

  ctx.run();
}

#else // defined(ASIO_HAS_POSIX_STREAM_DESCRIPTOR)
int main() {}
#endif // defined(ASIO_HAS_POSIX_STREAM_DESCRIPTOR)
