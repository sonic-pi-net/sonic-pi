//
// deferred_1.cpp
// ~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include <asio.hpp>
#include <iostream>

using asio::deferred;

int main()
{
  asio::io_context ctx;

  asio::steady_timer timer(ctx);
  timer.expires_after(std::chrono::seconds(1));

  auto deferred_op = timer.async_wait(deferred);

  std::move(deferred_op)(
      [](std::error_code ec)
      {
        std::cout << "timer wait finished: " << ec.message() << "\n";
      }
    );

  ctx.run();

  return 0;
}
