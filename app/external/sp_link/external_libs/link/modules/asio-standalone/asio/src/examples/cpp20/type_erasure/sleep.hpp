//
// sleep.hpp
// ~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef SLEEP_HPP
#define SLEEP_HPP

#include <asio/any_completion_handler.hpp>
#include <asio/any_io_executor.hpp>
#include <asio/async_result.hpp>
#include <asio/error.hpp>
#include <chrono>

void async_sleep_impl(
    asio::any_completion_handler<void(std::error_code)> handler,
    asio::any_io_executor ex, std::chrono::nanoseconds duration);

template <typename CompletionToken>
inline auto async_sleep(asio::any_io_executor ex,
    std::chrono::nanoseconds duration, CompletionToken&& token)
{
  return asio::async_initiate<CompletionToken, void(std::error_code)>(
      async_sleep_impl, token, std::move(ex), duration);
}

#endif // SLEEP_HPP
