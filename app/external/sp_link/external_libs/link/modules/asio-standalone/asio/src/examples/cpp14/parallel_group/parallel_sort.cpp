//
// parallel_sort.cpp
// ~~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include <asio.hpp>
#include <asio/experimental/parallel_group.hpp>
#include <algorithm>
#include <chrono>
#include <functional>
#include <iostream>
#include <random>

template <
    typename Executor,
    typename RandomAccessIterator,
    ASIO_COMPLETION_TOKEN_FOR(void()) CompletionToken>
auto parallel_sort(
    Executor executor,
    RandomAccessIterator begin,
    RandomAccessIterator end,
    CompletionToken&& token);

template <
    typename Executor,
    typename RandomAccessIterator>
void parallel_sort_impl(
    Executor executor,
    RandomAccessIterator begin,
    RandomAccessIterator end,
    std::function<void()> continuation)
{
    std::size_t n = end - begin;
    if (n <= 16384)
    {
      asio::post(executor,
          [=]
          {
            std::sort(begin, end);
            continuation();
          }
        );
    }
    else
    {
      asio::experimental::make_parallel_group(
          [=](auto token)
          {
            return parallel_sort(executor, begin, begin + n / 2, token);
          },
          [=](auto token)
          {
            return parallel_sort(executor, begin + n / 2, end, token);
          }
        ).async_wait(
          asio::experimental::wait_for_all(),
          [=](std::array<std::size_t, 2>)
          {
            std::inplace_merge(begin, begin + n / 2, end);
            continuation();
          }
        );
    }
}

template <
    typename Executor,
    typename RandomAccessIterator,
    ASIO_COMPLETION_TOKEN_FOR(void()) CompletionToken>
auto parallel_sort(
    Executor executor,
    RandomAccessIterator begin,
    RandomAccessIterator end,
    CompletionToken&& token)
{
  return asio::async_compose<CompletionToken, void()>(
      [=](auto& self, auto... args)
      {
        if (sizeof...(args) == 0)
        {
          using self_type = std::decay_t<decltype(self)>;
          parallel_sort_impl(executor, begin, end,
              [self = std::make_shared<self_type>(std::move(self))]
              {
                asio::dispatch(
                    asio::append(
                      std::move(*self), 0));
              }
            );
        }
        else
        {
          self.complete();
        }
      },
      token
    );
}

int main()
{
  asio::thread_pool pool(4);

  std::vector<int> values(100'000'000);

  std::random_device random_device;
  std::mt19937 rng(random_device());
  std::uniform_int_distribution<int> dist(1, 1'000'000);
  std::generate(values.begin(), values.end(), [&]{ return dist(rng); });

  std::cout << "starting sort\n";

  auto begin = std::chrono::high_resolution_clock::now();

  parallel_sort(
      pool.get_executor(),
      values.begin(),
      values.end(),
      asio::use_future
    ).get();
  
  auto end = std::chrono::high_resolution_clock::now();

  auto duration = end - begin;
  std::cout << "sort took "
    << std::chrono::duration_cast<std::chrono::microseconds>(duration).count()
    << " microseconds\n";
}
