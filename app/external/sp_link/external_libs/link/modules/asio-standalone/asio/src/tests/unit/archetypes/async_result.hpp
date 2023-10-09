//
// async_result.hpp
// ~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef ARCHETYPES_ASYNC_RESULT_HPP
#define ARCHETYPES_ASYNC_RESULT_HPP

#include <asio/async_result.hpp>
#include <asio/system_executor.hpp>

namespace archetypes {

struct immediate_handler
{
};

struct lazy_handler
{
};

template <typename Signature>
struct concrete_handler;

template <typename R, typename Arg1>
struct concrete_handler<R(Arg1)>
{
  concrete_handler()
  {
  }

  void operator()(typename asio::decay<Arg1>::type)
  {
  }

#if defined(ASIO_HAS_MOVE)
  concrete_handler(concrete_handler&&) {}
private:
  concrete_handler(const concrete_handler&);
#endif // defined(ASIO_HAS_MOVE)
};

template <typename R, typename Arg1, typename Arg2>
struct concrete_handler<R(Arg1, Arg2)>
{
  concrete_handler()
  {
  }

  void operator()(typename asio::decay<Arg1>::type,
      typename asio::decay<Arg2>::type)
  {
  }
};

template <typename Signature>
struct immediate_concrete_handler : concrete_handler<Signature>
{
  typedef asio::system_executor immediate_executor_type;

  immediate_concrete_handler(immediate_handler)
  {
  }

  immediate_executor_type get_immediate_executor() const ASIO_NOEXCEPT
  {
    return immediate_executor_type();
  }

#if defined(ASIO_HAS_MOVE)
  immediate_concrete_handler(immediate_concrete_handler&&) {}
private:
  immediate_concrete_handler(const immediate_concrete_handler&);
#endif // defined(ASIO_HAS_MOVE)
};

template <typename Signature>
struct lazy_concrete_handler : concrete_handler<Signature>
{
  lazy_concrete_handler(lazy_handler)
  {
  }

#if defined(ASIO_HAS_MOVE)
  lazy_concrete_handler(lazy_concrete_handler&&) {}
private:
  lazy_concrete_handler(const lazy_concrete_handler&);
#endif // defined(ASIO_HAS_MOVE)
};

} // namespace archetypes

namespace asio {

template <typename Signature>
class async_result<archetypes::immediate_handler, Signature>
{
public:
  // The concrete completion handler type.
  typedef archetypes::immediate_concrete_handler<Signature>
    completion_handler_type;

  // The return type of the initiating function.
  typedef void return_type;

  // Construct an async_result from a given handler.
  explicit async_result(completion_handler_type&)
  {
  }

  // Obtain the value to be returned from the initiating function.
  void get()
  {
  }

private:
  // Disallow copying and assignment.
  async_result(const async_result&) ASIO_DELETED;
  async_result& operator=(const async_result&) ASIO_DELETED;
};

template <typename Signature>
class async_result<archetypes::lazy_handler, Signature>
{
public:
  // The concrete completion handler type.
  typedef archetypes::lazy_concrete_handler<Signature> completion_handler_type;

  // The return type of the initiating function.
  typedef int return_type;

  // Construct an async_result from a given handler.
  explicit async_result(completion_handler_type&)
  {
  }

  // Obtain the value to be returned from the initiating function.
  return_type get()
  {
    return 42;
  }

private:
  // Disallow copying and assignment.
  async_result(const async_result&) ASIO_DELETED;
  async_result& operator=(const async_result&) ASIO_DELETED;
};

} // namespace asio

#endif // ARCHETYPES_ASYNC_RESULT_HPP
