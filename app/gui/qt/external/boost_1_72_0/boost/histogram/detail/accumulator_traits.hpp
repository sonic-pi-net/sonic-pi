// Copyright 2019 Hans Dembinski
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt
// or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_HISTOGRAM_DETAIL_ACCUMULATOR_TRAITS_HPP
#define BOOST_HISTOGRAM_DETAIL_ACCUMULATOR_TRAITS_HPP

#include <boost/histogram/fwd.hpp>
#include <tuple>
#include <type_traits>

namespace boost {

namespace accumulators {
template <class, class, class>
struct accumulator_set;
}

namespace histogram {
namespace detail {

template <bool WeightSupport, class... Ts>
struct accumulator_traits_holder {
  using wsupport = std::integral_constant<bool, WeightSupport>;
  using args = std::tuple<Ts...>;
};

template <class R, class T, class U, class... Ts>
accumulator_traits_holder<true, Ts...> accumulator_traits_impl_2(
    R (T::*)(boost::histogram::weight_type<U>, Ts...));

template <class R, class T, class U, class... Ts>
accumulator_traits_holder<true, Ts...> accumulator_traits_impl_2(
    R (T::*)(boost::histogram::weight_type<U>&&, Ts...));

template <class R, class T, class U, class... Ts>
accumulator_traits_holder<true, Ts...> accumulator_traits_impl_2(
    R (T::*)(const boost::histogram::weight_type<U>&, Ts...));

template <class R, class T, class... Ts>
accumulator_traits_holder<false, Ts...> accumulator_traits_impl_2(R (T::*)(Ts...));

template <class T>
auto accumulator_traits_impl(T&)
    -> decltype(std::declval<T&>() += 0, accumulator_traits_holder<true>{});

template <class T>
auto accumulator_traits_impl(T&) -> decltype(accumulator_traits_impl_2(&T::operator()));

// for boost.accumulators compatibility
template <class S, class F, class W>
accumulator_traits_holder<false, S> accumulator_traits_impl(
    boost::accumulators::accumulator_set<S, F, W>&);

template <class T>
using accumulator_traits = decltype(accumulator_traits_impl(std::declval<T&>()));

} // namespace detail
} // namespace histogram
} // namespace boost

#endif
