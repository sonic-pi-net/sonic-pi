//  binary arithmetic simd functions
//  Copyright (C) 2010 Tim Blechmann
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
//  Boston, MA 02110-1301, USA.

#ifndef SIMD_BINARY_ARITHMETIC_HPP
#define SIMD_BINARY_ARITHMETIC_HPP

#include <functional>
#include <algorithm>

#include "vec.hpp"

#include "detail/unroll_helpers.hpp"
#include "detail/define_macros.hpp"


#if defined(__GNUC__) && defined(NDEBUG)
#define always_inline inline  __attribute__((always_inline))
#else
#define always_inline inline
#endif


namespace nova {
namespace detail {

struct plus
{
    template<typename FloatType>
    FloatType operator()(FloatType const & arg1, FloatType const & arg2) const
    {
        return arg1 + arg2;
    }
};

struct minus
{
    template<typename FloatType>
    FloatType operator()(FloatType const & arg1, FloatType const & arg2) const
    {
        return arg1 - arg2;
    }
};

struct multiplies
{
    template<typename FloatType>
    FloatType operator()(FloatType const & arg1, FloatType const & arg2) const
    {
        return arg1 * arg2;
    }
};

struct divides
{
    template<typename FloatType>
    FloatType operator()(FloatType const & arg1, FloatType const & arg2) const
    {
        return arg1 / arg2;
    }
};

struct clip2
{
    template<typename FloatType>
    FloatType operator()(FloatType const & f, FloatType const & limit) const
    {
        FloatType zero = 0.0;
        FloatType neg = zero - FloatType(limit);
        return max_(neg, min_(f, limit));
    }
};

struct min_functor
{
    template<typename FloatType>
    FloatType operator()(FloatType const & x, FloatType const & y) const
    {
        return min_(x, y);
    }
};

struct max_functor
{
    template<typename FloatType>
    FloatType operator()(FloatType const & x, FloatType const & y) const
    {
        return max_(x, y);
    }
};

struct less
{
    template<typename FloatType>
    FloatType operator()(FloatType const & x, FloatType const & y) const
    {
        return x < y;
    }
};

struct less_equal
{
    template<typename FloatType>
    FloatType operator()(FloatType const & x, FloatType const & y) const
    {
        return x <= y;
    }
};

struct greater
{
    template<typename FloatType>
    FloatType operator()(FloatType const & x, FloatType const & y) const
    {
        return x > y;
    }
};

struct greater_equal
{
    template<typename FloatType>
    FloatType operator()(FloatType const & x, FloatType const & y) const
    {
        return x >= y;
    }
};


struct equal_to
{
    template<typename FloatType>
    FloatType operator()(FloatType const & x, FloatType const & y) const
    {
        return x == y;
    }
};

struct not_equal_to
{
    template<typename FloatType>
    FloatType operator()(FloatType const & x, FloatType const & y) const
    {
        return x != y;
    }
};


} /* namespace detail */


NOVA_SIMD_DEFINE_BINARY_WRAPPER(plus, detail::plus)

NOVA_SIMD_DEFINE_BINARY_WRAPPER(minus, detail::minus)
NOVA_SIMD_DEFINE_BINARY_WRAPPER(times, detail::multiplies)
NOVA_SIMD_DEFINE_BINARY_WRAPPER(over, detail::divides)

NOVA_SIMD_DEFINE_BINARY_WRAPPER(min, detail::min_functor)
NOVA_SIMD_DEFINE_BINARY_WRAPPER(max, detail::max_functor)
NOVA_SIMD_DEFINE_BINARY_WRAPPER(less, detail::less)
NOVA_SIMD_DEFINE_BINARY_WRAPPER(less_equal, detail::less_equal)
NOVA_SIMD_DEFINE_BINARY_WRAPPER(greater, detail::greater)
NOVA_SIMD_DEFINE_BINARY_WRAPPER(greater_equal, detail::greater_equal)
NOVA_SIMD_DEFINE_BINARY_WRAPPER(equal, detail::equal_to)
NOVA_SIMD_DEFINE_BINARY_WRAPPER(notequal, detail::not_equal_to)

NOVA_SIMD_DEFINE_BINARY_WRAPPER(clip2, detail::clip2)

} /* namespace nova */

#undef always_inline


#endif /* SIMD_BINARY_ARITHMETIC_HPP */
