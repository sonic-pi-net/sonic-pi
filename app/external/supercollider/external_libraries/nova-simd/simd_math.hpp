//  simdfied mathematical functions
//  Copyright (C) 2009 Tim Blechmann
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

#ifndef SIMD_MATH_HPP
#define SIMD_MATH_HPP

#include "vec.hpp"

#include "detail/math.hpp"
#include "detail/unroll_helpers.hpp"
#include "detail/define_macros.hpp"

#if defined(__GNUC__) && defined(NDEBUG)
#define always_inline inline  __attribute__((always_inline))
#else
#define always_inline inline
#endif


namespace nova {

#define DEFINE_UNARY_FUNCTOR(NAME)                                      \
namespace detail {                                                      \
struct NAME##_                                                          \
{                                                                       \
    template <typename FloatType>                                       \
    always_inline FloatType operator()(FloatType arg) const             \
    {                                                                   \
        return NAME(arg);                                               \
    }                                                                   \
};                                                                      \
} // namespace detail


#define DEFINE_UNARY_MATH_FUNCTIONS(NAME)               \
DEFINE_UNARY_FUNCTOR(NAME)                              \
NOVA_SIMD_DEFINE_UNARY_WRAPPER(NAME, detail::NAME##_)


DEFINE_UNARY_MATH_FUNCTIONS(sin)
DEFINE_UNARY_MATH_FUNCTIONS(cos)
DEFINE_UNARY_MATH_FUNCTIONS(tan)
DEFINE_UNARY_MATH_FUNCTIONS(asin)
DEFINE_UNARY_MATH_FUNCTIONS(acos)
DEFINE_UNARY_MATH_FUNCTIONS(atan)

DEFINE_UNARY_MATH_FUNCTIONS(tanh)

DEFINE_UNARY_MATH_FUNCTIONS(log)
DEFINE_UNARY_MATH_FUNCTIONS(log2)
DEFINE_UNARY_MATH_FUNCTIONS(log10)
DEFINE_UNARY_MATH_FUNCTIONS(exp)

DEFINE_UNARY_MATH_FUNCTIONS(signed_sqrt)



#define DEFINE_BINARY_MATH_FUNCTOR(NAME)                                \
namespace detail {                                                      \
struct NAME##_                                                          \
{                                                                       \
    template <typename FloatType>                                       \
    always_inline FloatType operator()(FloatType lhs, FloatType rhs) const \
    {                                                                   \
        return NAME(lhs, rhs);                               \
    }                                                                   \
};                                                                      \
                                                                        \
}


DEFINE_BINARY_MATH_FUNCTOR(pow)
DEFINE_BINARY_MATH_FUNCTOR(signed_pow)

NOVA_SIMD_DEFINE_BINARY_WRAPPER(pow, detail::pow_)
NOVA_SIMD_DEFINE_BINARY_WRAPPER(spow, detail::signed_pow_)

}

#undef DEFINE_UNARY_FUNCTOR
#undef DEFINE_BINARY_MATH_FUNCTOR
#undef DEFINE_UNARY_MATH_FUNCTIONS
#undef DEFINE_BINARY_MATH_FUNCTIONS
#undef always_inline

#endif /* SIMD_MATH_HPP */
