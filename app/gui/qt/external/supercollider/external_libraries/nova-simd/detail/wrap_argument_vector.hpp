//  vector signal argument wrappers
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

#ifndef NOVA_SIMD_DETAIL_WRAP_ARGUMENT_VECTOR_HPP
#define NOVA_SIMD_DETAIL_WRAP_ARGUMENT_VECTOR_HPP

#include "../vec.hpp"

#include "wrap_arguments.hpp"

#if defined(__GNUC__) && defined(NDEBUG)
#define always_inline inline  __attribute__((always_inline))
#else
#define always_inline inline
#endif


namespace nova {
namespace detail {

template <typename FloatType>
struct vector_pointer_argument
{
    always_inline explicit vector_pointer_argument(const FloatType * arg):
        data(arg)
    {}

    always_inline void increment(void)
    {
        data += vec<FloatType>::size;
    }

    always_inline vec<FloatType> get(void) const
    {
        vec<FloatType> ret;
        ret.load_aligned(data);
        return ret;
    }

    always_inline vec<FloatType> consume(void)
    {
        vec<FloatType> ret;
        ret.load_aligned(data);
        increment();
        return ret;
    }

    const FloatType * data;
};

template <typename FloatType>
struct vector_scalar_argument
{
    always_inline explicit vector_scalar_argument(FloatType const & arg):
        data(arg)
    {}

    always_inline void increment(void)
    {}

    always_inline vec<FloatType> get(void) const
    {
        return vec<FloatType>(data);
    }

    always_inline vec<FloatType> consume(void)
    {
        return vec<FloatType>(data);
    }

    FloatType data;
};

template <typename FloatType>
struct vector_ramp_argument
{
    always_inline vector_ramp_argument(FloatType const & base, FloatType const & slope)
    {
        float vSlope = data.set_slope(base, slope);
        slope_.set_vec(vSlope);
    }

    always_inline void increment(void)
    {
        data += slope_;
    }

    always_inline vec<FloatType> get(void) const
    {
        return data;
    }

    always_inline vec<FloatType> consume(void)
    {
        vec<FloatType> ret(data);
        increment();
        return ret;
    }

    vec<FloatType> data;
    vec<FloatType> slope_;
};

/* convert scalar args to vector args */
template <typename FloatType>
always_inline detail::vector_scalar_argument<FloatType>
wrap_vector_arg(detail::scalar_scalar_argument<FloatType> const & arg)
{
    return detail::vector_scalar_argument<FloatType>(arg.data);
}

template <typename FloatType>
always_inline detail::vector_pointer_argument<FloatType>
wrap_vector_arg(detail::scalar_pointer_argument<FloatType> const & arg)
{
    return detail::vector_pointer_argument<FloatType>(arg.data);
}

template <typename FloatType>
always_inline detail::vector_ramp_argument<FloatType>
wrap_vector_arg(detail::scalar_ramp_argument<FloatType> const & arg)
{
    return detail::vector_ramp_argument<FloatType>(arg.data, arg.slope_);
}

} /* namespace detail */
} /* namespace nova */

#undef always_inline


#endif /* NOVA_SIMD_DETAIL_WRAP_ARGUMENT_VECTOR_HPP */
