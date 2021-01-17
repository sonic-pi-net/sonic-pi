//  scalar signal argument wrappers
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

#ifndef NOVA_SIMD_WRAP_ARGUMENTS_HPP
#define NOVA_SIMD_WRAP_ARGUMENTS_HPP

#if defined(__GNUC__) && defined(NDEBUG)
#define always_inline inline  __attribute__((always_inline))
#else
#define always_inline inline
#endif


namespace nova {
namespace detail {

template <typename FloatType>
struct scalar_pointer_argument
{
    always_inline explicit scalar_pointer_argument(const FloatType * arg):
        data(arg)
    {}

    always_inline void increment(void)
    {
        data += 1;
    }

    always_inline FloatType get(void) const
    {
        return *data;
    }

    always_inline FloatType consume(void)
    {
        FloatType ret = *data;
        increment();
        return ret;
    }

    const FloatType * data;
};

template <typename FloatType>
struct scalar_scalar_argument
{
    always_inline explicit scalar_scalar_argument(FloatType const & arg):
        data(arg)
    {}

    always_inline void increment(void)
    {}

    always_inline FloatType get(void) const
    {
        return data;
    }

    always_inline FloatType consume(void)
    {
        return data;
    }

    FloatType data;
};

template <typename FloatType>
struct scalar_ramp_argument
{
    always_inline scalar_ramp_argument(FloatType const & base, FloatType const & slope):
        data(base), slope_(slope)
    {}

    always_inline void increment(void)
    {
        data += slope_;
    }

    always_inline FloatType get(void) const
    {
        return data;
    }

    always_inline FloatType consume(void)
    {
        FloatType ret = data;
        increment();
        return ret;
    }

    FloatType data;
    const FloatType slope_;
};

}

always_inline detail::scalar_scalar_argument<float> wrap_argument(float arg)
{
    return detail::scalar_scalar_argument<float>(arg);
}

always_inline detail::scalar_scalar_argument<double> wrap_argument(double arg)
{
    return detail::scalar_scalar_argument<double>(arg);
}

always_inline detail::scalar_pointer_argument<float> wrap_argument(const float * arg)
{
    return detail::scalar_pointer_argument<float>(arg);
}

always_inline detail::scalar_pointer_argument<double> wrap_argument(const double * arg)
{
    return detail::scalar_pointer_argument<double>(arg);
}

template <typename FloatType>
always_inline detail::scalar_ramp_argument<FloatType> wrap_argument(FloatType base, FloatType slope)
{
    return detail::scalar_ramp_argument<FloatType>(base, slope);
}

template <typename FloatType>
always_inline detail::scalar_scalar_argument<FloatType>
wrap_argument(detail::scalar_scalar_argument<FloatType> const & f)
{
    return f;
}

template <typename FloatType>
always_inline detail::scalar_pointer_argument<FloatType>
wrap_argument(detail::scalar_pointer_argument<FloatType> const & f)
{
    return f;
}

template <typename FloatType>
always_inline detail::scalar_ramp_argument<FloatType>
wrap_argument(detail::scalar_ramp_argument<FloatType> const & f)
{
    return f;
}

template <typename FloatType>
always_inline detail::scalar_scalar_argument<FloatType>
scalar_argument(FloatType const & f)
{
    return wrap_argument(f);
}

template <typename FloatType>
always_inline detail::scalar_pointer_argument<FloatType>
vector_argument(const FloatType * f)
{
    return wrap_argument(f);
}

template <typename FloatType>
always_inline detail::scalar_ramp_argument<FloatType>
slope_argument(FloatType const & value, FloatType const & slope)
{
    return wrap_argument(value, slope);
}

}

#undef always_inline

#endif /* NOVA_SIMD_WRAP_ARGUMENTS_HPP */
