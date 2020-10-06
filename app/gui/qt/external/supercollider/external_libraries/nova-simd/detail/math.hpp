//  genertic math functions
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

#ifndef NOVA_SIMD_DETAIL_MATH_HPP
#define NOVA_SIMD_DETAIL_MATH_HPP

#include <algorithm>
#include <cmath>
#include <functional>
#include <limits>


namespace nova {

namespace detail {

///@{
template <typename FloatType,
          int VectorSize,
          typename Arg1Type,
          typename Functor
         >
inline void apply_on_vector(FloatType * out, Arg1Type in1, Functor f)
{
    for (int i = 0; i != VectorSize; ++i) {
        *out++ = f(in1.get());
        in1.increment();
    }
}

template <typename FloatType,
          int VectorSize,
          typename Arg1Type,
          typename Arg2Type,
          typename Functor
         >
inline void apply_on_vector(FloatType * out, Arg1Type in1, Arg2Type in2, Functor f)
{
    for (int i = 0; i != VectorSize; ++i) {
        *out++ = f(in1.get(), in2.get());
        in1.increment();
        in2.increment();
    }
}

template <typename FloatType,
          int VectorSize,
          typename Arg1Type,
          typename Arg2Type,
          typename Arg3Type,
          typename Functor
         >
inline void apply_on_vector(FloatType * out, Arg1Type in1, Arg2Type in2, Arg3Type in3, Functor f)
{
    for (int i = 0; i != VectorSize; ++i) {
        *out++ = f(in1.get(), in2.get(), in3.get());
        in1.increment();
        in2.increment();
        in3.increment();
    }
}

template <typename FloatType,
          int VectorSize,
          typename Arg1Type,
          typename Arg2Type,
          typename Arg3Type,
          typename Arg4Type,
          typename Functor
         >
inline void apply_on_vector(FloatType * out, Arg1Type in1, Arg2Type in2, Arg3Type in3, Arg4Type in4, Functor f)
{
    for (int i = 0; i != VectorSize; ++i) {
        *out++ = f(in1.get(), in2.get(), in3.get(), in4.get());
        in1.increment();
        in2.increment();
        in3.increment();
        in4.increment();
    }
}

///@}

///@{
template <typename FloatType,
          typename Functor
         >
inline void apply_on_vector(FloatType * out, const FloatType * in, unsigned int n, Functor f)
{
    do
        *out++ = f(*in++);
    while (--n);
}

template <typename FloatType,
          typename Arg1Type,
          typename Arg2Type,
          typename Functor
         >
inline void apply_on_vector(FloatType * out, Arg1Type in1, Arg2Type in2, unsigned int n, Functor f)
{
    do
    {
        *out++ = f(in1.get(), in2.get());
        in1.increment();
        in2.increment();
    }
    while (--n);
}

template <typename FloatType,
          typename Arg1Type,
          typename Arg2Type,
          typename Arg3Type,
          typename Functor
         >
inline void apply_on_vector(FloatType * out, Arg1Type in1, Arg2Type in2, Arg3Type in3, unsigned int n, Functor f)
{
    do
    {
        *out++ = f(in1.get(), in2.get(), in3.get());
        in1.increment();
        in2.increment();
        in3.increment();
    }
    while (--n);
}

template <typename FloatType,
          typename Arg1Type,
          typename Arg2Type,
          typename Arg3Type,
          typename Arg4Type,
          typename Functor
         >
inline void apply_on_vector(FloatType * out, Arg1Type in1, Arg2Type in2, Arg3Type in3, Arg4Type in4, unsigned int n, Functor f)
{
    do
    {
        *out++ = f(in1.get(), in2.get(), in3.get(), in4.get());
        in1.increment();
        in2.increment();
        in3.increment();
        in4.increment();
    }
    while (--n);
}

///@}


#define DEFINE_STD_UNARY_WRAPPER(NAME)          \
template<typename float_type>                   \
inline float_type NAME(float_type const & x)    \
{                                               \
    return std::NAME(x);                        \
}

DEFINE_STD_UNARY_WRAPPER(fabs)
DEFINE_STD_UNARY_WRAPPER(floor)
DEFINE_STD_UNARY_WRAPPER(ceil)

DEFINE_STD_UNARY_WRAPPER(sin)
DEFINE_STD_UNARY_WRAPPER(cos)
DEFINE_STD_UNARY_WRAPPER(tan)
DEFINE_STD_UNARY_WRAPPER(asin)
DEFINE_STD_UNARY_WRAPPER(acos)
DEFINE_STD_UNARY_WRAPPER(atan)

DEFINE_STD_UNARY_WRAPPER(tanh)

DEFINE_STD_UNARY_WRAPPER(log)
DEFINE_STD_UNARY_WRAPPER(log10)
DEFINE_STD_UNARY_WRAPPER(exp)


#define DEFINE_STD_BINARY_WRAPPER(NAME)         \
template<typename float_type>                   \
inline float_type NAME(float_type const & lhs, float_type const & rhs)  \
{                                               \
    return std::NAME(lhs, rhs);                 \
}

DEFINE_STD_BINARY_WRAPPER(pow)

template<typename float_type>
inline float_type sign(float_type const & f)
{
    if (f > 0)
        return 1;
    if (f == 0)
        return 0;
    else
        return -1;
}

template<typename float_type>
inline float_type square(float_type const & f)
{
    return f*f;
}

template<typename float_type>
inline float_type cube(float_type const & f)
{
    return f*f*f;
}


template<typename float_type>
inline float_type min(float_type const & x, float_type const & y)
{
    return std::min(x, y);
}

template<typename float_type>
inline float_type max(float_type const & x, float_type const & y)
{
    return std::max(x, y);
}

template<typename float_type>
inline float_type round(float_type const & arg)
{
    return std::floor(arg + float_type(0.5));
}

#ifndef _MSC_VER // Fabian Aussems: visual c++ does not have round
template<>
inline double round<double>(double const & arg)
{
    return ::round(arg);
}
#endif

#if _XOPEN_SOURCE >= 600 || _ISOC99_SOURCE
template<>
inline float round<float>(float const & arg)
{
    return ::roundf(arg);
}
#endif

template<typename float_type>
inline float_type frac(float_type const & arg)
{
    return arg - floor<float_type>(arg);
}

template <typename float_type>
inline float_type log2(float_type arg)
{
#if __cplusplus >= 201103L
    return std::log2(arg);
#else
    const float rlog2 = 1.f/std::log(2.f);
    return std::log(arg) * rlog2;
#endif
}

#if !(__cplusplus >= 201103L) // C++11

#if __STDC_VERSION__ >= 199901L // C99
template <>
inline float log2(float arg)
{
    return ::log2f(arg);
}

template <>
inline double log2(double arg)
{
    return ::log2(arg);
}
#endif

#endif

template<typename float_type>
inline float_type trunc(float_type const & arg)
{
    return (float_type)(long)arg;
}


template <typename float_type>
inline float_type signed_sqrt(float_type in0)
{
    if (in0 >= 0)
        return std::sqrt(in0);
    else
        return -std::sqrt(-in0);
}

template <typename float_type>
inline float_type signed_pow(float_type in0, float_type in1)
{
    if (in0 > 0)
        return std::pow(in0, in1);
    else
        return -std::pow(-in0, in1);
}

template <typename float_type>
inline float_type undenormalize(float_type arg)
{
    const float_type min_positive_value = std::numeric_limits<float_type>::min();
    if (arg > 0) {
        if (arg < min_positive_value)
            return 0.0;
        else
            return arg;
    } else {
        if (arg > -min_positive_value)
            return 0.0;
        else
            return arg;
    }
}

template <typename float_type>
inline float_type reciprocal(float_type in)
{
    return float_type(1) / in;
}

}
}

#endif /* NOVA_SIMD_DETAIL_MATH_HPP */
