//  vector base class
//
//  Copyright (C) 2011 Tim Blechmann
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

#ifndef VEC_BASE_HPP
#define VEC_BASE_HPP

#include <cassert>
#include <functional>
#include <cstring>

#include "../detail/math.hpp"

#if defined(__GNUC__) && defined(NDEBUG)
#define always_inline inline  __attribute__((always_inline))
#else
#define always_inline inline
#endif

#include "stdint.h"

namespace nova {


/* vector base class
 *
 * requirements:
 * - WrappedType is the wrapped scalar type
 * - get_pointer(VecType) should return WrappedType*
 * - VecSize should be the number of WrappedType elements inside a VecType
 *
 */
template <typename WrappedType,
          typename VecType,
          int VecSize>
class vec_base
{
    typedef union {
        WrappedType f[VecSize];
        VecType vec;
    } cast_unit;

public:
    static const int size = VecSize;
    static const bool has_compare_bitmask = false;

protected:
    vec_base (void)
    {}

public:
    vec_base (VecType arg):
        data_(arg)
    {}

    operator VecType (void) const
    {
        return data_;
    }

public:
    /* @{ */
    /** io */
    void load(const WrappedType * src)
    {
        cast_unit u;
        for (int i = 0; i != size; ++i)
            u.f[i] = src[i];
        data_ = u.vec;
    }

    void load_first(const WrappedType * src)
    {
        cast_unit u;
        u.f[0] = *src;
        for (int i = 1; i != size; ++i)
            u.f[i] = 0;
        data_ = u.vec;
    }

    void load_aligned(const WrappedType * data)
    {
        load(data);
    }

    void store(WrappedType * dest) const
    {
        cast_unit u;
        u.vec = data_;
        for (int i = 0; i != size; ++i)
            dest[i] = u.f[i];
    }

    void store_aligned(WrappedType * dest) const
    {
        store(dest);
    }

    void store_aligned_stream(WrappedType * dest) const
    {
        store(dest);
    }

    void clear(void)
    {
        set_vec(0);
    }
    /* @} */


    /* @{ */
    /** element access */
    WrappedType get (int index) const
    {
        assert(index < size);
        cast_unit u;
        u.vec = data_;
        return u.f[index];
    }

    void set (int index, WrappedType arg)
    {
        cast_unit u;
        u.vec = data_;
        u.f[index] = arg;
        data_ = u.vec;
    }

    void set_vec (WrappedType value)
    {
        cast_unit u;
        for (int i = 0; i != size; ++i)
            u.f[i] = value;
        data_ = u.vec;
    }

    WrappedType set_slope(WrappedType start, WrappedType slope)
    {
        WrappedType diff = 0;
        cast_unit u;

        for (int i = 0; i != size; ++i)
        {
            u.f[i] = start + diff;
            diff += slope;
        }
        data_ = u.vec;
        return diff;
    }

    WrappedType set_exp(WrappedType start, WrappedType curve)
    {
        WrappedType value = start;
        cast_unit u;
        for (int i = 0; i != size; ++i)
        {
            u.f[i] = value;
            value *= curve;
        }
        data_ = u.vec;
        return value;
    }
    /* @} */

private:
    template <typename Functor>
    static always_inline VecType apply_unary(VecType const & arg, Functor const & f)
    {
        cast_unit u;
        u.vec = arg;

        for (int i = 0; i != VecSize; ++i)
            u.f[i] = f(u.f[i]);
        return u.vec;
    }

    template <typename Functor>
    static always_inline VecType apply_binary(VecType const & arg1, VecType const & arg2, Functor const & f)
    {
        cast_unit a1, a2, ret;
        a1.vec = arg1;
        a2.vec = arg2;

        for (int i = 0; i != VecSize; ++i)
            ret.f[i] = f(a1.f[i], a2.f[i]);
        return ret.vec;
    }

public:
    vec_base operator+(vec_base const & rhs) const
    {
        return vec_base::apply_binary(data_, rhs.data_, std::plus<WrappedType>());
    }

    vec_base operator-(vec_base const & rhs) const
    {
        return vec_base::apply_binary(data_, rhs.data_, std::minus<WrappedType>());
    }

    vec_base operator*(vec_base const & rhs) const
    {
        return vec_base::apply_binary(data_, rhs.data_, std::multiplies<WrappedType>());
    }

    vec_base operator/(vec_base const & rhs) const
    {
        return vec_base::apply_binary(data_, rhs.data_, std::divides<WrappedType>());
    }

    vec_base & operator+=(vec_base const & rhs)
    {
        data_ = vec_base::apply_binary(data_, rhs.data_, std::plus<WrappedType>());
        return *this;
    }

    vec_base & operator-=(vec_base const & rhs)
    {
        data_ = vec_base::apply_binary(data_, rhs.data_, std::minus<WrappedType>());
        return *this;
    }

    vec_base & operator*=(vec_base const & rhs)
    {
        data_ = vec_base::apply_binary(data_, rhs.data_, std::multiplies<WrappedType>());
        return *this;
    }

    vec_base & operator/=(vec_base const & rhs)
    {
        data_ = vec_base::apply_binary(data_, rhs.data_, std::divides<WrappedType>());
        return *this;
    }

    vec_base operator<(vec_base const & rhs) const
    {
        return vec_base::apply_binary(data_, rhs.data_, std::less<WrappedType>());
    }

    vec_base operator<=(vec_base const & rhs) const
    {
        return vec_base::apply_binary(data_, rhs.data_, std::less_equal<WrappedType>());
    }

    vec_base operator==(vec_base const & rhs) const
    {
        return vec_base::apply_binary(data_, rhs.data_, std::equal_to<WrappedType>());
    }

    vec_base operator!=(vec_base const & rhs) const
    {
        return vec_base::apply_binary(data_, rhs.data_, std::not_equal_to<WrappedType>());
    }

    vec_base operator>(vec_base const & rhs) const
    {
        return vec_base::apply_binary(data_, rhs.data_, std::greater<WrappedType>());
    }

    vec_base operator>=(vec_base const & rhs) const
    {
        return vec_base::apply_binary(data_, rhs.data_, std::greater_equal<WrappedType>());
    }

#define DEFINE_UNARY_STATIC(NAME, METHOD)                   \
    static always_inline VecType NAME(VecType const & arg)  \
    {                                                       \
       return apply_unary(arg, METHOD<WrappedType>);        \
    }

#define DEFINE_BINARY_STATIC(NAME, METHOD)                                          \
    static always_inline VecType NAME(VecType const & arg1, VecType const & arg2)   \
    {                                                                               \
       return apply_binary(arg1, arg2, METHOD<WrappedType>);                        \
    }


protected:
    DEFINE_UNARY_STATIC(reciprocal, detail::reciprocal)

    DEFINE_UNARY_STATIC(sin, detail::sin)
    DEFINE_UNARY_STATIC(cos, detail::cos)
    DEFINE_UNARY_STATIC(tan, detail::tan)

    DEFINE_UNARY_STATIC(asin, detail::asin)
    DEFINE_UNARY_STATIC(acos, detail::acos)
    DEFINE_UNARY_STATIC(atan, detail::atan)

    DEFINE_UNARY_STATIC(tanh, detail::tanh)

    DEFINE_UNARY_STATIC(log, detail::log)
    DEFINE_UNARY_STATIC(log2, detail::log2)
    DEFINE_UNARY_STATIC(log10, detail::log10)
    DEFINE_UNARY_STATIC(exp, detail::exp)
    DEFINE_UNARY_STATIC(signed_sqrt, detail::signed_sqrt)

    DEFINE_UNARY_STATIC(round, detail::round)
    DEFINE_UNARY_STATIC(ceil, detail::ceil)
    DEFINE_UNARY_STATIC(floor, detail::floor)
    DEFINE_UNARY_STATIC(frac, detail::frac)
    DEFINE_UNARY_STATIC(trunc, detail::trunc)

    DEFINE_BINARY_STATIC(pow, detail::pow)
    DEFINE_BINARY_STATIC(signed_pow, detail::signed_pow)

    DEFINE_UNARY_STATIC(abs, detail::fabs)
    DEFINE_UNARY_STATIC(sign, detail::sign)
    DEFINE_UNARY_STATIC(square, detail::square)
    DEFINE_UNARY_STATIC(cube, detail::cube)

    DEFINE_BINARY_STATIC(max_, detail::max)
    DEFINE_BINARY_STATIC(min_, detail::min)

    DEFINE_UNARY_STATIC(undenormalize, detail::undenormalize)

public:
    WrappedType horizontal_min(void) const
    {
        cast_unit u;
        u.vec = data_;
        return *std::min_element(u.f, u.f + size);
    }

    WrappedType horizontal_max(void) const
    {
        cast_unit u;
        u.vec = data_;
        return *std::max_element(u.f, u.f + size);
    }

    WrappedType horizontal_sum(void) const
    {
        cast_unit u;
        u.vec = data_;
        WrappedType ret = 0;
        for (int i = 0; i != size; ++i)
            ret += u.f[i];
        return ret;
    }

    template <typename Functor>
    VecType collect(Functor const & f)
    {
        vec_base ret;
        for (int i = 0; i != size; ++i)
            ret.set(i, f(get(i)));
        return ret.data_;
    }

protected:
    VecType data_;
};

}

#define NOVA_SIMD_DELEGATE_UNARY_TO_BASE(NAME)  \
    inline friend vec NAME(vec const & arg)     \
    {                                           \
        return base::NAME(arg.data_);            \
    }

#define NOVA_SIMD_DELEGATE_OPERATOR_TO_BASE(NAME)  \
    inline vec NAME(vec const & rhs) const         \
    {                                              \
        return base::NAME(rhs.data_);               \
    }

#define NOVA_SIMD_DELEGATE_BINARY_TO_BASE(NAME)                 \
    inline friend vec NAME(vec const & arg1, vec const & arg2)  \
    {                                                           \
        return base::NAME(arg1.data_, arg2.data_);               \
    }

#define NOVA_SIMD_DEFINE_MADD                                   \
    inline friend vec madd(vec const & arg1, vec const & arg2, vec const & arg3)  \
    {                                                           \
        return arg1 * arg2 + arg3;                       \
    }

#undef always_inline


#endif /* VEC_BASE_HPP */
