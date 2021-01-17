//  generic vector class
//
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

#ifndef VEC_GENERIC_HPP
#define VEC_GENERIC_HPP

#include <cmath>
#include <algorithm>
#include <numeric>

#include "../detail/wrap_arguments.hpp"
#include "../detail/math.hpp"
#include "vec_base.hpp"

namespace nova
{

namespace detail
{

template <typename T, int size>
struct array
{
    friend T* get_pointer(array & arg)
    {
        return arg.data;
    }

    friend const T * get_pointer(array const & arg)
    {
        return arg.data;
    }

    T operator[](int index) const
    {
        return data[index];
    }

    T & operator[](int index)
    {
        return data[index];
    }

    T data[size];
};

}

template <typename FloatType>
class vec:
    public vec_base<FloatType, detail::array<FloatType, 4>, 4>
{
    typedef vec_base<FloatType, detail::array<FloatType, 4>, 4> base;

public:
    typedef FloatType float_type;

    static const int size = 4;
    static const int objects_per_cacheline = 64/sizeof(float_type);

    /* @{ */
    /** constructors */
    vec(void)
    {}

    vec(double f)
    {
        base::set_vec(f);
    }

    vec(float f)
    {
        base::set_vec(f);
    }

    vec(vec const & rhs):
        base(rhs.data_)
    {}

    vec(detail::array<FloatType, 4> const & rhs)
    {
        base::data_ = rhs;
    }

    vec(base const & arg):
        base(arg)
    {}

    static bool is_aligned(FloatType * ptr)
    {
        return ((intptr_t)(ptr) & (intptr_t)(sizeof(float) - 1)) == 0;
    }

    static vec gen_zero()
    {
        return vec(0.0);
    }

    static vec gen_ones()
    {
        return vec(1.0);
    }

public:
    /* @} */

    NOVA_SIMD_DELEGATE_OPERATOR_TO_BASE(operator+)
    NOVA_SIMD_DELEGATE_OPERATOR_TO_BASE(operator-)
    NOVA_SIMD_DELEGATE_OPERATOR_TO_BASE(operator*)
    NOVA_SIMD_DELEGATE_OPERATOR_TO_BASE(operator/)

    /* @{ */
    /** unary functions */
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(abs)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(sign)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(square)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(cube)
    /* @} */

    /* @{ */
    NOVA_SIMD_DELEGATE_BINARY_TO_BASE(max_)
    NOVA_SIMD_DELEGATE_BINARY_TO_BASE(min_)
    /* @} */


    /* @{ */
    /** rounding functions */
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(round)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(frac)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(floor)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(ceil)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(trunc)
    /* @} */

    /* @{ */
    NOVA_SIMD_DEFINE_MADD

    friend inline vec select(vec lhs, vec rhs, vec bitmask)
    {
        vec ret;
        for (int i = 0; i != size; ++i) {
            ret.data_[i] = bitmask.data_[i] != 0 ? rhs.data_[i]
                                                 : lhs.data_[i];
        }
        return ret;
    }

    /** mathematical functions */
    NOVA_SIMD_DELEGATE_BINARY_TO_BASE(pow)
    NOVA_SIMD_DELEGATE_BINARY_TO_BASE(signed_pow)

    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(reciprocal)

    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(log)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(log2)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(log10)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(exp)

    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(sin)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(cos)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(tan)

    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(asin)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(acos)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(atan)

    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(tanh)

    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(signed_sqrt)
    NOVA_SIMD_DELEGATE_UNARY_TO_BASE(undenormalize)
    /* @} */
};

} /* namespace nova */

#endif /* VEC_GENERIC_HPP */
