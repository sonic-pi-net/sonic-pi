//  templated arithmetic simd functions
//  Copyright (C) 2009 Tim Blechmann <tim@klingt.org>
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
//
//  implemented as part of nova

#ifndef SIMD_TERNARY_ARITHMETIC_HPP
#define SIMD_TERNARY_ARITHMETIC_HPP

#include <algorithm>

#include "vec.hpp"

#include "detail/define_macros.hpp"

#if defined(__GNUC__) && defined(NDEBUG)
#define always_inline inline  __attribute__((always_inline))
#else
#define always_inline inline
#endif


namespace nova {
namespace detail {

struct clip
{
    template<typename float_type>
    float_type operator()(float_type value, float_type low, float_type high) const
    {
        return max_(min_(value, high),
                    low);
    }
};

struct muladd
{
    template<typename float_type>
    float_type operator()(float_type value, float_type mul, float_type add) const
    {
        return value * mul + add;
    }

    template<typename float_type>
    vec<float_type> operator()(vec<float_type> value, vec<float_type> mul, vec<float_type> add) const
    {
        return madd(value, mul, add);
    }
};

struct ampmod
{
    template<typename float_type>
    float_type operator()(float_type signal, float_type modulator, float_type amount) const
    {
        float_type one = 1.f;
        return signal * (one + modulator * amount);
    }
};

}


NOVA_SIMD_DEFINE_TERNARY_WRAPPER(clip, detail::clip)
NOVA_SIMD_DEFINE_TERNARY_WRAPPER(muladd, detail::muladd)
NOVA_SIMD_DEFINE_TERNARY_WRAPPER(ampmod, detail::ampmod)

}

#undef always_inline

#endif /* SIMD_TERNARY_ARITHMETIC_HPP */
