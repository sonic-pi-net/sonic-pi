//  simd functions for mixing
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


#ifndef SIMD_MIX_HPP
#define SIMD_MIX_HPP

#include "vec.hpp"
#include "detail/define_macros.hpp"
#include "detail/wrap_argument_vector.hpp"

namespace nova
{

#if defined(__GNUC__) && defined(NDEBUG)
#define always_inline inline  __attribute__((always_inline))
#else
#define always_inline inline
#endif

namespace detail {

struct scaled_mix2
{
    template<typename ArgType>
    always_inline ArgType operator()(ArgType sig0, ArgType factor0, ArgType sig1, ArgType factor1) const
    {
        return sig0 * factor0 + sig1 * factor1;
    }
};

struct sum
{
    template<typename ArgType>
    always_inline ArgType operator()(ArgType sig0, ArgType sig1) const
    {
        return sig0 + sig1;
    }

    template<typename ArgType>
    always_inline ArgType operator()(ArgType sig0, ArgType sig1, ArgType sig2) const
    {
        return sig0 + sig1 + sig2;
    }

    template<typename ArgType>
    always_inline ArgType operator()(ArgType sig0, ArgType sig1, ArgType sig2, ArgType sig3) const
    {
        return (sig0 + sig1) + (sig2 + sig3);
    }
};

}

NOVA_SIMD_DEFINE_4ARY_WRAPPER(mix, detail::scaled_mix2)

NOVA_SIMD_DEFINE_BINARY_WRAPPER(sum, detail::sum)
NOVA_SIMD_DEFINE_TERNARY_WRAPPER(sum, detail::sum)
NOVA_SIMD_DEFINE_4ARY_WRAPPER(sum, detail::sum)

}

#undef always_inline

#endif /* SIMD_MIX_HPP */
