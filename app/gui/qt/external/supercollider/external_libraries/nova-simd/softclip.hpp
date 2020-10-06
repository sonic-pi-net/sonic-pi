//  softclip
//  Copyright (C) 2008, 2009 Tim Blechmann
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

#ifndef SIMD_SOFTCLIP_HPP
#define SIMD_SOFTCLIP_HPP

#include <cassert>
#include <cmath>

#include "vec.hpp"

#include "detail/define_macros.hpp"

#if defined(__GNUC__) && defined(NDEBUG)
#define always_inline inline  __attribute__((always_inline))
#else
#define always_inline inline
#endif

namespace nova   {
namespace detail {

struct softclip
{
    template <typename FloatType>
    static FloatType s_run(FloatType arg)
    {
        FloatType abs = std::fabs(arg);
        if (abs < FloatType(0.5))
            return arg;
        else
            return (abs - FloatType(0.25)) / arg;
    }

    template <typename FloatType>
    always_inline FloatType operator()(FloatType arg) const
    {
        return s_run(arg);
    }

#if defined(__SSE__) || defined(__AVX__)
    /* this computes both parts of the branch
     *
     * benchmarks (core2) showed:
     * 6.5 seconds for simdfied code
     * 5.3 seconds for non-simd code for samples with abs(sample) < 0.5
     * 17 seconds                    when 50% of the samples have abs(sample) < 0.5
     * 26 seconds                    for samples with abs(sample) > 0.5
     *
     * on intel nethalem cpus, the simdfied code is faster than all non-simd code
     * */

    template <typename FloatType>
    always_inline vec<FloatType> operator()(vec<FloatType> arg) const
    {
        typedef vec<FloatType> vec_type;

        const vec_type const05 = vec_type::gen_05();
        const vec_type const025 (0.25);

        vec_type abs_ = abs(arg);
        vec_type selecter = mask_lt(abs_, const05);
        vec_type alt_ret = (abs_ - const025) / arg;

        return select(alt_ret, arg, selecter);
    }
#else
    template <typename FloatType>
    always_inline vec<FloatType> operator()(vec<FloatType> arg) const
    {
        return arg.collect(softclip::s_run<FloatType>);
    }
#endif
};

} /* namespace detail */

NOVA_SIMD_DEFINE_UNARY_WRAPPER(softclip, detail::softclip)


} /* namespace nova */

#undef always_inline

#endif /* SIMD_SOFTCLIP_HPP */
