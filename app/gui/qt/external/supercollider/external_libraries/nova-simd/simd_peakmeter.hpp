//  simd functions for peak metering
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


#ifndef SIMD_PEAKMETER_HPP
#define SIMD_PEAKMETER_HPP

#include "vec.hpp"

#include <cmath>                /* for abs */
#include <algorithm>            /* for max */

#if defined(__GNUC__) && defined(NDEBUG)
#define always_inline inline  __attribute__((always_inline))
#else
#define always_inline inline
#endif

namespace nova
{

/* updates peak, returns last abs(in[n-1]) */
template <typename F>
inline F peak_vec(const F * in, F * peak, unsigned int n)
{
    F last;
    F local_peak = *peak;
    using namespace std;

    do {
        last = std::fabs(*in++);
        local_peak = max(local_peak, last);
    } while(--n);

    *peak = local_peak;
    return last;
}

template <typename F>
inline F peak_vec_simd(const F * in, F * peak, unsigned int n)
{
    vec<F> maximum, abs3;
    maximum.load_first(peak);

    /* loop */
    const size_t vec_size = vec<F>::size;
    const size_t unroll = 4 * vec_size;
    n /= unroll;
    do {
        vec<F> in0, in1, in2, in3;
        in0.load_aligned(in);
        in1.load_aligned(in+vec_size);
        in2.load_aligned(in+2*vec_size);
        in3.load_aligned(in+3*vec_size);

        vec<F> abs0 = abs(in0);
        vec<F> abs1 = abs(in1);
        vec<F> abs2 = abs(in2);
        abs3 = abs(in3);

        vec<F> local_max = max_(max_(abs0, abs1),
                               max_(abs2, abs3));

        maximum = max_(maximum, local_max);
        in += unroll;
    } while(--n);

    /* horizonal accumulation */
    *peak = maximum.horizontal_max();

    /* return absolute of last sample */
    return abs3.get(vec<F>::size - 1);
}

/* updates peak and squared sum */
template <typename F>
inline void peak_rms_vec(const F * in, F * peak, F * squared_sum, unsigned int n)
{
    F local_peak = *peak;
    F local_squared_sum = *squared_sum;
    using namespace std;

    do {
        F in_sample = *in++;
        F last = std::fabs(in_sample);
        local_peak = max(local_peak, last);
        local_squared_sum += in_sample * in_sample;
    } while(--n);

    *peak = local_peak;
    *squared_sum = local_squared_sum;
}

template <typename F>
inline void peak_rms_vec_simd(const F * in, F * peak, F * squared_sum, unsigned int n)
{
    vec<F> maximum;
    maximum.load_first(peak);

    vec<F> local_squared_sum;
    local_squared_sum.load_first(squared_sum);

    /* loop */
    const size_t vec_size = vec<F>::size;
    const size_t unroll = 4 * vec_size;
    n /= unroll;
    do {
        vec<F> in0, in1, in2, in3;
        in0.load_aligned(in);
        in1.load_aligned(in+vec_size);
        in2.load_aligned(in+2*vec_size);
        in3.load_aligned(in+3*vec_size);

        vec<F> abs0 = abs(in0);
        vec<F> sqr0 = square(in0);
        vec<F> abs1 = abs(in1);
        vec<F> sqr1 = square(in1);
        vec<F> abs2 = abs(in2);
        vec<F> sqr2 = square(in2);
        vec<F> abs3 = abs(in3);
        vec<F> sqr3 = square(in3);

        vec<F> local_max = max_(max_(abs0, abs1),
                                max_(abs2, abs3));
        maximum = max_(maximum, local_max);

        local_squared_sum += sqr0 + sqr1 + sqr2 + sqr3;

        in += unroll;
    } while(--n);

    /* horizonal accumulation */
    *peak = maximum.horizontal_max();
    *squared_sum = local_squared_sum.horizontal_sum();
}


} /* namespace nova */

#undef always_inline

#endif /* SIMD_PEAKMETER_HPP */
