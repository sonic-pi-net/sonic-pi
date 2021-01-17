//  simd functions for panning
//  Copyright (C) 2009, 2010 Tim Blechmann
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


#ifndef SIMD_PAN_HPP
#define SIMD_PAN_HPP

#include "vec.hpp"

#if defined(__GNUC__) && defined(NDEBUG)
#define always_inline inline  __attribute__((always_inline))
#else
#define always_inline inline
#endif

namespace nova
{

template <typename F>
inline void pan2_vec(F * out0, F * out1, const F * in, F factor0, F factor1, unsigned int n)
{
    do
    {
        F sig = *in++;
        *out0++ = sig * factor0;
        *out1++ = sig * factor1;
    } while(--n);
}

template <typename F>
inline void pan2_vec(F * out0, F * out1, const F * in, F factor0, F slope0, F factor1, F slope1, unsigned int n)
{
    do
    {
        F sig = *in++;
        *out0++ = sig * factor0;
        *out1++ = sig * factor1;
        factor0 += slope0;
        factor1 += slope1;
    } while(--n);
}

namespace detail
{

template <typename F, unsigned int n>
struct pan2
{
    static const int offset = vec<F>::size;

    static always_inline void mp_iteration(F * out0, F * out1, const F * in, vec<F> const & factor0, vec<F> const & factor1)
    {
        vec<F> vin, vout0, vout1;
        vin.load_aligned(in);

        vout0 = vin * factor0;
        vout1 = vin * factor1;

        vout0.store_aligned(out0);
        vout1.store_aligned(out1);

        pan2<F, n-offset>::mp_iteration(out0+offset, out1+offset, in+offset, factor0, factor1);
    }

    static always_inline void mp_iteration(F * out0, F * out1, const F * in, vec<F> & factor0, vec<F> const & slope0,
                                           vec<F> & factor1, vec<F> const & slope1)
    {
        vec<F> vin, vout0, vout1;
        vin.load_aligned(in);

        vout0 = vin * factor0;
        vout1 = vin * factor1;

        vout0.store_aligned(out0);
        vout1.store_aligned(out1);
        factor0 += slope0;
        factor1 += slope1;

        pan2<F, n-offset>::mp_iteration(out0+offset, out1+offset, in+offset, factor0, slope0, factor1, slope1);
    }
};


template <typename F>
struct pan2<F, 0>
{
    static always_inline void mp_iteration(F * out0, F * out1, const F * in, vec<F> const & factor0, vec<F> const & factor1)
    {}

    static always_inline void mp_iteration(F * out0, F * out1, const F * in, vec<F> & factor0, vec<F> const & slope0,
                                           vec<F> & factor1, vec<F> const & slope1)
    {}
};

} /* namespace detail */

template <typename F>
inline void pan2_vec_simd(F * out0, F * out1, const F * in, F factor0, F factor1, unsigned int n)
{
    vec<F> vf0(factor0), vf1(factor1);
    const int per_loop = vec<F>::objects_per_cacheline;

    n /= per_loop;
    do {
        detail::pan2<F, per_loop>::mp_iteration(out0, out1, in, vf0, vf1);
        out0 += per_loop; out1 += per_loop; in += per_loop;
    } while(--n);
}

template <unsigned int n, typename F>
inline void pan2_vec_simd(F * out0, F * out1, const F * in, F factor0, F factor1)
{
    vec<F> vf0(factor0), vf1(factor1);

    detail::pan2<F, n>::mp_iteration(out0, out1, in, vf0, vf1);
}

template <typename F>
inline void pan2_vec_simd(F * out0, F * out1, const F * in, F factor0, F slope0, F factor1, F slope1, unsigned int n)
{
    const int per_loop = vec<F>::objects_per_cacheline;

    vec<F> vf0, vf1, vslope0, vslope1;
    vslope0.set_vec(vf0.set_slope(factor0, slope0));
    vslope1.set_vec(vf1.set_slope(factor1, slope1));

    n /= per_loop;
    do {
        detail::pan2<F, per_loop>::mp_iteration(out0, out1, in, vf0, vslope0, vf1, vslope1);
        out0 += per_loop; out1 += per_loop; in += per_loop;
    } while(--n);
}

template <unsigned int n, typename F>
inline void pan2_vec_simd(F * out0, F * out1, const F * in, F factor0, F slope0, F factor1, F slope1)
{
    vec<F> vf0, vf1, vslope0, vslope1;
    vslope0.set_vec(vf0.set_slope(factor0, slope0));
    vslope1.set_vec(vf1.set_slope(factor1, slope1));

    detail::pan2<F, n>::mp_iteration(out0, out1, in, vf0, vslope0, vf1, vslope1);
}

} /* namespace nova */

#undef always_inline

#endif /* SIMD_PAN_HPP */
