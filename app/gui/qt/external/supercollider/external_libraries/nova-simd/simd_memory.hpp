//  templated memory simd functions
//  Copyright (C) 2010 Tim Blechmann <tim@klingt.org>
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


#ifndef SIMD_MEMORY_HPP
#define SIMD_MEMORY_HPP

#include <cassert>
#include <cstring>

#include "vec.hpp"

#if defined(__GNUC__) && defined(NDEBUG)
#define always_inline inline  __attribute__((always_inline))
#else
#define always_inline inline
#endif

namespace nova {

template <typename F>
inline void zerovec(F * dest, unsigned int n)
{
    std::memset(dest, 0, n*sizeof(F));
}

template <typename F>
inline void setvec(F * dest, F f, unsigned int n)
{
    assert(n);
    do
        *dest++ = f;
    while (--n);
}

namespace detail
{

template <bool aligned, typename F>
inline void store_aligned(vec<F> const & value, F * dest)
{
    if (aligned)
        value.store_aligned(dest);
    else
        value.store(dest);
}

template <typename F, unsigned int n, bool aligned>
struct setvec
{
    static const int offset = vec<F>::size;

    static always_inline void mp_iteration(F * dst, vec<F> const & val)
    {
        store_aligned<aligned>(val, dst);
        setvec<F, n-offset, aligned>::mp_iteration(dst+offset, val);
    }
};

template <typename F, bool aligned>
struct setvec<F, 0, aligned>
{
    static always_inline void mp_iteration(F * dst, vec<F> const & val)
    {}
};

template <typename F, bool aligned>
inline void setvec_simd(F * dest, vec<F> const & val, unsigned int n)
{
    const unsigned int offset = vec<F>::objects_per_cacheline;
    unsigned int unroll = n / offset;

    do
    {
        setvec<F, offset, aligned>::mp_iteration(dest, val);
        dest += offset;
    }
    while (--unroll);
}

} /* namespace detail */

template <typename F>
inline void zerovec_simd(F * dest, unsigned int n)
{
    vec<F> zero; zero.clear();
    detail::setvec_simd<F, true>(dest, zero, n);
}

template <unsigned int n, typename F>
inline void zerovec_simd(F *dest)
{
    vec<F> zero; zero.clear();
    detail::setvec<F, n, true>::mp_iteration(dest, zero);
}

template <typename F>
inline void zerovec_na_simd(F * dest, unsigned int n)
{
    vec<F> zero; zero.clear();
    detail::setvec_simd<F, false>(dest, zero, n);
}

template <unsigned int n, typename F>
inline void zerovec_na_simd(F *dest)
{
    vec<F> zero; zero.clear();
    detail::setvec<F, n, false>::mp_iteration(dest, zero);
}



template <typename F>
inline void setvec_simd(F * dest, F f, unsigned int n)
{
    vec<F> val(f);
    detail::setvec_simd<F, true>(dest, val, n);
}

template <unsigned int n, typename F>
inline void setvec_simd(F *dest, F f)
{
    vec<F> val(f);
    detail::setvec<F, n, true>::mp_iteration(dest, val);
}

template <typename F>
inline void setvec_na_simd(F * dest, F f, unsigned int n)
{
    vec<F> val(f);
    detail::setvec_simd<F, false>(dest, val, n);
}

template <unsigned int n, typename F>
inline void setvec_na_simd(F *dest, F f)
{
    vec<F> val(f);
    detail::setvec<F, n, false>::mp_iteration(dest, val);
}


namespace detail
{

template <typename F, unsigned int n>
struct set_ramp
{
    static const int offset = vec<F>::size;

    static always_inline void slope_mp_iteration(F * dst, vec<F> & vbase, vec<F> const & vslope)
    {
        vbase.store_aligned(dst);
        vbase += vslope;
        set_ramp<F, n-offset>::slope_mp_iteration(dst+offset, vbase, vslope);
    }

    static always_inline void exp_mp_iteration(F * dst, vec<F> & vbase, vec<F> const & vcurve)
    {
        vbase.store_aligned(dst);
        vbase *= vcurve;
        set_ramp<F, n-offset>::exp_mp_iteration(dst+offset, vbase, vcurve);
    }
};

template <typename F>
struct set_ramp<F, 0>
{
    static always_inline void slope_mp_iteration(F * dst, vec<F> & vbase, vec<F> const & vslope)
    {}
    static always_inline void exp_mp_iteration(F * dst, vec<F> & vbase, vec<F> const & curve)
    {}
};

} /* namespace detail */


template <typename F>
inline void set_slope_vec(F * dest, F f, F slope, unsigned int n)
{
    assert(n);
    do {
        *dest++ = f; f += slope;
    } while (--n);
}

template <typename F>
inline void set_slope_vec_simd(F * dest, F f, F slope, unsigned int n)
{
    vec<F> vbase, vslope;
    vbase.set_slope(f, slope);
    vslope.set_vec(vec<F>::size * slope);

    unsigned int unroll = n / vec<F>::objects_per_cacheline;
    do
    {
        detail::set_ramp<F, vec<F>::objects_per_cacheline>::slope_mp_iteration(dest, vbase, vslope);
        dest += vec<F>::objects_per_cacheline;
    } while(--unroll);
}

template <typename F>
inline void set_exp_vec(F * dest, F f, F curve, unsigned int n)
{
    assert(n);
    do {
        *dest++ = f; f *= curve;
    } while (--n);
}

namespace detail {

template <int Exponent, typename Type>
class pow_i
{
public:
    static Type run(Type const & base)
    {
        return base * pow_i<Exponent - 1, Type>::run(base);
    }
};

template <typename Type>
class pow_i<1, Type>
{
public:
    static Type run(Type const & base)
    {
        return base;
    }
};

template <size_t Exponent, typename Type>
Type ipow(Type const & base)
{
    return pow_i<Exponent, Type>::run(base);
}

}

template <typename F>
inline void set_exp_vec_simd(F * dest, F f, F curve, unsigned int n)
{
    vec<F> vbase, vcurve(detail::ipow<vec<F>::size, F>(curve));
    vbase.set_exp(f, curve);

    unsigned int unroll = n / vec<F>::objects_per_cacheline;
    do
    {
        detail::set_ramp<F, vec<F>::objects_per_cacheline>::exp_mp_iteration(dest, vbase, vcurve);
        dest += vec<F>::objects_per_cacheline;
    } while(--unroll);
}


template <typename F>
inline void copyvec(F * dest, const F * src, unsigned int n)
{
    std::memcpy(dest, src, n*sizeof(F));
}

namespace detail {

template <typename F, bool src_aligned, bool dst_aligned, unsigned int n>
struct copyvec
{
    static const int offset = vec<F>::size;

    static always_inline void mp_iteration(F * dst, const F * src)
    {
        vec<F> val;
        if (src_aligned)
            val.load_aligned(src);
        else
            val.load(src);

        mp_iteration(dst, src + offset, val);
    }

    static always_inline void mp_iteration(F * dst, const F * src, vec<F> const & loaded_value)
    {
        vec<F> val;

        if (src_aligned)
            val.load_aligned(src);
        else
            val.load(src);

        store_aligned<dst_aligned>(loaded_value, dst);
        copyvec<F, src_aligned, dst_aligned, n-offset>::mp_iteration(dst+offset, src+offset, val);
    }
};

template <typename F, bool src_aligned, bool dst_aligned>
struct copyvec<F, src_aligned, dst_aligned, 0>
{
    static always_inline void mp_iteration(F * dst, const F * src, vec<F> loaded_value)
    {}
};

}

#define COPYVEC_FUNCTION(name, src_aligned, dst_aligned)                \
template <typename F>                                                   \
inline void name##_simd(F * dest, const F * src, unsigned int n)        \
{                                                                       \
    const int per_loop = vec<F>::objects_per_cacheline;                 \
    n /= per_loop;                                                      \
    do                                                                  \
    {                                                                   \
        detail::copyvec<F, src_aligned, dst_aligned, per_loop>::mp_iteration(dest, src); \
        dest += per_loop; src += per_loop;                              \
    }                                                                   \
    while (--n);                                                        \
}                                                                       \
                                                                        \
template <unsigned int n, typename F>                                   \
inline void name##_simd(F * dest, const F * src)                        \
{                                                                       \
    detail::copyvec<F, src_aligned, dst_aligned, n>::mp_iteration(dest, src); \
}

COPYVEC_FUNCTION(copyvec_aa, true, true)
COPYVEC_FUNCTION(copyvec_na, false, true)
COPYVEC_FUNCTION(copyvec_an, true, false)
COPYVEC_FUNCTION(copyvec_nn, false, false)

template <typename F>
inline void copyvec_simd(F * dest, const F * src, unsigned int n)
{
    copyvec_aa_simd(dest, src, n);
}

template <unsigned int n, typename F>
inline void copyvec_simd(F * dest, const F * src)
{
    copyvec_aa_simd<n, F>(dest, src);
}

template <typename F>
inline void addvec(F * out, const F * in, unsigned int n)
{
    do {
        *out++ += *in++;
    } while (--n);
}

template <typename F>
inline void addvec(F * out, const F in, unsigned int n)
{
    do {
        *out++ += in;
    } while (--n);
}

template <typename F>
inline void addvec(F * out, const F in, const F slope, unsigned int n)
{
    do {
        *out++ += in; in += slope;
    } while (--n);
}

namespace detail
{

template <typename F, unsigned int n>
struct addvec
{
    static const int offset = vec<F>::size;

    static always_inline void mp_iteration(F * dst, const F * src)
    {
        vec<F> v1, v2;
        v1.load_aligned(dst);
        v2.load_aligned(src);
        v1 += v2;
        v1.store_aligned(dst);
        addvec<F, n-offset>::mp_iteration(dst+offset, src+offset);
    }

    static always_inline void mp_iteration(F * dst, vec<F> const & in)
    {
        vec<F> v1;
        v1.load_aligned(dst);
        v1 += in;
        v1.store_aligned(dst);
        addvec<F, n-offset>::mp_iteration(dst+offset, in);
    }

    static always_inline void mp_iteration(F * dst, vec<F> & in, vec<F> const & vslope)
    {
        vec<F> v1;
        v1.load_aligned(dst);
        v1 += in;
        v1.store_aligned(dst);
        in += vslope;
        addvec<F, n-offset>::mp_iteration(dst+offset, in, vslope);
    }
};

template <typename F>
struct addvec<F, 0>
{
    static always_inline void mp_iteration(F * dst, const F * src)
    {}

    static always_inline void mp_iteration(F * dst, vec<F> const & in)
    {}

    static always_inline void mp_iteration(F * dst, vec<F> & in, vec<F> const & vslope)
    {}
};

}

template <typename F>
inline void addvec_simd(F * out, const F * in, unsigned int n)
{
    const int per_loop = vec<F>::objects_per_cacheline;
    n /= per_loop;
    do
    {
        detail::addvec<F, per_loop>::mp_iteration(out, in);
        out += per_loop; in += per_loop;
    }
    while (--n);
}

template <typename F>
inline void addvec_simd(F * out, const F in, unsigned int n)
{
    const int per_loop = vec<F>::objects_per_cacheline;
    vec<F> vin(in);
    n /= per_loop;
    do
    {
        detail::addvec<F, per_loop>::mp_iteration(out, vin);
        out += per_loop;
    }
    while (--n);
}

template <typename F>
inline void addvec_simd(F * out, const F in, const F slope, unsigned int n)
{
    const int per_loop = vec<F>::objects_per_cacheline;
    vec<F> vin; vin.set_slope(in, slope);
    vec<F> vslope; vslope.set(slope * vec<F>::size);
    n /= per_loop;
    do
    {
        detail::addvec<F, per_loop>::mp_iteration(out, vin, vslope);
        out += per_loop;
    }
    while (--n);
}

template <unsigned int n, typename F>
inline void addvec_simd(F * out, const F * in)
{
    detail::addvec<F, n>::mp_iteration(out, in);
}

template <unsigned int n, typename F>
inline void addvec_simd(F * out, const F in)
{
    vec<F> vin(in);
    detail::addvec<F, n>::mp_iteration(out, vin);
}

template <unsigned int n, typename F>
inline void addvec_simd(F * out, const F in, const F slope)
{
    vec<F> vin; vin.set_slope(in, slope);
    vec<F> vslope; vslope.set(slope * vec<F>::size);
    detail::addvec<F, n>::mp_iteration(out, vin, vslope);
}


} /* namespace nova */

#undef always_inline

#endif /* SIMD_MEMORY_HPP */
