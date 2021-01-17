//  horizontal simd functions
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


#ifndef SIMD_HORIZONTAL_FUNCTIONS_HPP
#define SIMD_HORIZONTAL_FUNCTIONS_HPP

#include "vec.hpp"

#include <algorithm>            /* for max */

#if defined(__GNUC__) && defined(NDEBUG)
#define always_inline inline  __attribute__((always_inline))
#else
#define always_inline inline
#endif

namespace nova {

/* horizontal max */
template <typename F>
inline F horizontal_max_vec(const F * in, unsigned int n)
{
    F current = std::numeric_limits<F>::min();
    using namespace std;

    do {
        current = max(current, *in++);
    } while(--n);

    return current;
}

template <typename F>
inline F horizontal_max_vec_simd(const F * in, unsigned int n)
{
    F init = std::numeric_limits<F>::min();
    vec<F> current(init);

    /* loop */
    const size_t vec_size = vec<F>::size;
    n /= vec_size;
    do {
        vec<F> val;
        val.load_aligned(in);

        current = max_(current, val);
        in += vec_size;
    } while(--n);

    return current.horizontal_max();
}


/* horizontal min */
template <typename F>
inline F horizontal_min_vec(const F * in, unsigned int n)
{
    F current = std::numeric_limits<F>::min();
    using namespace std;

    do {
        current = min(current, *in++);
    } while(--n);

    return current;
}

template <typename F>
inline F horizontal_min_vec_simd(const F * in, unsigned int n)
{
    F init = std::numeric_limits<F>::min();
    vec<F> current(init);

    /* loop */
    const size_t vec_size = vec<F>::size;
    n /= vec_size;
    do {
        vec<F> val;
        val.load_aligned(in);

        current = min_(current, val);
        in += vec_size;
    } while(--n);

    return current.horizontal_min();
}

/* horizontal sum */
template <typename F>
inline F horizontal_sum_vec(const F * in, unsigned int n)
{
    F current = 0;
    using namespace std;

    do {
        current = current + *in++;
    } while(--n);

    return current;
}

template <typename F>
inline F horizontal_sum_vec_simd(const F * in, unsigned int n)
{
    vec<F> current(F(0));

    /* loop */
    const size_t vec_size = vec<F>::size;
    n /= vec_size;
    do {
        vec<F> val;
        val.load_aligned(in);

        current = current + val;
        in += vec_size;
    } while(--n);

    return current.horizontal_sum();
}


/* horizontal min/max */
template <typename F>
inline void horizontal_minmax_vec(F & rmin, F & rmax, const F * in, unsigned int n)
{
    F current_max = std::numeric_limits<F>::min();
    F current_min = std::numeric_limits<F>::max();
    using namespace std;

    do {
        current_max = max(current_max, *in);
        current_min = min(current_min, *in++);
    } while(--n);

    rmax = current_max;
    rmin = current_min;
}

template <typename F>
inline void horizontal_minmax_vec_simd(F & rmin, F & rmax, const F * in, unsigned int n)
{
    vec<F> current_max(std::numeric_limits<F>::min());
    vec<F> current_min(std::numeric_limits<F>::max());

    /* loop */
    const size_t vec_size = vec<F>::size;
    n /= vec_size;
    do {
        vec<F> val;
        val.load_aligned(in);

        current_max = max_(current_max, val);
        current_min = min_(current_min, val);
        in += vec_size;
    } while(--n);

    rmin = current_min.horizontal_min();
    rmax = current_max.horizontal_max();
}

/* horizontal max/sum */
template <typename F>
inline void horizontal_maxsum_vec(F & rmax, F & rsum, const F * in, unsigned int n)
{
    F current_max = std::numeric_limits<F>::min();
    F current_sum = 0;
    using namespace std;

    do {
        current_max = max(current_max, *in);
        current_sum = current_sum + *in++;
    } while(--n);

    rmax = current_max;
    rsum = current_sum;
}

template <typename F>
inline void horizontal_maxsum_vec_simd(F & rmax, F & rsum, const F * in, unsigned int n)
{
    vec<F> current_max(std::numeric_limits<F>::min());
    vec<F> current_sum(F(0));

    /* loop */
    const size_t vec_size = vec<F>::size;
    n /= vec_size;
    do {
        vec<F> val;
        val.load_aligned(in);

        current_max = max_(current_max, val);
        current_sum = current_sum + val;
        in += vec_size;
    } while(--n);

    rsum = current_sum.horizontal_sum();
    rmax = current_max.horizontal_max();
}


} /* namespace nova */

#undef always_inline

#endif /* SIMD_PEAKMETER_HPP */
