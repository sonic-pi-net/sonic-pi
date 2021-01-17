//  unroll helpers
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

#ifndef NOVA_SIMD_DETAIL_UNROLL_HELPERS_HPP
#define NOVA_SIMD_DETAIL_UNROLL_HELPERS_HPP

#include "../vec.hpp"

#if defined(__GNUC__) && defined(NDEBUG)
#define always_inline inline  __attribute__((always_inline))
#else
#define always_inline inline
#endif

namespace nova {
namespace detail {

template <typename FloatType,
          int N
         >
struct compile_time_unroller
{
    typedef vec<FloatType> vec_type;

    static const int offset = vec_type::size;

    template <typename arg1_type,
              typename Functor
             >
    static always_inline void run(FloatType * out, arg1_type & in1, Functor const & f)
    {
        compile_time_unroller<FloatType, N>::mp_iteration_1(out, in1.consume(), in1, f);
    }

    template <typename arg1_type,
              typename arg2_type,
              typename Functor
             >
    static always_inline void run(FloatType * out, arg1_type & in1, arg2_type & in2, Functor const & f)
    {
        compile_time_unroller<FloatType, N>::mp_iteration_2(out, in1.consume(), in1, in2.consume(), in2, f);
    }

    template <typename arg1_type,
              typename arg2_type,
              typename arg3_type,
              typename Functor
             >
    static always_inline void run(FloatType * out, arg1_type & in1, arg2_type & in2,
                                  arg3_type & in3, Functor const & f)
    {
        compile_time_unroller<FloatType, N>::mp_iteration_3(out, in1.consume(), in1, in2.consume(), in2, in3.consume(), in3, f);
    }

    template <typename arg1_type,
              typename arg2_type,
              typename arg3_type,
              typename arg4_type,
              typename Functor
             >
    static always_inline void run(FloatType * out, arg1_type & in1, arg2_type & in2,
                                  arg3_type & in3, arg4_type & in4, Functor const & f)
    {
        compile_time_unroller<FloatType, N>::mp_iteration_4(out, in1.consume(), in1, in2.consume(), in2,
                                                            in3.consume(), in3, in4.consume(), in4, f);
    }

private:
    friend struct compile_time_unroller<FloatType, vec_type::size + N>;

    template <typename arg1_type,
              typename Functor
             >
    static always_inline void mp_iteration_1(FloatType * out, vec_type loaded_in1, arg1_type & in1, Functor const & f)
    {
        vec_type loaded_next_in1;
        if (N != offset)
            loaded_next_in1 = in1.consume();

        vec_type result = f(loaded_in1);
        result.store_aligned(out);
        compile_time_unroller<FloatType, N-offset>::mp_iteration_1(out+offset, loaded_next_in1, in1, f);
    }

    template <typename arg1_type,
              typename arg2_type,
              typename Functor
             >
    static always_inline void mp_iteration_2(FloatType * out, vec_type loaded_in1, arg1_type & in1,
                                             vec_type loaded_in2, arg2_type & in2, Functor const & f)
    {
        vec_type loaded_next_in1;
        if (N != offset)
            loaded_next_in1 = in1.consume();

        vec_type loaded_next_in2;
        if (N != offset)
            loaded_next_in2 = in2.consume();

        vec_type result = f(loaded_in1, loaded_in2);
        result.store_aligned(out);
        compile_time_unroller<FloatType, N-offset>::mp_iteration_2(out+offset, loaded_next_in1, in1, loaded_next_in2, in2, f);
    }

    template <typename arg1_type,
              typename arg2_type,
              typename arg3_type,
              typename Functor
             >
    static always_inline void mp_iteration_3(FloatType * out, vec_type loaded_in1, arg1_type & in1,
                                             vec_type loaded_in2, arg2_type & in2,
                                             vec_type loaded_in3, arg3_type & in3, Functor const & f)
    {
        vec_type loaded_next_in1;
        if (N != offset)
            loaded_next_in1 = in1.consume();

        vec_type loaded_next_in2;
        if (N != offset)
            loaded_next_in2 = in2.consume();

        vec_type loaded_next_in3;
        if (N != offset)
            loaded_next_in3 = in3.consume();

        vec_type result = f(loaded_in1, loaded_in2, loaded_in3);
        result.store_aligned(out);
        compile_time_unroller<FloatType, N-offset>::mp_iteration_3(out+offset, loaded_next_in1, in1, loaded_next_in2, in2,
                                                                   loaded_next_in3, in3, f);
    }

    template <typename arg1_type,
              typename arg2_type,
              typename arg3_type,
              typename arg4_type,
              typename Functor
             >
    static always_inline void mp_iteration_4(FloatType * out, vec_type loaded_in1, arg1_type & in1, vec_type loaded_in2, arg2_type & in2,
                                             vec_type loaded_in3, arg3_type & in3, vec_type loaded_in4, arg4_type & in4, Functor const & f)
    {
        vec_type loaded_next_in1;
        if (N != offset)
            loaded_next_in1 = in1.consume();

        vec_type loaded_next_in2;
        if (N != offset)
            loaded_next_in2 = in2.consume();

        vec_type loaded_next_in3;
        if (N != offset)
            loaded_next_in3 = in3.consume();

        vec_type loaded_next_in4;
        if (N != offset)
            loaded_next_in4 = in4.consume();

        vec_type result = f(loaded_in1, loaded_in2, loaded_in3, loaded_in4);
        result.store_aligned(out);

        compile_time_unroller<FloatType, N-offset>::mp_iteration_4(out+offset, loaded_next_in1, in1, loaded_next_in2, in2,
                                                                   loaded_next_in3, in3, loaded_next_in4, in4, f);
    }
};

template <typename FloatType>
struct compile_time_unroller<FloatType, 0>
{
    friend struct compile_time_unroller<FloatType, vec<FloatType>::size>;

private:
    template <typename LoadedArg1, typename Arg1,
              typename Functor
             >
    static always_inline void mp_iteration_1(FloatType * out, LoadedArg1 const &, Arg1 const &, Functor const & f)
    {}

    template <typename LoadedArg1, typename Arg1,
              typename LoadedArg2, typename Arg2,
              typename Functor
             >
    static always_inline void mp_iteration_2(FloatType * out, LoadedArg1 const &, Arg1 const &,
                                             LoadedArg2 const &, Arg2 const &, Functor const & f)
    {}

    template <typename LoadedArg1, typename Arg1,
              typename LoadedArg2, typename Arg2,
              typename LoadedArg3, typename Arg3,
              typename Functor
             >
    static always_inline void mp_iteration_3(FloatType * out, LoadedArg1 const &, Arg1 const &,
                                             LoadedArg2 const &, Arg2 const &, LoadedArg3 const &, Arg3 const &,
                                             Functor const & f)
    {}

    template <typename LoadedArg1, typename Arg1,
              typename LoadedArg2, typename Arg2,
              typename LoadedArg3, typename Arg3,
              typename LoadedArg4, typename Arg4,
              typename Functor
             >
    static always_inline void mp_iteration_4(FloatType * out, LoadedArg1 const &, Arg1 const &,
                                             LoadedArg2 const &, Arg2 const &, LoadedArg3 const &, Arg3 const &,
                                             LoadedArg4 const &, Arg4 const &, Functor const & f)
    {}
};


template <typename float_type,
          typename Arg1,
          typename Functor
         >
always_inline void generate_simd_loop(float_type * out, Arg1 arg1, unsigned int n, Functor const & f)
{
    const unsigned int per_loop = vec<float_type>::objects_per_cacheline;
    n /= per_loop;
    do {
        detail::compile_time_unroller<float_type, per_loop>::run(out, arg1, f);
        out += per_loop;
    } while (--n);
}

template <typename float_type,
          typename Arg1,
          typename Arg2,
          typename Functor
         >
always_inline void generate_simd_loop(float_type * out, Arg1 arg1, Arg2 arg2, unsigned int n, Functor const & f)
{
    const unsigned int per_loop = vec<float_type>::objects_per_cacheline;
    n /= per_loop;
    do {
        detail::compile_time_unroller<float_type, per_loop>::run(out, arg1, arg2, f);
        out += per_loop;
    } while (--n);
}

template <typename float_type,
          typename Arg1,
          typename Arg2,
          typename Arg3,
          typename Functor
         >
always_inline void generate_simd_loop(float_type * out, Arg1 arg1, Arg2 arg2, Arg3 arg3, unsigned int n, Functor const & f)
{
    const unsigned int per_loop = vec<float_type>::objects_per_cacheline;
    n /= per_loop;
    do {
        detail::compile_time_unroller<float_type, per_loop>::run(out, arg1, arg2, arg3, f);
        out += per_loop;
    } while (--n);
}

template <typename float_type,
          typename Arg1,
          typename Arg2,
          typename Arg3,
          typename Arg4,
          typename Functor
         >
always_inline void generate_simd_loop(float_type * out, Arg1 arg1, Arg2 arg2, Arg3 arg3, Arg4 arg4, unsigned int n, Functor const & f)
{
    const unsigned int per_loop = vec<float_type>::objects_per_cacheline;
    n /= per_loop;
    do {
        detail::compile_time_unroller<float_type, per_loop>::run(out, arg1, arg2, arg3, arg4, f);
        out += per_loop;
    } while (--n);
}

}
}

#undef always_inline

#endif /* NOVA_SIMD_DETAIL_UNROLL_HELPERS_HPP */
