//  helper macros for defining the external interface functions
//  Copyright (C) 2010-2012 Tim Blechmann
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

#ifndef NOVA_SIMD_DETAIL_DEFINE_MACROS_HPP
#define NOVA_SIMD_DETAIL_DEFINE_MACROS_HPP

#include "wrap_argument_vector.hpp"
#include "unroll_helpers.hpp"

#if defined(__GNUC__) && defined(NDEBUG)
#define always_inline inline  __attribute__((always_inline))
#else
#define always_inline inline
#endif


namespace nova {
namespace detail {

template <typename Functor>
struct unary_functor
{
    template <typename FloatType>
    static always_inline void perform_vec(FloatType * out, const FloatType * arg, unsigned int n)
    {
        nova::detail::apply_on_vector(out, arg, n, Functor());
    }

    template <typename FloatType>
    static always_inline void perform_vec_simd(FloatType * out, const FloatType * arg, unsigned int n)
    {
        nova::detail::generate_simd_loop(out, nova::detail::wrap_vector_arg(wrap_argument(arg)), n, Functor());
    }

    template <unsigned int n, typename FloatType>
    static always_inline void perform_vec_simd(FloatType * out, const FloatType * arg)
    {
        nova::detail::vector_pointer_argument<FloatType> varg(arg);
        nova::detail::compile_time_unroller<FloatType, n>::run(out, varg, Functor());
    }
};

template <typename Functor>
struct binary_functor
{
    template <typename FloatType, typename Arg1Type, typename Arg2Type>
    static always_inline void perform_vec(FloatType * out, Arg1Type arg1, Arg2Type arg2, unsigned int n)
    {
        nova::detail::apply_on_vector(out, wrap_argument(arg1), wrap_argument(arg2), n, Functor());
    }

    template <typename FloatType, typename Arg1Type, typename Arg2Type>
    static always_inline void perform_vec_simd(FloatType * out, Arg1Type arg1, Arg2Type arg2, unsigned int n)
    {
        nova::detail::generate_simd_loop(out,
                                         nova::detail::wrap_vector_arg(wrap_argument(arg1)),
                                         nova::detail::wrap_vector_arg(wrap_argument(arg2)),
                                         n, Functor());
    }

    template <unsigned int n, typename FloatType, typename Arg1Type, typename Arg2Type>
    static always_inline void perform_vec_simd(FloatType * out, Arg1Type arg1, Arg2Type arg2)
    {
        perform_vec_simd_<n, FloatType>(out,
                                        nova::detail::wrap_vector_arg(wrap_argument(arg1)),
                                        nova::detail::wrap_vector_arg(wrap_argument(arg2)));
    }

private:
    template <unsigned int n, typename FloatType, typename Arg1Type, typename Arg2Type>
    static always_inline void perform_vec_simd_(FloatType * out, Arg1Type arg1, Arg2Type arg2)
    {
        nova::detail::compile_time_unroller<FloatType, n>::run(out, arg1, arg2, Functor());
    }
};

template <typename Functor>
struct ternary_functor
{
    template <typename FloatType, typename Arg1Type, typename Arg2Type, typename Arg3Type>
    static always_inline void perform_vec(FloatType * out, Arg1Type arg1, Arg2Type arg2, Arg3Type arg3, unsigned int n)
    {
        nova::detail::apply_on_vector(out, wrap_argument(arg1), wrap_argument(arg2), wrap_argument(arg3), n, Functor());
    }

    template <typename FloatType, typename Arg1Type, typename Arg2Type, typename Arg3Type>
    static always_inline void perform_vec_simd(FloatType * out, Arg1Type arg1, Arg2Type arg2, Arg3Type arg3, unsigned int n)
    {
        nova::detail::generate_simd_loop(out,
                                         nova::detail::wrap_vector_arg(wrap_argument(arg1)),
                                         nova::detail::wrap_vector_arg(wrap_argument(arg2)),
                                         nova::detail::wrap_vector_arg(wrap_argument(arg3)),
                                         n, Functor());
    }

    template <unsigned int n, typename FloatType, typename Arg1Type, typename Arg2Type, typename Arg3Type>
    static always_inline void perform_vec_simd(FloatType * out, Arg1Type arg1, Arg2Type arg2, Arg3Type arg3)
    {
        perform_vec_simd_<n, FloatType>(out,
                                        nova::detail::wrap_vector_arg(wrap_argument(arg1)),
                                        nova::detail::wrap_vector_arg(wrap_argument(arg2)),
                                        nova::detail::wrap_vector_arg(wrap_argument(arg3)));
    }

private:
    template <unsigned int n, typename FloatType, typename Arg1Type, typename Arg2Type, typename Arg3Type>
    static always_inline void perform_vec_simd_(FloatType * out, Arg1Type arg1, Arg2Type arg2, Arg3Type arg3)
    {
        nova::detail::compile_time_unroller<FloatType, n>::run(out, arg1, arg2, arg3, Functor());
    }
};

template <typename Functor>
struct quarternary_functor
{
    template <typename FloatType, typename Arg1Type, typename Arg2Type, typename Arg3Type, typename Arg4Type>
    static always_inline void perform_vec(FloatType * out, Arg1Type arg1, Arg2Type arg2, Arg3Type arg3, Arg4Type arg4, unsigned int n)
    {
        nova::detail::apply_on_vector(out, wrap_argument(arg1), wrap_argument(arg2),
                                      wrap_argument(arg3), wrap_argument(arg4), n, Functor());
    }

    template <typename FloatType, typename Arg1Type, typename Arg2Type, typename Arg3Type, typename Arg4Type>
    static always_inline void perform_vec_simd(FloatType * out, Arg1Type arg1, Arg2Type arg2, Arg3Type arg3, Arg4Type arg4, unsigned int n)
    {
        nova::detail::generate_simd_loop(out,
                                         nova::detail::wrap_vector_arg(wrap_argument(arg1)),
                                         nova::detail::wrap_vector_arg(wrap_argument(arg2)),
                                         nova::detail::wrap_vector_arg(wrap_argument(arg3)),
                                         nova::detail::wrap_vector_arg(wrap_argument(arg4)),
                                         n, Functor());
    }

    template <unsigned int n, typename FloatType, typename Arg1Type, typename Arg2Type, typename Arg3Type, typename Arg4Type>
    static always_inline void perform_vec_simd(FloatType * out, Arg1Type arg1, Arg2Type arg2, Arg3Type arg3, Arg4Type arg4)
    {
        perform_vec_simd_<n, FloatType>(out,
                                        nova::detail::wrap_vector_arg(wrap_argument(arg1)),
                                        nova::detail::wrap_vector_arg(wrap_argument(arg2)),
                                        nova::detail::wrap_vector_arg(wrap_argument(arg3)),
                                        nova::detail::wrap_vector_arg(wrap_argument(arg4)));
    }

private:
    template <unsigned int n, typename FloatType, typename Arg1Type, typename Arg2Type, typename Arg3Type, typename Arg4Type>
    static always_inline void perform_vec_simd_(FloatType * out, Arg1Type arg1, Arg2Type arg2, Arg3Type arg3, Arg4Type arg4)
    {
        nova::detail::compile_time_unroller<FloatType, n>::run(out, arg1, arg2, arg3, arg4, Functor());
    }
};

} // namespace detail
} // namespace nova



#define NOVA_SIMD_DEFINE_UNARY_WRAPPER(NAME, FUNCTOR)                   \
                                                                        \
template <typename FloatType>                                           \
inline void NAME##_vec(FloatType * out, const FloatType * arg, unsigned int n) \
{                                                                       \
    nova::detail::unary_functor<FUNCTOR>::perform_vec<FloatType>(out, arg, n); \
}                                                                       \
                                                                        \
template <typename FloatType>                                           \
inline void NAME##_vec_simd(FloatType * out, const FloatType * arg, unsigned int n) \
{                                                                       \
    nova::detail::unary_functor<FUNCTOR>::perform_vec_simd<FloatType>(out, arg, n); \
}                                                                       \
                                                                        \
template <unsigned int n, typename FloatType>                           \
inline void NAME##_vec_simd(FloatType * out, const FloatType * arg)     \
{                                                                       \
    nova::detail::unary_functor<FUNCTOR>::perform_vec_simd<n, FloatType>(out, arg); \
}

#define NOVA_SIMD_DEFINE_BINARY_WRAPPER(NAME, FUNCTOR)                  \
                                                                        \
template <typename FloatType, typename Arg1Type, typename Arg2Type>     \
inline void NAME##_vec(FloatType * out, Arg1Type arg1, Arg2Type arg2, unsigned int n) \
{                                                                       \
    nova::detail::binary_functor<FUNCTOR>::perform_vec<FloatType>(out, arg1, arg2, n); \
}                                                                       \
                                                                        \
template <typename FloatType, typename Arg1Type, typename Arg2Type>     \
inline void NAME##_vec_simd(FloatType * out, Arg1Type arg1, Arg2Type arg2, unsigned int n) \
{                                                                       \
    nova::detail::binary_functor<FUNCTOR>::perform_vec_simd<FloatType>(out, arg1, arg2, n); \
}                                                                       \
                                                                        \
template <unsigned int n, typename FloatType, typename Arg1Type, typename Arg2Type> \
inline void NAME##_vec_simd(FloatType * out, Arg1Type arg1, Arg2Type arg2) \
{                                                                       \
    nova::detail::binary_functor<FUNCTOR>::perform_vec_simd<n, FloatType>(out, arg1, arg2); \
}



#define NOVA_SIMD_DEFINE_TERNARY_WRAPPER(NAME, FUNCTOR)                 \
template <typename FloatType,                                           \
          typename Arg1,                                                \
          typename Arg2,                                                \
          typename Arg3                                                 \
         >                                                              \
inline void NAME##_vec(FloatType * out, Arg1 arg1, Arg2 arg2, Arg3 arg3, unsigned int n) \
{                                                                       \
    nova::detail::ternary_functor<FUNCTOR>::perform_vec<FloatType>(out, arg1, arg2, arg3, n); \
}                                                                       \
                                                                        \
template <typename FloatType,                                           \
          typename Arg1,                                                \
          typename Arg2,                                                \
          typename Arg3                                                 \
         >                                                              \
inline void NAME##_vec_simd(FloatType * out, Arg1 arg1, Arg2 arg2, Arg3 arg3, unsigned int n) \
{                                                                       \
    nova::detail::ternary_functor<FUNCTOR>::perform_vec_simd<FloatType>(out, arg1, arg2, arg3, n); \
}                                                                       \
                                                                        \
template <int N,                                                        \
          typename FloatType,                                           \
          typename Arg1,                                                \
          typename Arg2,                                                \
          typename Arg3                                                 \
         >                                                              \
inline void NAME##_vec_simd(FloatType * out, Arg1 arg1, Arg2 arg2, Arg3 arg3) \
{                                                                       \
    nova::detail::ternary_functor<FUNCTOR>::perform_vec_simd<N, FloatType>(out, arg1, arg2, arg3); \
}


#define NOVA_SIMD_DEFINE_4ARY_WRAPPER(NAME, FUNCTOR)                    \
template <typename FloatType,                                           \
          typename Arg1,                                                \
          typename Arg2,                                                \
          typename Arg3,                                                \
          typename Arg4                                                 \
         >                                                              \
inline void NAME##_vec(FloatType * out, Arg1 arg1, Arg2 arg2,           \
                       Arg3 arg3, Arg4 arg4, unsigned int n)            \
{                                                                       \
    nova::detail::quarternary_functor<FUNCTOR>::perform_vec<FloatType>(out, arg1, arg2, arg3, arg4, n); \
}                                                                       \
                                                                        \
template <typename FloatType,                                           \
          typename Arg1,                                                \
          typename Arg2,                                                \
          typename Arg3,                                                \
          typename Arg4                                                 \
         >                                                              \
inline void NAME##_vec_simd(FloatType * out, Arg1 arg1, Arg2 arg2, Arg3 arg3, Arg4 arg4, unsigned int n) \
{                                                                       \
    nova::detail::quarternary_functor<FUNCTOR>::perform_vec_simd<FloatType>(out, arg1, arg2, arg3, arg4, n); \
}                                                                       \
                                                                        \
template <int N,                                                        \
          typename FloatType,                                           \
          typename Arg1,                                                \
          typename Arg2,                                                \
          typename Arg3,                                                \
          typename Arg4                                                 \
         >                                                              \
inline void NAME##_vec_simd(FloatType * out, Arg1 arg1, Arg2 arg2, Arg3 arg3, Arg4 arg4) \
{                                                                       \
    nova::detail::quarternary_functor<FUNCTOR>::perform_vec_simd<N, FloatType>(out, arg1, arg2, arg3, arg4); \
}


#undef always_inline

#endif /* NOVA_SIMD_DETAIL_DEFINE_MACROS_HPP */
