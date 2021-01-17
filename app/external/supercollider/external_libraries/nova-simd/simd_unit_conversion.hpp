//  simdfied unit conversion functions
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

#ifndef SIMD_UNIT_CONVERSION_HPP
#define SIMD_UNIT_CONVERSION_HPP

#include "simd_unary_arithmetic.hpp"
#include "simd_math.hpp"

#if defined(__GNUC__) && defined(NDEBUG)
#define always_inline inline  __attribute__((always_inline))
#else
#define always_inline inline
#endif

namespace nova {
namespace detail {

struct abs_log2
{
    template <typename FloatType>
    always_inline FloatType operator()(const FloatType & arg) const
    {
        return log2_()(fabs_()(arg));
    }
};

struct midi2freq
{
    template <typename FloatType>
    always_inline FloatType operator()(const FloatType & midi) const
    {
        return (FloatType)440. * pow_()(FloatType(2.),
                                                     (midi - FloatType(69.)) * FloatType(0.083333333333));
    }
};

struct freq2midi
{
    template <typename FloatType>
    always_inline FloatType operator()(const FloatType & freq) const
    {
        return abs_log2()(freq * FloatType(0.0022727272727))  * FloatType(12.) + FloatType(69.);
    }
};

struct midi2ratio
{
    template <typename FloatType>
    always_inline FloatType operator()(const FloatType & midi) const
    {
        return pow_()(FloatType(2.), midi * FloatType(0.083333333333));
    }
};

struct ratio2midi
{
    template <typename FloatType>
    always_inline FloatType operator()(const FloatType & ratio) const
    {
        return FloatType(12.) * log2_()(ratio);
    }
};

struct oct2freq
{
    template <typename FloatType>
    always_inline FloatType operator()(const FloatType & note) const
    {
        return FloatType(440.) * pow_()(FloatType(2.), note - FloatType(4.75));
    }
};


struct freq2oct
{
    template <typename FloatType>
    always_inline FloatType operator()(const FloatType & freq) const
    {
        return abs_log2()(freq * FloatType(0.0022727272727)) + FloatType(4.75);
    }
};

struct amp2db
{
    template <typename FloatType>
    always_inline FloatType operator()(const FloatType & amp) const
    {
        return log10_()(fabs_()(amp)) * FloatType(20.);
    }
};

struct db2amp
{
    template <typename FloatType>
    always_inline FloatType operator()(const FloatType & db) const
    {
        return pow_()(FloatType(10.), db * FloatType(0.05));
    }
};

}


NOVA_SIMD_DEFINE_UNARY_WRAPPER(midi2freq, detail::midi2freq)
NOVA_SIMD_DEFINE_UNARY_WRAPPER(freq2midi, detail::freq2midi)

NOVA_SIMD_DEFINE_UNARY_WRAPPER(midi2ratio, detail::midi2ratio)
NOVA_SIMD_DEFINE_UNARY_WRAPPER(ratio2midi, detail::ratio2midi)

NOVA_SIMD_DEFINE_UNARY_WRAPPER(oct2freq, detail::oct2freq)
NOVA_SIMD_DEFINE_UNARY_WRAPPER(freq2oct, detail::freq2oct)

NOVA_SIMD_DEFINE_UNARY_WRAPPER(amp2db, detail::amp2db)
NOVA_SIMD_DEFINE_UNARY_WRAPPER(db2amp, detail::db2amp)

}

#undef always_inline

#endif /* SIMD_UNIT_CONVERSION_HPP */
