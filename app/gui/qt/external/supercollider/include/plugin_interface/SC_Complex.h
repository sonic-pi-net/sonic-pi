/*
    SuperCollider real time audio synthesis system
    Copyright (c) 2002 James McCartney. All rights reserved.
    http://www.audiosynth.com

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
*/


#pragma once

#include <cmath>

#include "SC_Types.h"
#include "SC_Constants.h"
#include "float.h"

#ifdef _MSC_VER
// hypotf is c99, but not c++
#    define hypotf _hypotf
#endif

////////////////////////////////////////////////////////////////////////////////

namespace detail {

const int kSineSize = 8192;
const int kSineMask = kSineSize - 1;
const double kSinePhaseScale = kSineSize / twopi;
const int32 kPolarLUTSize = 2049;
const int32 kPolarLUTSize2 = kPolarLUTSize >> 1;


/* each object file that is including this header will have separate lookup tables */
namespace {

float gMagLUT[kPolarLUTSize];
float gPhaseLUT[kPolarLUTSize];
float gSine[kSineSize + 1];

static bool initTables(void) {
    double sineIndexToPhase = twopi / kSineSize;
    for (int i = 0; i <= kSineSize; ++i) {
        double phase = i * sineIndexToPhase;
        float32 d = sin(phase);
        gSine[i] = d;
    }

    double rPolarLUTSize2 = 1. / kPolarLUTSize2;
    for (int i = 0; i < kPolarLUTSize; ++i) {
        double slope = (i - kPolarLUTSize2) * rPolarLUTSize2;
        double angle = atan(slope);
        gPhaseLUT[i] = (float)angle;
        gMagLUT[i] = (float)(1.f / cos(angle));
    }

    return true;
}

bool dummy = initTables();

}

struct Polar;


struct Complex {
    Complex() {}
    Complex(float r, float i): real(r), imag(i) {}
    void Set(float r, float i) {
        real = r;
        imag = i;
    }

    Complex& operator=(Complex b) {
        real = b.real;
        imag = b.imag;
        return *this;
    }
    Complex& operator=(float b) {
        real = b;
        imag = 0.;
        return *this;
    }

    Polar ToPolar();

    /**
     * Converts cartesian to polar representation, using lookup tables.
     * Note: in this implementation the phase values returned lie in the range [-pi/4, 7pi/4]
     * rather than the more conventional [0, 2pi] or [-pi, pi].
     */
    Polar ToPolarApx();

    void ToPolarInPlace();

    void ToPolarApxInPlace();

    float real, imag;
};

struct Polar {
    Polar() {}
    Polar(float m, float p): mag(m), phase(p) {}
    void Set(float m, float p) {
        mag = m;
        phase = p;
    }

    Complex ToComplex() { return Complex(mag * std::cos(phase), mag * std::sin(phase)); }

    Complex ToComplexApx() {
        uint32 sinindex = (int32)(kSinePhaseScale * phase) & kSineMask;
        uint32 cosindex = (sinindex + (kSineSize >> 2)) & kSineMask;
        return Complex(mag * gSine[cosindex], mag * gSine[sinindex]);
    }

    void ToComplexInPlace() {
        Complex complx = ToComplex();
        mag = complx.real;
        phase = complx.imag;
    }

    void ToComplexApxInPlace() {
        Complex complx = ToComplexApx();
        mag = complx.real;
        phase = complx.imag;
    }

    float mag, phase;
};

inline Polar Complex::ToPolar() { return Polar(hypotf(imag, real), std::atan2(imag, real)); }

inline Polar Complex::ToPolarApx() {
    int32 index;
    float absreal = fabs(real);
    float absimag = fabs(imag);
    float mag, phase, slope;
    if (absreal > absimag) {
        slope = imag / real;
        index = (int32)(kPolarLUTSize2 + kPolarLUTSize2 * slope);
        mag = gMagLUT[index] * absreal;
        phase = gPhaseLUT[index];
        if (real > 0) {
            return Polar(mag, phase);
        } else {
            return Polar(mag, (float)(pi + phase));
        }
    } else if (absimag > 0) {
        slope = real / imag;
        index = (int32)(kPolarLUTSize2 + kPolarLUTSize2 * slope);
        mag = gMagLUT[index] * absimag;
        phase = gPhaseLUT[index];
        if (imag > 0) {
            return Polar(mag, (float)(pi2 - phase));
        } else {
            return Polar(mag, (float)(pi32 - phase));
        }
    } else
        return Polar(0, 0);
}

inline void Complex::ToPolarInPlace() {
    Polar polar = ToPolar();
    real = polar.mag;
    imag = polar.phase;
}

inline void Complex::ToPolarApxInPlace() {
    Polar polar = ToPolarApx();
    real = polar.mag;
    imag = polar.phase;
}

}

using detail::Complex;
using detail::Polar;

struct ComplexFT {
    float dc, nyq;
    Complex complex[1];
};

struct PolarFT {
    float dc, nyq;
    Polar polar[1];
};

void ToComplex(Polar in, Complex& out);

inline Complex operator+(Complex a, Complex b) { return Complex(a.real + b.real, a.imag + b.imag); }
inline Complex operator+(Complex a, float b) { return Complex(a.real + b, a.imag); }
inline Complex operator+(float a, Complex b) { return Complex(a + b.real, b.imag); }

inline Complex& operator+=(Complex& a, const Complex& b) {
    a.real += b.real, a.imag += b.imag;
    return a;
}
inline Complex& operator+=(Complex& a, float b) {
    a.real += b;
    return a;
}

inline Complex operator-(Complex a, Complex b) { return Complex(a.real - b.real, a.imag - b.imag); }
inline Complex operator-(Complex a, float b) { return Complex(a.real - b, a.imag); }
inline Complex operator-(float a, Complex b) { return Complex(a - b.real, b.imag); }

inline Complex operator-=(Complex a, Complex b) {
    a.real -= b.real, a.imag -= b.imag;
    return a;
}
inline Complex operator-=(Complex a, float b) {
    a.real -= b;
    return a;
}

inline Complex operator*(Complex a, Complex b) {
    return Complex(a.real * b.real - a.imag * b.imag, a.real * b.imag + a.imag * b.real);
}

inline Complex operator*(Complex a, float b) { return Complex(a.real * b, a.imag * b); }

inline Complex operator*(float a, Complex b) { return Complex(b.real * a, b.imag * a); }

inline Complex operator*=(Complex a, Complex b) {
    a.Set(a.real * b.real - a.imag * b.imag, a.real * b.imag + a.imag * b.real);
    return a;
}

inline Complex operator*=(Complex a, float b) {
    a.real *= b;
    a.imag *= b;
    return a;
}


inline Polar operator*(Polar a, float b) { return Polar(a.mag * b, a.phase); }

inline Polar operator*(float a, Polar b) { return Polar(a * b.mag, b.phase); }

inline Polar operator*=(Polar a, float b) {
    a.mag *= b;
    return a;
}
