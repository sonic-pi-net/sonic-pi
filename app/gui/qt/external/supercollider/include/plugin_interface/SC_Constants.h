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

#ifndef __FP__
const double pi = std::acos(-1.);
#else
const double sc_pi = std::acos(-1.);
#    define pi sc_pi // hack to avoid osx warning about deprecated pi
#endif
const double pi2 = pi * .5;
const double pi32 = pi * 1.5;
const double twopi = pi * 2.;
const double rtwopi = 1. / twopi;
const double log001 = std::log(0.001);
const double log01 = std::log(0.01);
const double log1 = std::log(0.1);
const double rlog2 = 1. / std::log(2.);
const double sqrt2 = std::sqrt(2.);
const double rsqrt2 = 1. / sqrt2;

const float pi_f = std::acos(-1.f);
const float pi2_f = pi_f * 0.5f;
const float pi32_f = pi_f * 1.5f;
const float twopi_f = pi_f * 2.f;
const float sqrt2_f = std::sqrt(2.f);
const float rsqrt2_f = 1.f / std::sqrt(2.f);

// used to truncate precision
const float truncFloat = (float)(3. * std::pow(2.0, 22));
const double truncDouble = 3. * std::pow(2.0, 51);

const float kBadValue = 1e20f; // used in the secant table for values very close to 1/0
