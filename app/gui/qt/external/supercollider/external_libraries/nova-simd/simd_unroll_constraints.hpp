//  unrolling constratins for simd functions
//  Copyright (C) 2009 Tim Blechmann
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


#ifndef SIMD_UNROLL_CONSTRAINTS_HPP
#define SIMD_UNROLL_CONSTRAINTS_HPP

#include "vec.hpp"

namespace nova
{

template <typename float_type>
struct unroll_constraints
{
    static const unsigned int samples_per_loop = vec<float_type>::objects_per_cacheline;
};

} /* namespace nova */

#endif /* SIMD_UNROLL_CONSTRAINTS_HPP */
