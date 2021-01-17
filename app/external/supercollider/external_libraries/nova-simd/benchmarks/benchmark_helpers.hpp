//  some benchmarking helpers
//  Copyright (C) 2008 Tim Blechmann
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

#include <iostream>
#include <stdint.h>

#include <boost/bind.hpp>

#include "cache_aligned_array.hpp"

template <typename Container>
void fill_container(Container & t)
{
    for (int i = 0; i != t.size(); ++i)
        t[i] = rand()/RAND_MAX * 2 - 1;
}


#ifdef NO_PERFORMANCE_COUNTERS

#include "../source/utilities/high_resolution_timer.hpp"

template <typename Function>
inline void run_bench(Function const & f, unsigned int loops = 5000000)
{
    boost::high_resolution_timer hrt;
    hrt.restart();

    for (int i = 0; i != loops; ++i)
        f();

    double e = hrt.elapsed();
    std::cout << e << std::endl;
}

#else

#include "perf_counter.hpp"

template <typename Function>
inline void run_bench(Function const & f, unsigned int loops = 5000000)
{
    perf_counter pc;
    pc.start();

    for (int i = 0; i != loops; ++i)
        f();

    pc.stop();
    pc.dump(false);
}

#endif

#define __noinline__ __attribute__ ((noinline))
