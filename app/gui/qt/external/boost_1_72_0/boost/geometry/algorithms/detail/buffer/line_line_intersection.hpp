// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2012-2019 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_GEOMETRY_ALGORITHMS_DETAIL_BUFFER_LINE_LINE_INTERSECTION_HPP
#define BOOST_GEOMETRY_ALGORITHMS_DETAIL_BUFFER_LINE_LINE_INTERSECTION_HPP


#include <boost/geometry/arithmetic/infinite_line_functions.hpp>
#include <boost/geometry/algorithms/detail/make/make.hpp>
#include <boost/geometry/strategies/buffer.hpp>

namespace boost { namespace geometry
{


#ifndef DOXYGEN_NO_DETAIL
namespace detail { namespace buffer
{

// TODO: it might once be changed this to proper strategy
struct line_line_intersection
{
    template <typename Point>
    static inline strategy::buffer::join_selector
    apply(Point const& pi, Point const& pj,
          Point const& qi, Point const& qj,
          Point& ip)
    {
        typedef typename coordinate_type<Point>::type ct;
        typedef model::infinite_line<ct> line_type;

        line_type const p = detail::make::make_infinite_line<ct>(pi, pj);
        line_type const q = detail::make::make_infinite_line<ct>(qi, qj);

        if (arithmetic::intersection_point(p, q, ip))
        {
            return strategy::buffer::join_convex;
        }

        // The lines do not intersect.
        // Distinguish between continuing lines (having a similar direction)
        // and spikes (having the opposite direction).
        return arithmetic::similar_direction(p, q)
            ? strategy::buffer::join_continue
            : strategy::buffer::join_spike
            ;
    }
};


}} // namespace detail::buffer
#endif // DOXYGEN_NO_DETAIL


}} // namespace boost::geometry


#endif // BOOST_GEOMETRY_ALGORITHMS_DETAIL_BUFFER_LINE_LINE_INTERSECTION_HPP
