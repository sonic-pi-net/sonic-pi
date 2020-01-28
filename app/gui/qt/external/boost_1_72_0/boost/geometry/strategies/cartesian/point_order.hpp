// Boost.Geometry

// Copyright (c) 2019, Oracle and/or its affiliates.

// Contributed and/or modified by Adam Wulkiewicz, on behalf of Oracle

// Licensed under the Boost Software License version 1.0.
// http://www.boost.org/users/license.html

#ifndef BOOST_GEOMETRY_STRATEGIES_CARTESIAN_POINT_ORDER_HPP
#define BOOST_GEOMETRY_STRATEGIES_CARTESIAN_POINT_ORDER_HPP


#include <boost/geometry/core/tags.hpp>

#include <boost/geometry/strategies/point_order.hpp>
#include <boost/geometry/strategies/cartesian/area.hpp>


namespace boost { namespace geometry
{

namespace strategy { namespace point_order
{

template <typename CalculationType = void>
struct cartesian
    : strategy::area::cartesian<CalculationType>
{
    typedef area_tag version_tag;
};

namespace services
{

template <>
struct default_strategy<cartesian_tag>
{
    typedef cartesian<> type;
};

} // namespace services

}} // namespace strategy::point_order

}} // namespace boost::geometry

#endif // BOOST_GEOMETRY_STRATEGIES_CARTESIAN_POINT_ORDER_HPP
