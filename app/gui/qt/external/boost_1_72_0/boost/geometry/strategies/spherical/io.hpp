// Boost.Geometry

// Copyright (c) 2019, Oracle and/or its affiliates.

// Contributed and/or modified by Adam Wulkiewicz, on behalf of Oracle

// Licensed under the Boost Software License version 1.0.
// http://www.boost.org/users/license.html

#ifndef BOOST_GEOMETRY_EXTENSIONS_STRATEGIES_SPHERICAL_IO_HPP
#define BOOST_GEOMETRY_EXTENSIONS_STRATEGIES_SPHERICAL_IO_HPP


#include <boost/geometry/strategies/io.hpp>

#include <boost/geometry/strategies/spherical/point_order.hpp>
#include <boost/geometry/strategies/spherical/point_in_poly_winding.hpp>


namespace boost { namespace geometry
{

namespace strategy { namespace io
{

template <typename CalculationType = void>
struct spherical
{
    typedef strategy::point_order::spherical<CalculationType> point_order_strategy_type;
    
    static inline point_order_strategy_type get_point_order_strategy()
    {
        return point_order_strategy_type();
    }

    template <typename Geometry1, typename Geometry2>
    struct point_in_geometry_strategy
    {
        typedef strategy::within::spherical_winding
            <
                typename point_type<Geometry1>::type,
                typename point_type<Geometry2>::type,
                CalculationType
            > type;
    };

    template <typename Geometry1, typename Geometry2>
    static inline typename point_in_geometry_strategy<Geometry1, Geometry2>::type
        get_point_in_geometry_strategy()
    {
        typedef typename point_in_geometry_strategy
            <
                Geometry1, Geometry2
            >::type strategy_type;
        return strategy_type();
    }
};

namespace services
{

template <>
struct default_strategy<spherical_equatorial_tag>
{
    typedef spherical<> type;
};

template <>
struct default_strategy<spherical_polar_tag>
{
    typedef spherical<> type;
};

} // namespace services

}} // namespace strategy::io

}} // namespace boost::geometry

#endif // BOOST_GEOMETRY_EXTENSIONS_STRATEGIES_SPHERICAL_IO_HPP
