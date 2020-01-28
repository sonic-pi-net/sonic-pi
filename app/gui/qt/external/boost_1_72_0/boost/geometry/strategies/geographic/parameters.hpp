// Boost.Geometry

// Copyright (c) 2017-2019, Oracle and/or its affiliates.
// Contributed and/or modified by Adam Wulkiewicz, on behalf of Oracle

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_GEOMETRY_STRATEGIES_GEOGRAPHIC_PARAMETERS_HPP
#define BOOST_GEOMETRY_STRATEGIES_GEOGRAPHIC_PARAMETERS_HPP

#include <boost/geometry/formulas/andoyer_inverse.hpp>
#include <boost/geometry/formulas/thomas_direct.hpp>
#include <boost/geometry/formulas/thomas_inverse.hpp>
#include <boost/geometry/formulas/vincenty_direct.hpp>
#include <boost/geometry/formulas/vincenty_inverse.hpp>
//#include <boost/geometry/formulas/karney_direct.hpp>
//#include <boost/geometry/formulas/karney_inverse.hpp>

#include <boost/mpl/assert.hpp>
#include <boost/mpl/integral_c.hpp>


namespace boost { namespace geometry { namespace strategy
{

struct andoyer
{
    template
    <
        typename CT,
        bool EnableCoordinates = true,
        bool EnableReverseAzimuth = false,
        bool EnableReducedLength = false,
        bool EnableGeodesicScale = false
    >
    struct direct
            : formula::thomas_direct
              <
                  CT, false,
                  EnableCoordinates, EnableReverseAzimuth,
                  EnableReducedLength, EnableGeodesicScale
              >
    {};

    template
    <
        typename CT,
        bool EnableDistance,
        bool EnableAzimuth,
        bool EnableReverseAzimuth = false,
        bool EnableReducedLength = false,
        bool EnableGeodesicScale = false
    >
    struct inverse
        : formula::andoyer_inverse
            <
                CT, EnableDistance,
                EnableAzimuth, EnableReverseAzimuth,
                EnableReducedLength, EnableGeodesicScale
            >
    {};
};

struct thomas
{
    template
    <
        typename CT,
        bool EnableCoordinates = true,
        bool EnableReverseAzimuth = false,
        bool EnableReducedLength = false,
        bool EnableGeodesicScale = false
    >
    struct direct
            : formula::thomas_direct
              <
                  CT, true,
                  EnableCoordinates, EnableReverseAzimuth,
                  EnableReducedLength, EnableGeodesicScale
              >
    {};

    template
    <
        typename CT,
        bool EnableDistance,
        bool EnableAzimuth,
        bool EnableReverseAzimuth = false,
        bool EnableReducedLength = false,
        bool EnableGeodesicScale = false
    >
    struct inverse
        : formula::thomas_inverse
            <
                CT, EnableDistance,
                EnableAzimuth, EnableReverseAzimuth,
                EnableReducedLength, EnableGeodesicScale
            >
    {};
};

struct vincenty
{
    template
    <
        typename CT,
        bool EnableCoordinates = true,
        bool EnableReverseAzimuth = false,
        bool EnableReducedLength = false,
        bool EnableGeodesicScale = false
    >
    struct direct
            : formula::vincenty_direct
              <
                  CT, EnableCoordinates, EnableReverseAzimuth,
                  EnableReducedLength, EnableGeodesicScale
              >
    {};

    template
    <
        typename CT,
        bool EnableDistance,
        bool EnableAzimuth,
        bool EnableReverseAzimuth = false,
        bool EnableReducedLength = false,
        bool EnableGeodesicScale = false
    >
    struct inverse
        : formula::vincenty_inverse
            <
                CT, EnableDistance,
                EnableAzimuth, EnableReverseAzimuth,
                EnableReducedLength, EnableGeodesicScale
            >
    {};
};
/*
struct karney
{
    template
    <
        typename CT,
        bool EnableCoordinates = true,
        bool EnableReverseAzimuth = false,
        bool EnableReducedLength = false,
        bool EnableGeodesicScale = false,
        size_t SeriesOrder = 8
    >
    struct direct
            : formula::karney_direct
              <
                  CT, EnableCoordinates, EnableReverseAzimuth,
                  EnableReducedLength, EnableGeodesicScale,
                  SeriesOrder
              >
    {};

    template
    <
        typename CT,
        bool EnableDistance,
        bool EnableAzimuth,
        bool EnableReverseAzimuth = false,
        bool EnableReducedLength = false,
        bool EnableGeodesicScale = false,
        size_t SeriesOrder = 8
    >
    struct inverse
        : formula::karney_inverse
            <
                CT, EnableDistance,
                EnableAzimuth, EnableReverseAzimuth,
                EnableReducedLength, EnableGeodesicScale,
                SeriesOrder
            >
    {};
};
*/
template <typename FormulaPolicy>
struct default_order
{
    BOOST_MPL_ASSERT_MSG
    (
        false, NOT_IMPLEMENTED_FOR_THIS_TYPE
        , (types<FormulaPolicy>)
    );
};

template<>
struct default_order<andoyer>
    : boost::mpl::integral_c<unsigned int, 1>
{};

template<>
struct default_order<thomas>
    : boost::mpl::integral_c<unsigned int, 2>
{};

template<>
struct default_order<vincenty>
    : boost::mpl::integral_c<unsigned int, 4>
{};
/*
template<>
struct default_order<karney>
    : boost::mpl::integral_c<unsigned int, 8>
{};
*/

}}} // namespace boost::geometry::strategy


#endif // BOOST_GEOMETRY_STRATEGIES_GEOGRAPHIC_PARAMETERS_HPP
