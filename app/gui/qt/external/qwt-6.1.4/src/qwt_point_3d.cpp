/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_point_3d.h"

#ifndef QT_NO_DEBUG_STREAM

QDebug operator<<( QDebug debug, const QwtPoint3D &point )
{
    debug.nospace() << "QwtPoint3D(" << point.x()
        << "," << point.y() << "," << point.z() << ")";
    return debug.space();
}

#endif

