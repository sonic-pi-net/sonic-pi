/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#ifndef _QWT_COMPAT_H_
#define _QWT_COMPAT_H_

#include "qwt_global.h"
#include "qwt_interval.h"
#include "qwt_point_3d.h"
#include <qlist.h>
#include <qvector.h>
#include <qpoint.h>
#include <qsize.h>
#include <qrect.h>
#include <qpolygon.h>

// A couple of definition for Qwt5 compatibility

#define qwtMax qMax
#define qwtMin qMin
#define qwtAbs qAbs
#define qwtRound qRound

#define QwtArray QVector

typedef QList<double> QwtValueList;
typedef QPointF QwtDoublePoint;
typedef QSizeF QwtDoubleSize;
typedef QRectF QwtDoubleRect;

typedef QPolygon QwtPolygon;
typedef QPolygonF QwtPolygonF;
typedef QwtInterval QwtDoubleInterval;
typedef QwtPoint3D QwtDoublePoint3D;

#endif
