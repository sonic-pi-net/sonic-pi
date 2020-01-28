/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_plot.h"

/*!
  This method is intended for manipulating the plot widget
  from a specific editor in the Qwt designer plugin.

  \warning The plot editor has never been implemented.
*/
void QwtPlot::applyProperties( const QString & /* xmlDocument */ )
{
#if 0
    // Temporary dummy code, for designer tests
    setTitle( xmlDocument );
    replot();
#endif
}

/*!
  This method is intended for manipulating the plot widget
  from a specific editor in the Qwt designer plugin.

  \return QString()
  \warning The plot editor has never been implemented.
*/
QString QwtPlot::grabProperties() const
{
#if 0
    // Temporary dummy code, for designer tests
    return title().text();
#else
    return QString();
#endif
}
