/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#ifndef _QWT_SAMPLING_THREAD_H_
#define _QWT_SAMPLING_THREAD_H_

#include "qwt_global.h"
#include <qthread.h>

/*!
  \brief A thread collecting samples at regular intervals.

  Continuous signals are converted into a discrete signal by
  collecting samples at regular intervals. A discrete signal
  can be displayed by a QwtPlotSeriesItem on a QwtPlot widget.

  QwtSamplingThread starts a thread calling periodically sample(),
  to collect and store ( or emit ) a single sample.

  \sa QwtPlotCurve, QwtPlotSeriesItem
*/
class QWT_EXPORT QwtSamplingThread: public QThread
{
    Q_OBJECT

public:
    virtual ~QwtSamplingThread();

    double interval() const;
    double elapsed() const;

public Q_SLOTS:
    void setInterval( double interval );
    void stop();

protected:
    explicit QwtSamplingThread( QObject *parent = NULL );

    virtual void run();

    /*!
       Collect a sample

       \param elapsed Time since the thread was started in milliseconds
     */
    virtual void sample( double elapsed ) = 0;

private:
    class PrivateData;
    PrivateData *d_data;
};

#endif
