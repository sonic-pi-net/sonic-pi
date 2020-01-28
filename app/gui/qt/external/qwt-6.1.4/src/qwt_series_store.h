/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#ifndef QWT_SERIES_STORE_H
#define QWT_SERIES_STORE_H

#include "qwt_global.h"
#include "qwt_series_data.h"

/*!
  \brief Bridge between QwtSeriesStore and QwtPlotSeriesItem

  QwtAbstractSeriesStore is an abstract interface only
  to make it possible to isolate the template based methods ( QwtSeriesStore )
  from the regular methods ( QwtPlotSeriesItem ) to make it possible
  to derive from QwtPlotSeriesItem without any hassle with templates.
*/
class QwtAbstractSeriesStore
{
protected:
    //! Destructor
    virtual ~QwtAbstractSeriesStore() {}

#ifndef QWT_PYTHON_WRAPPER
    //! dataChanged() indicates, that the series has been changed.
    virtual void dataChanged() = 0;

    /*!
      Set a the "rectangle of interest" for the stored series
      \sa QwtSeriesData<T>::setRectOfInterest()
     */
    virtual void setRectOfInterest( const QRectF & ) = 0;

    //! \return Bounding rectangle of the stored series
    virtual QRectF dataRect() const = 0;

    //! \return Number of samples
    virtual size_t dataSize() const = 0;
#else
    // Needed for generating the python bindings, but not for using them !
    virtual void dataChanged() {}
    virtual void setRectOfInterest( const QRectF & ) {}
    virtual QRectF dataRect() const { return  QRectF( 0.0, 0.0, -1.0, -1.0 ); }
    virtual size_t dataSize() const { return 0; }
#endif
};

/*!
  \brief Class storing a QwtSeriesData object

  QwtSeriesStore and QwtPlotSeriesItem are intended as base classes for all
  plot items iterating over a series of samples. Both classes share
  a virtual base class ( QwtAbstractSeriesStore ) to bridge between them.

  QwtSeriesStore offers the template based part for the plot item API, so
  that QwtPlotSeriesItem can be derived without any hassle with templates.
 */
template <typename T>
class QwtSeriesStore: public virtual QwtAbstractSeriesStore
{
public:
    /*!
      \brief Constructor
      The store contains no series
    */
    explicit QwtSeriesStore<T>();

    //! Destructor
    ~QwtSeriesStore<T>();

    /*!
      Assign a series of samples

      \param series Data
      \warning The item takes ownership of the data object, deleting
               it when its not used anymore.
    */
    void setData( QwtSeriesData<T> *series );

    //! \return the the series data
    QwtSeriesData<T> *data();

    //! \return the the series data
    const QwtSeriesData<T> *data() const;

    /*!
        \param index Index
        \return Sample at position index
    */
    T sample( int index ) const;

    /*!
      \return Number of samples of the series
      \sa setData(), QwtSeriesData<T>::size()
    */
    virtual size_t dataSize() const;

    /*!
      \return Bounding rectangle of the series
              or an invalid rectangle, when no series is stored

      \sa QwtSeriesData<T>::boundingRect()
    */
    virtual QRectF dataRect() const;

    /*!
      Set a the "rect of interest" for the series

      \param rect Rectangle of interest
      \sa QwtSeriesData<T>::setRectOfInterest()
    */
    virtual void setRectOfInterest( const QRectF &rect );

    /*!
      Replace a series without deleting the previous one

      \param series New series
      \return Previously assigned series
     */
    QwtSeriesData<T> *swapData( QwtSeriesData<T> *series );

private:
    QwtSeriesData<T> *d_series;
};

template <typename T>
QwtSeriesStore<T>::QwtSeriesStore():
    d_series( NULL )
{
}

template <typename T>
QwtSeriesStore<T>::~QwtSeriesStore()
{
    delete d_series;
}

template <typename T>
inline QwtSeriesData<T> *QwtSeriesStore<T>::data()
{
    return d_series;
}

template <typename T>
inline const QwtSeriesData<T> *QwtSeriesStore<T>::data() const
{
    return d_series;
}

template <typename T>
inline T QwtSeriesStore<T>::sample( int index ) const
{
    return d_series ? d_series->sample( index ) : T();
}

template <typename T>
void QwtSeriesStore<T>::setData( QwtSeriesData<T> *series )
{
    if ( d_series != series )
    {
        delete d_series;
        d_series = series;
        dataChanged();
    }
}

template <typename T>
size_t QwtSeriesStore<T>::dataSize() const
{
    if ( d_series == NULL )
        return 0;

    return d_series->size();
}

template <typename T>
QRectF QwtSeriesStore<T>::dataRect() const
{
    if ( d_series == NULL )
        return QRectF( 1.0, 1.0, -2.0, -2.0 ); // invalid

    return d_series->boundingRect();
}

template <typename T>
void QwtSeriesStore<T>::setRectOfInterest( const QRectF &rect )
{
    if ( d_series )
        d_series->setRectOfInterest( rect );
}

template <typename T>
QwtSeriesData<T>* QwtSeriesStore<T>::swapData( QwtSeriesData<T> *series )
{
    QwtSeriesData<T> * swappedSeries = d_series;
    d_series = series;

    return swappedSeries;
}

#endif
