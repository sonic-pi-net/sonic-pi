/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_plot_dict.h"

class QwtPlotDict::PrivateData
{
public:

    class ItemList: public QList<QwtPlotItem *>
    {
    public:
        void insertItem( QwtPlotItem *item )
        {
            if ( item == NULL )
                return;

            QList<QwtPlotItem *>::iterator it =
                qUpperBound( begin(), end(), item, LessZThan() );
            insert( it, item );
        }

        void removeItem( QwtPlotItem *item )
        {
            if ( item == NULL )
                return;

            QList<QwtPlotItem *>::iterator it =
                qLowerBound( begin(), end(), item, LessZThan() );

            for ( ; it != end(); ++it )
            {
                if ( item == *it )
                {
                    erase( it );
                    break;
                }
            }
        }
    private:
        class LessZThan
        {
        public:
            inline bool operator()( const QwtPlotItem *item1,
                const QwtPlotItem *item2 ) const
            {
                return item1->z() < item2->z();
            }
        };
    };

    ItemList itemList;
    bool autoDelete;
};

/*!
   Constructor

   Auto deletion is enabled.
   \sa setAutoDelete(), QwtPlotItem::attach()
*/
QwtPlotDict::QwtPlotDict()
{
    d_data = new QwtPlotDict::PrivateData;
    d_data->autoDelete = true;
}

/*!
   Destructor

   If autoDelete() is on, all attached items will be deleted
   \sa setAutoDelete(), autoDelete(), QwtPlotItem::attach()
*/
QwtPlotDict::~QwtPlotDict()
{
    detachItems( QwtPlotItem::Rtti_PlotItem, d_data->autoDelete );
    delete d_data;
}

/*!
   En/Disable Auto deletion

   If Auto deletion is on all attached plot items will be deleted
   in the destructor of QwtPlotDict. The default value is on.

   \sa autoDelete(), insertItem()
*/
void QwtPlotDict::setAutoDelete( bool autoDelete )
{
    d_data->autoDelete = autoDelete;
}

/*!
   \return true if auto deletion is enabled
   \sa setAutoDelete(), insertItem()
*/
bool QwtPlotDict::autoDelete() const
{
    return d_data->autoDelete;
}

/*!
  Insert a plot item

  \param item PlotItem
  \sa removeItem()
 */
void QwtPlotDict::insertItem( QwtPlotItem *item )
{
    d_data->itemList.insertItem( item );
}

/*!
  Remove a plot item

  \param item PlotItem
  \sa insertItem()
 */
void QwtPlotDict::removeItem( QwtPlotItem *item )
{
    d_data->itemList.removeItem( item );
}

/*!
   Detach items from the dictionary

   \param rtti In case of QwtPlotItem::Rtti_PlotItem detach all items
               otherwise only those items of the type rtti.
   \param autoDelete If true, delete all detached items
*/
void QwtPlotDict::detachItems( int rtti, bool autoDelete )
{
    PrivateData::ItemList list = d_data->itemList;
    QwtPlotItemIterator it = list.constBegin();
    while ( it != list.constEnd() )
    {
        QwtPlotItem *item = *it;

        ++it; // increment before removing item from the list

        if ( rtti == QwtPlotItem::Rtti_PlotItem || item->rtti() == rtti )
        {
            item->attach( NULL );
            if ( autoDelete )
                delete item;
        }
    }
}

/*!
  \brief A QwtPlotItemList of all attached plot items.

  Use caution when iterating these lists, as removing/detaching an item will
  invalidate the iterator. Instead you can place pointers to objects to be
  removed in a removal list, and traverse that list later.

  \return List of all attached plot items.
*/
const QwtPlotItemList &QwtPlotDict::itemList() const
{
    return d_data->itemList;
}

/*!
  \return List of all attached plot items of a specific type.
  \param rtti See QwtPlotItem::RttiValues
  \sa QwtPlotItem::rtti()
*/
QwtPlotItemList QwtPlotDict::itemList( int rtti ) const
{
    if ( rtti == QwtPlotItem::Rtti_PlotItem )
        return d_data->itemList;

    QwtPlotItemList items;

    PrivateData::ItemList list = d_data->itemList;
    for ( QwtPlotItemIterator it = list.constBegin(); it != list.constEnd(); ++it )
    {
        QwtPlotItem *item = *it;
        if ( item->rtti() == rtti )
            items += item;
    }

    return items;
}
