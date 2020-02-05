#pragma once

#include <QGuiApplication>
#include <QScreen>

inline QSizeF GetDisplayScale()
{
    QSizeF scaleDpi = QSizeF(96.0f, 96.0f);
    if (const QScreen* pScreen = QGuiApplication::primaryScreen())
    {
        scaleDpi.setWidth(pScreen->logicalDotsPerInchX());
        scaleDpi.setHeight(pScreen->logicalDotsPerInchY());
    }

    return QSizeF(scaleDpi.width() / 96.0f, scaleDpi.height() / 96.0f);
}

inline QSize ScaleForDPI(const QSize& sz)
{
    auto scale = GetDisplayScale();
    return QSize(scale.width() * sz.width(), scale.height() * sz.height());
}

inline QSize ScaleForDPI(int x, int y)
{
    auto scale = GetDisplayScale();
    return QSize(scale.width() * x, scale.height() * y);
}

inline int ScaleHeightForDPI(int y)
{
  if (y <= 0)
  {
    return 0;
  }

  // ensure returned value is at least 1
  auto scale = GetDisplayScale();
  return (scale.height() * y) + 1;
}

inline int ScaleWidthForDPI(int x)
{
  if (x <= 0)
  {
    return 0;
  }

  // ensure returned value is at least 1
  auto scale = GetDisplayScale();
  return (scale.width() * x) + 1;
}
