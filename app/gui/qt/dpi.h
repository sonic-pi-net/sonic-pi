#pragma once

#include <QGuiApplication>
#include <QScreen>

inline QSizeF GetDisplayScale()
{
//super hacky temporary fudge to paper over the
//massive cracks that is the difference between how
//macOS and other platforms handle high DPI monitors
#ifdef __APPLE__
    float scale = 96.0;
#else
    float scale = 96.0 * 1.6;
#endif

  QSizeF scaleDpi = QSizeF(scale, scale);
    if (const QScreen* pScreen = QGuiApplication::primaryScreen())
    {
        scaleDpi.setWidth(pScreen->logicalDotsPerInchX());
        scaleDpi.setHeight(pScreen->logicalDotsPerInchY());
    }

    return QSizeF(scaleDpi.width() / scale, scaleDpi.height() / scale);
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

inline QString ScalePxInStyleSheet(QString style)
{

  // TODO: Need to figure out a nicer way to do all this in one pass.
  // My c++ fu doesn't currently rise to the challenge
  /* style = style.replace(QRegExp(":\\s*1px"), QString(": %1px").arg(ScaleHeightForDPI(1))); */
  /* style = style.replace(QRegExp(":\\s*2px"), QString(": %1px").arg(ScaleHeightForDPI(2))); */
  /* style = style.replace(QRegExp(":\\s*3px"), QString(": %1px").arg(ScaleHeightForDPI(3))); */
  /* style = style.replace(QRegExp(":\\s*4px"), QString(": %1px").arg(ScaleHeightForDPI(4))); */
  /* style = style.replace(QRegExp(":\\s*5px"), QString(": %1px").arg(ScaleHeightForDPI(5))); */
  /* style = style.replace(QRegExp(":\\s*6px"), QString(": %1px").arg(ScaleHeightForDPI(6))); */
  /* style = style.replace(QRegExp(":\\s*7px"), QString(": %1px").arg(ScaleHeightForDPI(7))); */
  /* style = style.replace(QRegExp(":\\s*8px"), QString(": %1px").arg(ScaleHeightForDPI(8))); */
  /* style = style.replace(QRegExp(":\\s*9px"), QString(": %1px").arg(ScaleHeightForDPI(9))); */
  /* style = style.replace(QRegExp(":\\s*10px"), QString(": %1px").arg(ScaleHeightForDPI(10))); */
  /* style = style.replace(QRegExp(":\\s*11px"), QString(": %1px").arg(ScaleHeightForDPI(11))); */
  /* style = style.replace(QRegExp(":\\s*12px"), QString(": %1px").arg(ScaleHeightForDPI(12))); */
  /* style = style.replace(QRegExp(":\\s*13px"), QString(": %1px").arg(ScaleHeightForDPI(13))); */
  /* style = style.replace(QRegExp(":\\s*14px"), QString(": %1px").arg(ScaleHeightForDPI(14))); */
  /* style = style.replace(QRegExp(":\\s*15px"), QString(": %1px").arg(ScaleHeightForDPI(15))); */
  /* style = style.replace(QRegExp(":\\s*16px"), QString(": %1px").arg(ScaleHeightForDPI(16))); */
  /* style = style.replace(QRegExp(":\\s*17px"), QString(": %1px").arg(ScaleHeightForDPI(17))); */
  /* style = style.replace(QRegExp(":\\s*18px"), QString(": %1px").arg(ScaleHeightForDPI(18))); */
  /* style = style.replace(QRegExp(":\\s*19px"), QString(": %1px").arg(ScaleHeightForDPI(19))); */
  /* style = style.replace(QRegExp(":\\s*20px"), QString(": %1px").arg(ScaleHeightForDPI(20))); */
  /* style = style.replace(QRegExp(":\\s*21px"), QString(": %1px").arg(ScaleHeightForDPI(21))); */
  /* style = style.replace(QRegExp(":\\s*22px"), QString(": %1px").arg(ScaleHeightForDPI(22))); */
  /* style = style.replace(QRegExp(":\\s*23px"), QString(": %1px").arg(ScaleHeightForDPI(23))); */
  /* style = style.replace(QRegExp(":\\s*24px"), QString(": %1px").arg(ScaleHeightForDPI(24))); */
  /* style = style.replace(QRegExp(":\\s*25px"), QString(": %1px").arg(ScaleHeightForDPI(25))); */
  /* style = style.replace(QRegExp(":\\s*26px"), QString(": %1px").arg(ScaleHeightForDPI(26))); */
  /* style = style.replace(QRegExp(":\\s*27px"), QString(": %1px").arg(ScaleHeightForDPI(27))); */
  /* style = style.replace(QRegExp(":\\s*28px"), QString(": %1px").arg(ScaleHeightForDPI(28))); */
  /* style = style.replace(QRegExp(":\\s*29px"), QString(": %1px").arg(ScaleHeightForDPI(29))); */
  /* style = style.replace(QRegExp(":\\s*3\\dpx"), QString(": %1px").arg(ScaleHeightForDPI(30))); */
  /* style = style.replace(QRegExp(":\\s*4\\dpx"), QString(": %1px").arg(ScaleHeightForDPI(40))); */
  /* style = style.replace(QRegExp(":\\s*5\\dpx"), QString(": %1px").arg(ScaleHeightForDPI(50))); */
  /* style = style.replace(QRegExp(":\\s*6\\dpx"), QString(": %1px").arg(ScaleHeightForDPI(60))); */
  /* style = style.replace(QRegExp(":\\s*7\\dpx"), QString(": %1px").arg(ScaleHeightForDPI(70))); */
  /* style = style.replace(QRegExp(":\\s*8\\dpx"), QString(": %1px").arg(ScaleHeightForDPI(80))); */
  /* style = style.replace(QRegExp(":\\s*9\\dpx"), QString(": %1px").arg(ScaleHeightForDPI(90))); */
  /* style = style.replace(QRegExp(":\\s*1\\d\\dpx"), QString(": %1px").arg(ScaleHeightForDPI(100))); */
  /* style = style.replace(QRegExp(":\\s*2\\d\\dpx"), QString(": %1px").arg(ScaleHeightForDPI(200))); */
  /* style = style.replace(QRegExp(":\\s*3\\d\\dpx"), QString(": %1px").arg(ScaleHeightForDPI(300))); */
  /* style = style.replace(QRegExp(":\\s*4\\d\\dpx"), QString(": %1px").arg(ScaleHeightForDPI(400))); */
  /* style = style.replace(QRegExp(":\\s*5\\d\\dpx"), QString(": %1px").arg(ScaleHeightForDPI(500))); */
  style = style.replace(QRegExp("font-size:\\s*small\\s*;"), QString("font-size: %1px; /*small*/").arg(ScaleHeightForDPI(12)));
  style = style.replace(QRegExp("font-size:\\s*medium\\s*;"), QString("font-size: %1px; /*medium*/").arg(ScaleHeightForDPI(17)));
  style = style.replace(QRegExp("font-size:\\s*large\\s*;"), QString("font-size: %1px; /*large*/").arg(ScaleHeightForDPI(20)));
  style = style.replace(QRegExp("font-size:\\s*x-large\\s*;"), QString("font-size: %1px; /*x-large*/").arg(ScaleHeightForDPI(24)));
  style = style.replace(QRegExp("font-size:\\s*xx-large\\s*;"), QString("font-size: %1px; /*xx-large*/").arg(ScaleHeightForDPI(30)));

  return style;
}
