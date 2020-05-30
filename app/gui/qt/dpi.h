#pragma once

#include <QGuiApplication>
#include <QScreen>

inline QSizeF GetDisplayScale()
{
//super hacky temporary fudge to paper over the
//massive cracks that is the difference between how
//macOS and other platforms handle high DPI monitors
#ifdef __APPLE__
    float scale = 96.0f;
#else
    float scale = 96.0f * 1.6f;
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
  style = style.replace(QRegExp(":\\s*1dx"), QString(": %1px").arg(ScaleHeightForDPI(1)));
  style = style.replace(QRegExp(":\\s*2dx"), QString(": %1px").arg(ScaleHeightForDPI(2)));
  style = style.replace(QRegExp(":\\s*3dx"), QString(": %1px").arg(ScaleHeightForDPI(3)));
  style = style.replace(QRegExp(":\\s*4dx"), QString(": %1px").arg(ScaleHeightForDPI(4)));
  style = style.replace(QRegExp(":\\s*5dx"), QString(": %1px").arg(ScaleHeightForDPI(5)));
  style = style.replace(QRegExp(":\\s*6dx"), QString(": %1px").arg(ScaleHeightForDPI(6)));
  style = style.replace(QRegExp(":\\s*7dx"), QString(": %1px").arg(ScaleHeightForDPI(7)));
  style = style.replace(QRegExp(":\\s*8dx"), QString(": %1px").arg(ScaleHeightForDPI(8)));
  style = style.replace(QRegExp(":\\s*9dx"), QString(": %1px").arg(ScaleHeightForDPI(9)));
  style = style.replace(QRegExp(":\\s*10dx"), QString(": %1px").arg(ScaleHeightForDPI(10)));
  style = style.replace(QRegExp(":\\s*11dx"), QString(": %1px").arg(ScaleHeightForDPI(11)));
  style = style.replace(QRegExp(":\\s*12dx"), QString(": %1px").arg(ScaleHeightForDPI(12)));
  style = style.replace(QRegExp(":\\s*13dx"), QString(": %1px").arg(ScaleHeightForDPI(13)));
  style = style.replace(QRegExp(":\\s*14dx"), QString(": %1px").arg(ScaleHeightForDPI(14)));
  style = style.replace(QRegExp(":\\s*15dx"), QString(": %1px").arg(ScaleHeightForDPI(15)));
  style = style.replace(QRegExp(":\\s*16dx"), QString(": %1px").arg(ScaleHeightForDPI(16)));
  style = style.replace(QRegExp(":\\s*17dx"), QString(": %1px").arg(ScaleHeightForDPI(17)));
  style = style.replace(QRegExp(":\\s*18dx"), QString(": %1px").arg(ScaleHeightForDPI(18)));
  style = style.replace(QRegExp(":\\s*19dx"), QString(": %1px").arg(ScaleHeightForDPI(19)));
  style = style.replace(QRegExp(":\\s*20dx"), QString(": %1px").arg(ScaleHeightForDPI(20)));
  style = style.replace(QRegExp(":\\s*21dx"), QString(": %1px").arg(ScaleHeightForDPI(21)));
  style = style.replace(QRegExp(":\\s*22dx"), QString(": %1px").arg(ScaleHeightForDPI(22)));
  style = style.replace(QRegExp(":\\s*23dx"), QString(": %1px").arg(ScaleHeightForDPI(23)));
  style = style.replace(QRegExp(":\\s*24dx"), QString(": %1px").arg(ScaleHeightForDPI(24)));
  style = style.replace(QRegExp(":\\s*25dx"), QString(": %1px").arg(ScaleHeightForDPI(25)));
  style = style.replace(QRegExp(":\\s*26dx"), QString(": %1px").arg(ScaleHeightForDPI(26)));
  style = style.replace(QRegExp(":\\s*27dx"), QString(": %1px").arg(ScaleHeightForDPI(27)));
  style = style.replace(QRegExp(":\\s*28dx"), QString(": %1px").arg(ScaleHeightForDPI(28)));
  style = style.replace(QRegExp(":\\s*29dx"), QString(": %1px").arg(ScaleHeightForDPI(29)));
  style = style.replace(QRegExp(":\\s*3\\ddx"), QString(": %1px").arg(ScaleHeightForDPI(30)));
  style = style.replace(QRegExp(":\\s*4\\ddx"), QString(": %1px").arg(ScaleHeightForDPI(40)));
  style = style.replace(QRegExp(":\\s*5\\ddx"), QString(": %1px").arg(ScaleHeightForDPI(50)));
  style = style.replace(QRegExp(":\\s*6\\ddx"), QString(": %1px").arg(ScaleHeightForDPI(60)));
  style = style.replace(QRegExp(":\\s*7\\ddx"), QString(": %1px").arg(ScaleHeightForDPI(70)));
  style = style.replace(QRegExp(":\\s*8\\ddx"), QString(": %1px").arg(ScaleHeightForDPI(80)));
  style = style.replace(QRegExp(":\\s*9\\ddx"), QString(": %1px").arg(ScaleHeightForDPI(90)));
  style = style.replace(QRegExp(":\\s*1\\d\\ddx"), QString(": %1px").arg(ScaleHeightForDPI(100)));
  style = style.replace(QRegExp(":\\s*2\\d\\ddx"), QString(": %1px").arg(ScaleHeightForDPI(200)));
  style = style.replace(QRegExp(":\\s*3\\d\\ddx"), QString(": %1px").arg(ScaleHeightForDPI(300)));
  style = style.replace(QRegExp(":\\s*4\\d\\ddx"), QString(": %1px").arg(ScaleHeightForDPI(400)));
  style = style.replace(QRegExp(":\\s*5\\d\\ddx"), QString(": %1px").arg(ScaleHeightForDPI(500)));

#ifdef __APPLE__
  style = style.replace(QRegExp("font-size:\\s*small\\s*;"), QString("font-size: %1px; /*small*/").arg(ScaleHeightForDPI(13)));
  style = style.replace(QRegExp("font-size:\\s*medium\\s*;"), QString("font-size: %1px; /*medium*/").arg(ScaleHeightForDPI(18)));
  style = style.replace(QRegExp("font-size:\\s*large\\s*;"), QString("font-size: %1px; /*large*/").arg(ScaleHeightForDPI(21)));
  style = style.replace(QRegExp("font-size:\\s*x-large\\s*;"), QString("font-size: %1px; /*x-large*/").arg(ScaleHeightForDPI(25)));
  style = style.replace(QRegExp("font-size:\\s*xx-large\\s*;"), QString("font-size: %1px; /*xx-large*/").arg(ScaleHeightForDPI(31)));
#else
  style = style.replace(QRegExp("font-size:\\s*small\\s*;"), QString("font-size: %1px; /*small*/").arg(ScaleHeightForDPI(14)));
  style = style.replace(QRegExp("font-size:\\s*medium\\s*;"), QString("font-size: %1px; /*medium*/").arg(ScaleHeightForDPI(19)));
  style = style.replace(QRegExp("font-size:\\s*large\\s*;"), QString("font-size: %1px; /*large*/").arg(ScaleHeightForDPI(22)));
  style = style.replace(QRegExp("font-size:\\s*x-large\\s*;"), QString("font-size: %1px; /*x-large*/").arg(ScaleHeightForDPI(26)));
  style = style.replace(QRegExp("font-size:\\s*xx-large\\s*;"), QString("font-size: %1px; /*xx-large*/").arg(ScaleHeightForDPI(32)));
#endif

  return style;
}
