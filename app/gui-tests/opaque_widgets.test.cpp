// The GUI's animated widgets paint every pixel of themselves and say so
// (WA_OpaquePaintEvent), so Qt repaints nothing behind them: the styled cells,
// cards and panels under them stay put. Qt's stylesheet style clears that
// attribute whenever it polishes a widget — the app's stylesheet applied, a
// theme switch — and the app has a stylesheet. Under it, every one of these
// lost the attribute, and each frame repainted what was behind it too. Each
// is checked here under the app's own stylesheet, applied before and after
// the widget is shown.

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QImage>
#include <QPainter>
#include <QScrollBar>
#include <QTest>

#include "model/sonicpitheme.h"
#include "utils/opaque.h"
#include "widgets/metricslog.h"
#include "widgets/metricvalue.h"
#include "widgets/nodetreegraph.h"

namespace {

// The app's stylesheet on for the case, off after it.
struct AppStylesheet
{
    SonicPiTheme theme{ nullptr, "", QStringLiteral(SP_ROOT) };
    AppStylesheet() { qApp->setStyleSheet(theme.getAppStylesheet()); }
    ~AppStylesheet() { qApp->setStyleSheet(QString()); }
    // As a theme switch does: the sheet set again over shown widgets.
    void again() { qApp->setStyleSheet(QString()); qApp->setStyleSheet(theme.getAppStylesheet()); }
};

bool opaqueThroughRestyle(QWidget* shown, QWidget* painter, AppStylesheet& sheet)
{
    shown->show();
    QTest::qWait(20);
    if (!painter->testAttribute(Qt::WA_OpaquePaintEvent))
        return false;
    sheet.again();
    QTest::qWait(20);
    return painter->testAttribute(Qt::WA_OpaquePaintEvent);
}

} // namespace

TEST_CASE("opaque widgets: a kept-opaque widget stays so through the app's stylesheet", "[opaque]")
{
    AppStylesheet sheet;
    SonicPi::KeptOpaque<QWidget> w;
    CHECK(opaqueThroughRestyle(&w, &w, sheet));
    QWidget plain;   // and a widget that only set the attribute loses it: the reason for the above
    plain.setAttribute(Qt::WA_OpaquePaintEvent, true);
    CHECK(!opaqueThroughRestyle(&plain, &plain, sheet));
}

TEST_CASE("opaque widgets: a metric value stays opaque under the app's stylesheet", "[opaque]")
{
    AppStylesheet sheet;
    SonicPi::MetricValue v;
    CHECK(opaqueThroughRestyle(&v, &v, sheet));
}

TEST_CASE("opaque widgets: a log card's view stays opaque under the app's stylesheet", "[opaque]")
{
    AppStylesheet sheet;
    SonicPi::MetricsLog log;
    CHECK(opaqueThroughRestyle(&log, log.viewport(), sheet));
}

TEST_CASE("opaque widgets: the node tree stays opaque under the app's stylesheet", "[opaque]")
{
    AppStylesheet sheet;
    NodeTreeGraph g;
    CHECK(opaqueThroughRestyle(&g, &g, sheet));
}

// An opaque widget's frame is put on screen as a copy, not blended: a tenth of
// the cost for a 2x frame. Copying is what makes it fast, and what makes it
// wrong for a frame that is not opaque, so the helper is pinned as a copy:
// whatever was there is replaced, pixel for pixel.
TEST_CASE("opaque widgets: a frame is copied onto the window, not blended", "[opaque]")
{
    QImage frame(4, 4, QImage::Format_ARGB32_Premultiplied);
    frame.fill(QColor(20, 40, 60));
    frame.setPixelColor(1, 1, QColor(200, 0, 0, 128));   // were it blended, the green behind would show here
    QImage window(4, 4, QImage::Format_ARGB32_Premultiplied);
    window.fill(Qt::green);
    {
        QPainter p(&window);
        SonicPi::drawOpaqueFrame(p, QPointF(0, 0), frame);
        CHECK(p.compositionMode() == QPainter::CompositionMode_SourceOver);   // the painter is left as it was
    }
    for (int y = 0; y < 4; ++y)
        for (int x = 0; x < 4; ++x)
            CHECK(window.pixel(x, y) == frame.pixel(x, y));
}
