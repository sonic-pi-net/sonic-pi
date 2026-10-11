// A metric's value in the Debug pane changes several times a second. As a
// rich-text label, each change parsed HTML, laid out a document and repainted
// the styled grid cell behind it, borders and all, since a label is
// see-through: a sixth of the GUI's main thread with loops running. These
// hold that a value paints only itself, every pixel of it, that an unchanged
// value paints nothing, and that it reads as the label did. Headless.

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QEvent>
#include <QFrame>
#include <QImage>
#include <QTest>
#include <QVBoxLayout>

#include "widgets/metricvalue.h"

using SonicPi::MetricValue;

namespace {

struct PaintCounter : QObject
{
    int paints = 0;
    bool eventFilter(QObject*, QEvent* e) override
    {
        if (e->type() == QEvent::Paint)
            paints++;
        return false;
    }
};

// A value in a styled cell, as the panel has it.
struct Cell
{
    QFrame frame;
    MetricValue* value = new MetricValue(&frame);
    Cell()
    {
        frame.setObjectName(QStringLiteral("ssCell"));
        frame.setStyleSheet(QStringLiteral("QFrame#ssCell { background:#202020; border-right:2px solid #555555; }"));
        auto* layout = new QVBoxLayout(&frame);
        layout->setContentsMargins(4, 4, 4, 4);
        layout->addWidget(value);
        value->setBackground(QColor("#202020"));
        frame.resize(200, 40);
        frame.show();
        QTest::qWait(50);
    }
};

QVector<LogRun> runs(const QString& number)
{
    return { { QColor("#ffffff"), number, QString() }, { QColor("#808080"), QStringLiteral(" / 40.0%"), QString() } };
}

} // namespace

TEST_CASE("metric value: a change repaints the value and not the cell behind it", "[metricvalue]")
{
    Cell c;
    c.value->setRuns(runs(QStringLiteral("12.5")));
    QTest::qWait(30);
    PaintCounter cell, value;
    c.frame.installEventFilter(&cell);
    c.value->installEventFilter(&value);
    c.value->setRuns(runs(QStringLiteral("13.0")));
    QTest::qWait(30);
    CHECK(value.paints == 1);
    CHECK(cell.paints == 0);
}

TEST_CASE("metric value: the same value again repaints nothing", "[metricvalue]")
{
    Cell c;
    c.value->setRuns(runs(QStringLiteral("12.5")));
    QTest::qWait(30);
    PaintCounter value;
    c.value->installEventFilter(&value);
    c.value->setRuns(runs(QStringLiteral("12.5")));
    QTest::qWait(30);
    CHECK(value.paints == 0);
}

TEST_CASE("metric value: paints every pixel of itself, its text right-aligned", "[metricvalue]")
{
    MetricValue v;
    v.setBackground(QColor(30, 30, 30, 120));   // opaque whatever the theme says
    v.setRuns({ { Qt::white, QStringLiteral("8"), QString() } });
    v.resize(120, 20);
    auto over = [&](const QColor& behind) {
        QImage img(v.size(), QImage::Format_ARGB32_Premultiplied);
        img.fill(behind);
        v.render(&img, QPoint(), QRegion(), QWidget::DrawChildren);
        return img;
    };
    const QImage a = over(QColor(255, 0, 255)), b = over(QColor(0, 255, 0));
    int showingThrough = 0, litLeft = 0, litRight = 0;
    for (int y = 0; y < a.height(); ++y)
        for (int x = 0; x < a.width(); ++x)
        {
            if (a.pixel(x, y) != b.pixel(x, y))
                ++showingThrough;
            if (qGray(a.pixel(x, y)) > 128)
                (x < a.width() / 2 ? litLeft : litRight)++;
        }
    CHECK(showingThrough == 0);
    CHECK(litLeft == 0);
    CHECK(litRight > 0);
}

TEST_CASE("metric value: reads as its runs, joined", "[metricvalue]")
{
    MetricValue v;
    v.setRuns(runs(QStringLiteral("12.5")));
    CHECK(v.text() == QStringLiteral("12.5 / 40.0%"));
    CHECK(v.runs().at(1).color == QColor("#808080"));
}
