// The Debug pane's metrics panel sets its text size in its own stylesheet,
// built with QString::arg. Removing a rule took the only %2 with it, every
// later argument moved down a placeholder, and the font size became the grid
// line's width: the whole panel drawn in 1-pixel text (2026-10-11). Built on
// the real panel, headless, with no engine: every label and log card is the
// size the zoom asks for.

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QLabel>
#include <QTest>

#include "model/sonicpitheme.h"
#include "utils/framepacer.h"
#include "widgets/metricslog.h"
#include "widgets/metricspanel.h"
#include "widgets/metricvalue.h"

TEST_CASE("metrics panel: its text is the size its zoom asks for", "[metricspanel]")
{
    SonicPiTheme theme(nullptr, "", QStringLiteral(SP_ROOT));
    MetricsPanel panel(nullptr);
    panel.resize(900, 600);
    panel.show();
    panel.applyTheme(&theme);
    const int zoom = -1;
    panel.setFontZoom(zoom);
    QTest::qWait(50);

    const int wantPx = 11 + zoom;
    const auto titles = panel.findChildren<QLabel*>(QStringLiteral("ssCardTitle"));
    REQUIRE(!titles.isEmpty());
    for (QLabel* t : titles)
    {
        t->ensurePolished();
        INFO("card title '" << t->text().toStdString() << "'");
        CHECK(QFontInfo(t->font()).pixelSize() == wantPx);
    }
    const auto values = panel.findChildren<SonicPi::MetricValue*>();
    REQUIRE(values.size() > 10);
    for (SonicPi::MetricValue* v : values)
    {
        v->ensurePolished();
        CHECK(QFontInfo(v->font()).pixelSize() == wantPx);
        CHECK(!v->text().isEmpty());   // no engine: the disconnected dashes
    }
    const auto logs = panel.findChildren<SonicPi::MetricsLog*>();
    REQUIRE(logs.size() == 3);
    for (SonicPi::MetricsLog* l : logs)
    {
        l->ensurePolished();
        CHECK(QFontInfo(l->font()).pixelSize() == wantPx);
    }
}

// Shown, the panel reads the node tree on the frame pacer's tick and holds the
// pacer running; hidden, it lets go. Destroyed while shown, it let go of
// nothing, and the pacer ticked on for a panel that was gone.
TEST_CASE("metrics panel: lets go of the frame pacer when it goes, shown or not", "[metricspanel]")
{
    auto* pacer = SonicPi::FramePacer::instance();
    const int before = pacer->clients();
    {
        MetricsPanel panel(nullptr);
        panel.show();
        QTest::qWait(20);
        CHECK(pacer->clients() > before);
    }
    // Work the panel deferred to the next tick holds the pacer until that
    // tick has run and dropped it; then nothing of the panel's is left.
    CHECK(QTest::qWaitFor([&] { return pacer->clients() == before; }, 500));
}
