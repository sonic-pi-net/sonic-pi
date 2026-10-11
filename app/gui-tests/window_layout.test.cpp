#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QDockWidget>
#include <QMainWindow>
#include <QTest>
#include <QToolBar>
#include <QWidget>

#include "utils/dockshown.h"

// The help pane shares the bottom row with a 1px bar that stands in for it
// while it is off, shown and hidden from the help state. After the layout is
// restored the state follows the pane; read with isVisible() — false for every
// widget until the window is first shown — the pane looked off, the bar went
// up beside it, and as the window appeared the row squeezed to the pane's
// minimum. The bar came down a moment later; the squeeze stayed. The pane
// came back 47 pixels high on every launch, however high it was at quit.
// Shaped as Sonic Pi's window, which the squeeze needs: a toolbar, a right
// column with its own placeholder, the help pane and its bar along the bottom.
TEST_CASE("window layout: the help pane comes back at its height, not squeezed by its placeholder bar", "[windowlayout]")
{
    const QSize size(1492, 896);
    const QPoint pos(0, 40);
    struct SonicPiShaped
    {
        QMainWindow w;
        QDockWidget* help = nullptr;
        QDockWidget* bar = nullptr;
        SonicPiShaped()
        {
            auto* editor = new QWidget;
            editor->setMinimumHeight(300);
            w.setCentralWidget(editor);
            auto* toolbar = new QToolBar;
            toolbar->setObjectName(QStringLiteral("toolbar"));
            w.addToolBar(toolbar);
            auto dock = [&](const char* name, Qt::DockWidgetArea area) {
                auto* d = new QDockWidget(QString::fromLatin1(name), &w);
                d->setObjectName(QString::fromLatin1(name));
                auto* body = new QWidget;
                body->setMinimumHeight(30);
                d->setWidget(body);
                w.addDockWidget(area, d);
                return d;
            };
            for (const char* n : { "Scope", "output", "input", "metro" })
                dock(n, Qt::RightDockWidgetArea);
            QDockWidget* side = dock("sideAwayDock", Qt::RightDockWidgetArea);
            side->setFixedWidth(1);
            side->hide();
            help = dock("help", Qt::BottomDockWidgetArea);
            bar = dock("helpAwayDock", Qt::BottomDockWidgetArea);
            bar->setFixedHeight(1);
            w.splitDockWidget(help, bar, Qt::Horizontal);
        }
        // The help state's rule: the bar stands in for the pane while it is off.
        void followState() { bar->setVisible(!SonicPi::dockShown(help)); }
    };

    QByteArray geometry, state;
    int savedHeight = 0;
    {
        SonicPiShaped before;
        before.bar->hide();
        before.w.resize(size);
        before.w.show();
        QTest::qWait(30);
        before.w.resizeDocks({ before.help }, { 300 }, Qt::Vertical);
        QTest::qWait(30);
        savedHeight = before.help->height();
        geometry = before.w.saveGeometry();
        state = before.w.saveState();
    }
    REQUIRE(savedHeight > 250);

    for (int launch = 1; launch <= 3; ++launch)
    {
        SonicPiShaped after;
        after.help->hide();   // the help state as it begins: away, the bar up
        after.bar->show();
        // As Sonic Pi restores: the layout, the window's size and place, then
        // the help state following the pane.
        after.w.restoreState(state);
        after.w.restoreGeometry(geometry);
        after.w.resize(size);
        after.w.move(pos);
        after.followState();
        after.w.show();
        QTest::qWait(30);
        after.followState();   // shown: the state follows the pane again
        QTest::qWait(30);
        INFO("launch " << launch << ": help pane " << after.help->height() << " pixels, saved at " << savedHeight);
        CHECK(std::abs(after.help->height() - savedHeight) <= 2);
        CHECK(after.bar->isHidden());
        geometry = after.w.saveGeometry();
        state = after.w.saveState();
    }
}
