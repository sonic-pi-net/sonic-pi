//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

// The help's text-size controls (A-/A+) sit at the foot of its tab rail, as
// the web's sit at the foot of its rail: larger over smaller, as low as the
// rail goes, across its width, and never over a tab when the dock is short.
// With SP_GUI_SHOT_DIR set it renders the rail offscreen to PNGs so the look
// can be checked without launching the app.

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QDir>
#include <QPushButton>
#include <QVBoxLayout>

#include "dpi.h"
#include "model/sonicpitheme.h"
#include "utils/chrome_metrics.h"
#include "widgets/icontabbar.h"
#include "widgets/zoombar.h"

namespace
{
void settle()
{
    for (int i = 0; i < 8; i++)
        QApplication::processEvents();
}

void shoot(QWidget& w, const QString& name)
{
    const QString dir = qEnvironmentVariable("SP_GUI_SHOT_DIR");
    if (dir.isEmpty()) return;
    QDir().mkpath(dir);
    w.grab().save(dir + QStringLiteral("/") + name + QStringLiteral(".png"));
}

// The help dock's tabs as MainWindow builds them: a West rail of square
// icon tabs (Cards, Docs, Logs, Debug, Tracks), each with its own A-/A+
// pair in the foot, the current tab's shown.
struct HelpRail
{
    SonicPiTheme* theme;
    IconTabWidget tabs;
    QVector<ZoomBar*> zooms;

    HelpRail()
        : theme(new SonicPiTheme(nullptr, "", QStringLiteral(SP_ROOT)))
    {
        qApp->setStyleSheet(theme->getAppStylesheet());
        tabs.setObjectName("southTabs");
        tabs.setTabPosition(QTabWidget::West);
        const int side = ScaleHeightForDPI(SonicPi::kChromeControlDp);
        static_cast<IconTabBar*>(tabs.tabBar())->setSquareSide(side);
        QWidget* foot = new QWidget;
        QVBoxLayout* lay = new QVBoxLayout(foot);
        lay->setContentsMargins(0, 0, 0, ScaleHeightForDPI(6));
        lay->setSpacing(0);
        for (const char* subject : { "quickstart", "documentation", "logs", "metrics", "tracks" })
        {
            tabs.addTab(new QWidget, QString());
            ZoomBar* zoom = new ZoomBar(theme, QString::fromLatin1(subject));
            zooms.append(zoom);
            lay->addWidget(zoom, 0, Qt::AlignHCenter);
        }
        tabs.setFootWidget(foot);
        showZoomFor(1);   // Docs
    }

    void showZoomFor(int tab)
    {
        tabs.setCurrentIndex(tab);
        for (int i = 0; i < zooms.size(); i++)
            zooms[i]->setVisible(i == tab);
    }

    QRect rail() const { return tabs.tabBar()->geometry(); }
    QRect foot() const { return tabs.footWidget()->geometry(); }
};
} // namespace

TEST_CASE("the help's text-size controls sit at the foot of its tab rail", "[helprail][style]")
{
    HelpRail h;
    h.tabs.resize(480, 360);
    h.tabs.show();
    settle();
    shoot(h.tabs, "helprail-tall");

    INFO("rail " << h.rail().x() << "," << h.rail().y() << " " << h.rail().width() << "x"
                 << h.rail().height() << "; foot " << h.foot().x() << "," << h.foot().y() << " "
                 << h.foot().width() << "x" << h.foot().height());
    CHECK(h.foot().bottom() == h.tabs.height() - 1);          // as low as the rail goes
    CHECK(h.foot().top() >= h.rail().bottom() + 1);           // under the last tab
    CHECK(h.foot().left() >= h.rail().left());                // across the rail's width
    CHECK(h.foot().right() <= h.rail().right());

    // Larger over smaller, the current tab's pair, and named for its tab.
    const auto buttons = h.zooms[1]->findChildren<QPushButton*>("zoomBtn");
    REQUIRE(buttons.size() == 2);
    CHECK(buttons[0]->accessibleName() == "Make documentation text larger");
    CHECK(buttons[1]->accessibleName() == "Make documentation text smaller");
    CHECK(buttons[0]->y() < buttons[1]->y());
    CHECK(buttons[0]->x() == buttons[1]->x());
}

TEST_CASE("a help dock too short for the rail's foot keeps it off the tabs", "[helprail][style]")
{
    HelpRail h;
    h.tabs.resize(480, 360);
    h.tabs.show();
    settle();
    // Room under the tabs for less than the foot.
    h.tabs.resize(480, h.rail().bottom() + 1 + h.foot().height() / 2);
    settle();
    shoot(h.tabs, "helprail-short");

    INFO("rail bottom " << h.rail().bottom() << "; foot top " << h.foot().top());
    CHECK(h.foot().top() == h.rail().bottom() + 1);
}

TEST_CASE("another tab's text-size controls take the same place at the rail's foot",
          "[helprail][style]")
{
    HelpRail h;
    h.tabs.resize(480, 360);
    h.tabs.show();
    settle();
    const QRect docs = h.foot();

    h.showZoomFor(2);   // Logs
    settle();
    CHECK(h.foot() == docs);
    CHECK(h.zooms[2]->isVisible());
    CHECK_FALSE(h.zooms[1]->isVisible());
}
