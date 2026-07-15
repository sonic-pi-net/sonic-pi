//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#include "welcomewidget.h"

#include <QHBoxLayout>
#include <QIcon>
#include <QKeyEvent>
#include <QLabel>
#include <QPainter>
#include <QPixmap>
#include <QPushButton>
#include <QVBoxLayout>

#include "dpi.h"
#include "model/sonicpitheme.h"

// Deliberately dark regardless of the app theme.
namespace
{
const QColor kStageBg(13, 13, 13);
const QColor kStageFg(242, 242, 242);
const QColor kStageDim(140, 140, 140);
} // namespace

WelcomeWidget::WelcomeWidget(SonicPiTheme* theme, bool, QWidget* parent)
    : QWidget(parent, Qt::Window), m_theme(theme)
{
    setWindowTitle(tr("Welcome to Sonic Pi"));
    setWindowIcon(QIcon(":images/icon-smaller.png"));
    setAttribute(Qt::WA_DeleteOnClose);
    setFixedSize(ScaleHeightForDPI(600), ScaleHeightForDPI(680));

    const QColor accentColor = m_theme->applyGlobalTransforms(QColor("deeppink"));
    const QString accent = accentColor.name();
    const QString fg = kStageFg.name();
    const QString dim = kStageDim.name();

    QVBoxLayout* layout = new QVBoxLayout(this);
    const int pad = ScaleHeightForDPI(40);
    layout->setContentsMargins(pad, ScaleHeightForDPI(34), pad, ScaleHeightForDPI(38));
    layout->setSpacing(0);

    // Downscale in device pixels so the logo stays sharp on high-DPI displays.
    QLabel* mark = new QLabel(this);
    QPixmap logo(":/images/logo-transparent-dark.png");
    const qreal dpr = devicePixelRatioF();
    const int markW = ScaleHeightForDPI(250);
    if (logo.width() > markW * dpr)
        logo = logo.scaledToWidth(qRound(markW * dpr), Qt::SmoothTransformation);
    logo.setDevicePixelRatio(dpr);
    mark->setPixmap(logo);
    mark->setStyleSheet("background: transparent;");
    layout->addWidget(mark, 0, Qt::AlignHCenter);

    // Font properties must live in the stylesheet: Qt ignores
    // QWidget::setFont on styled widgets.
    QWidget* tagline = new QWidget(this);
    tagline->setStyleSheet("background: transparent;");
    QHBoxLayout* taglineLayout = new QHBoxLayout(tagline);
    taglineLayout->setContentsMargins(0, 0, 0, 0);
    taglineLayout->setSpacing(ScaleHeightForDPI(12));
    const QStringList words = { "Code", "Music", "Live" };
    for (int i = 0; i < words.size(); ++i)
    {
        QLabel* word = new QLabel(
            QString("%1<span style=\"color: %2;\">.</span>").arg(words[i]).arg(accent), tagline);
        word->setTextFormat(Qt::RichText);
        word->setStyleSheet(QString("color: %1; background: transparent; font-family: Hack;"
                                    " font-size: %2px; font-style: italic; font-weight: 700;")
                                .arg(fg)
                                .arg(ScaleHeightForDPI(38)));
        taglineLayout->addWidget(word);
    }
    layout->addSpacing(ScaleHeightForDPI(60));
    layout->addWidget(tagline, 0, Qt::AlignHCenter);

    QLabel* greeting = new QLabel(tr("Hello, and welcome!"), this);
    greeting->setAlignment(Qt::AlignHCenter);
    greeting->setStyleSheet(QString("color: %1; background: transparent; font-size: %2px;"
                                    " font-weight: 600;")
                                .arg(fg)
                                .arg(ScaleHeightForDPI(22)));
    layout->addSpacing(ScaleHeightForDPI(58));
    layout->addWidget(greeting);

    QLabel* body = new QLabel(
        tr("Sonic Pi is a musical instrument you play by writing code."), this);
    body->setAlignment(Qt::AlignHCenter);
    body->setWordWrap(true);
    body->setStyleSheet(QString("color: %1; background: transparent; font-size: %2px;")
                            .arg(dim)
                            .arg(ScaleHeightForDPI(20)));
    layout->addSpacing(ScaleHeightForDPI(16));
    layout->addWidget(body);

    QLabel* motto = new QLabel(
        tr("Remember, when you code live there are no mistakes, only opportunities..."),
        this);
    motto->setAlignment(Qt::AlignHCenter);
    motto->setWordWrap(true);
    motto->setStyleSheet(QString("color: %1; background: transparent; font-size: %2px;"
                                 " font-style: italic;")
                             .arg(dim)
                             .arg(ScaleHeightForDPI(20)));
    layout->addSpacing(ScaleHeightForDPI(26));
    layout->addWidget(motto);

    layout->addStretch(1);

    QPushButton* start = new QPushButton(tr("Get Started"), this);
    start->setCursor(Qt::PointingHandCursor);
    start->setDefault(true);
    start->setStyleSheet(
        QString("QPushButton { background-color: %1; color: %2; border: 2px solid transparent;"
                " border-radius: %3px; padding: %4px %5px; font-size: %6px; font-weight: 600; }"
                "QPushButton:hover { background-color: %7; }"
                "QPushButton:focus { border: 2px solid %8; }")
            .arg(accent)
            .arg(m_theme->contrastingText(accentColor).name())
            .arg(ScaleHeightForDPI(24))
            .arg(ScaleHeightForDPI(12))
            .arg(ScaleHeightForDPI(38))
            .arg(ScaleHeightForDPI(17))
            .arg(m_theme->applyGlobalTransforms(QColor(255, 71, 163)).name())
            .arg(fg));
    connect(start, &QPushButton::clicked, this, &WelcomeWidget::dismissRequested);
    layout->addWidget(start, 0, Qt::AlignHCenter);

    start->setFocus();
}

void WelcomeWidget::paintEvent(QPaintEvent*)
{
    QPainter p(this);
    p.fillRect(rect(), kStageBg);

    // Faint sound-wave arcs radiating from the logo's pi position.
    p.setRenderHint(QPainter::Antialiasing);
    const qreal cx = width() * 0.425;
    const qreal cy = height() * 0.15;
    for (int i = 0; i < 3; ++i)
    {
        const qreal r = width() * (0.38 + 0.21 * i);
        QColor wave(255, 255, 255, 6);
        QPen pen(wave, width() * (0.040 + 0.010 * i), Qt::SolidLine, Qt::RoundCap);
        p.setPen(pen);
        QRectF box(cx - r, cy - r, r * 2, r * 2);
        p.drawArc(box, -60 * 16, 95 * 16);
    }
}

void WelcomeWidget::keyPressEvent(QKeyEvent* event)
{
    if (event->key() == Qt::Key_Escape)
    {
        emit dismissRequested();
        return;
    }
    QWidget::keyPressEvent(event);
}
