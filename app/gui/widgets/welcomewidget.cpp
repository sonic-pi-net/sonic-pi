//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#include "welcomewidget.h"

#include <QFrame>
#include <QGraphicsOpacityEffect>
#include <QHBoxLayout>
#include <QIcon>
#include <QKeyEvent>
#include <QLabel>
#include <QPainter>
#include <QPixmap>
#include <QPropertyAnimation>
#include <QPushButton>
#include <QTimer>
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

WelcomeWidget::WelcomeWidget(SonicPiTheme* theme, bool reduceMotion, QWidget* parent)
    : QWidget(parent, Qt::Window), m_theme(theme), m_reduceMotion(reduceMotion)
{
    setWindowTitle(tr("Welcome to Sonic Pi"));
    setWindowIcon(QIcon(":images/icon-smaller.png"));
    setAttribute(Qt::WA_DeleteOnClose);
    setFixedSize(ScaleHeightForDPI(600), ScaleHeightForDPI(640));

    const QColor accentColor = m_theme->applyGlobalTransforms(QColor("deeppink"));
    const QString accent = accentColor.name();
    const QString fg = kStageFg.name();
    const QString dim = kStageDim.name();

    QVBoxLayout* layout = new QVBoxLayout(this);
    const int pad = ScaleHeightForDPI(40);
    layout->setContentsMargins(pad, ScaleHeightForDPI(30), pad, ScaleHeightForDPI(34));
    layout->setSpacing(0);

    // Collect a widget into the staggered entrance (no-op under reduce
    // motion). The opacity effect owns its fade animation, so removing the
    // effect in finishIntro() also stops and frees the animation.
    auto enters = [&](QWidget* w, int delayMs) {
        if (m_reduceMotion)
            return;
        QGraphicsOpacityEffect* effect = new QGraphicsOpacityEffect(w);
        effect->setOpacity(0.0);
        w->setGraphicsEffect(effect);
        m_introTargets.append({ w, delayMs });
    };

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
    enters(mark, 0);

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
        enters(word, 500 + i * 350);
    }
    layout->addSpacing(ScaleHeightForDPI(26));
    layout->addWidget(tagline, 0, Qt::AlignHCenter);

    QLabel* blurb = new QLabel(
        tr("Simple enough for your first ever note.\n"
           "Powerful enough to perform in nightclubs."),
        this);
    blurb->setAlignment(Qt::AlignHCenter);
    blurb->setStyleSheet(QString("color: %1; background: transparent; font-size: %2px;"
                                 " font-style: italic;")
                             .arg(dim)
                             .arg(ScaleHeightForDPI(18)));
    layout->addSpacing(ScaleHeightForDPI(14));
    layout->addWidget(blurb);
    enters(blurb, 1550);

    layout->addStretch(1);
    QWidget* rows = new QWidget(this);
    rows->setStyleSheet("background: transparent;");
    QVBoxLayout* rowsLayout = new QVBoxLayout(rows);
    rowsLayout->setContentsMargins(0, 0, 0, 0);
    rowsLayout->setSpacing(ScaleHeightForDPI(10));
    auto row = [&](const QString& lead, const QString& detail) {
        QFrame* card = new QFrame(rows);
        card->setFixedWidth(ScaleHeightForDPI(470));
        card->setStyleSheet(QString("QFrame { background-color: #232323; border-radius: %1px; }")
                                .arg(ScaleHeightForDPI(10)));
        QHBoxLayout* cardLayout = new QHBoxLayout(card);
        cardLayout->setContentsMargins(ScaleHeightForDPI(16), ScaleHeightForDPI(11),
                                       ScaleHeightForDPI(16), ScaleHeightForDPI(11));
        // Fixed-width lead column so the detail sentences share a left edge.
        QLabel* leadLabel = new QLabel(
            QString("<span style=\"color: %1;\">&#9654;</span>&nbsp;&nbsp;"
                    "<b style=\"color: %2; font-size: %3px;\">%4</b>")
                .arg(accent)
                .arg(fg)
                .arg(ScaleHeightForDPI(19))
                .arg(lead),
            card);
        leadLabel->setTextFormat(Qt::RichText);
        leadLabel->setFixedWidth(ScaleHeightForDPI(118));
        leadLabel->setStyleSheet(QString("background: transparent; font-size: %1px;")
                                     .arg(ScaleHeightForDPI(17)));
        QLabel* detailLabel = new QLabel(
            QString("<span style=\"color: #d4d4d4;\">%1</span>").arg(detail), card);
        detailLabel->setTextFormat(Qt::RichText);
        detailLabel->setStyleSheet(QString("background: transparent; font-size: %1px;")
                                       .arg(ScaleHeightForDPI(17)));
        cardLayout->addWidget(leadLabel);
        cardLayout->addWidget(detailLabel, 1);
        rowsLayout->addWidget(card);
    };
    // Hack runs optically larger than the sans face, so the code span gets a
    // slightly smaller point size to sit level with the sentence.
    row(tr("Play"), tr("type <span style=\"color:%1; font-family:Hack; font-size:%3px;\">play 70</span>, hit <b style=\"color:%2;\">Run</b>")
                        .arg(accent)
                        .arg(fg)
                        .arg(ScaleHeightForDPI(15)));
    row(tr("Learn"), tr("follow the tutorial below"));
    row(tr("Explore"), tr("remix the examples"));
    layout->addWidget(rows, 0, Qt::AlignHCenter);
    enters(rows, 1550);

    layout->addStretch(2);

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
    enters(start, 1550);

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

void WelcomeWidget::showEvent(QShowEvent* event)
{
    QWidget::showEvent(event);
    if (!m_introStarted)
    {
        m_introStarted = true;
        beginIntro();
    }
}

void WelcomeWidget::beginIntro()
{
    if (m_introTargets.isEmpty())
    {
        m_introDone = true;
        return;
    }
    for (const auto& target : m_introTargets)
    {
        QWidget* w = target.first;
        QTimer::singleShot(target.second, this, [this, w] {
            QGraphicsOpacityEffect* effect =
                qobject_cast<QGraphicsOpacityEffect*>(w->graphicsEffect());
            if (m_introDone || !effect)
                return;
            QPropertyAnimation* fade = new QPropertyAnimation(effect, "opacity", effect);
            fade->setDuration(320);
            fade->setStartValue(0.0);
            fade->setEndValue(1.0);
            fade->setEasingCurve(QEasingCurve::OutCubic);
            fade->start(QAbstractAnimation::DeleteWhenStopped);
        });
    }
    // Once the last element has landed, drop the opacity effects so nothing
    // renders through an effect raster (keeps the logo pixel-sharp).
    const int last = m_introTargets.last().second;
    QTimer::singleShot(last + 400, this, &WelcomeWidget::finishIntro);
}

void WelcomeWidget::finishIntro()
{
    if (m_introDone)
        return;
    m_introDone = true;
    for (const auto& target : m_introTargets)
        target.first->setGraphicsEffect(nullptr);
}

void WelcomeWidget::keyPressEvent(QKeyEvent* event)
{
    if (!m_introDone)
    {
        finishIntro();
        return;
    }
    if (event->key() == Qt::Key_Escape)
    {
        emit dismissRequested();
        return;
    }
    QWidget::keyPressEvent(event);
}

void WelcomeWidget::mousePressEvent(QMouseEvent* event)
{
    if (!m_introDone)
    {
        finishIntro();
        return;
    }
    QWidget::mousePressEvent(event);
}
