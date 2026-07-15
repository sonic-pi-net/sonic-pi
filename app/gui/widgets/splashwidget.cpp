//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#include "splashwidget.h"

#include <QGraphicsOpacityEffect>
#include <QHBoxLayout>
#include <QLabel>
#include <QPainter>
#include <QPixmap>
#include <QPropertyAnimation>
#include <QScreen>
#include <QTimer>
#include <QVBoxLayout>

#include "config.h" // SONIC_PI_VERSION
#include "dpi.h"

// No theme exists this early, so the palette is fixed (matches the
// WelcomeWidget stage).
namespace
{
const QColor kStageBg(13, 13, 13);
const QColor kStageFg(242, 242, 242);
const QColor kStageDim(200, 200, 200);
const QColor kAccent(255, 20, 147); // deeppink, the brand accent
} // namespace

SplashWidget::SplashWidget(QWidget* parent)
    : QWidget(parent, Qt::SplashScreen)
{
    setAttribute(Qt::WA_DeleteOnClose);
    setAccessibleName(tr("Sonic Pi is starting"));
    setFixedSize(ScaleHeightForDPI(928), ScaleHeightForDPI(608));

    const QString accent = kAccent.name();
    const QString fg = kStageFg.name();
    const QString dim = kStageDim.name();

    QVBoxLayout* layout = new QVBoxLayout(this);
    const int pad = ScaleHeightForDPI(44);
    layout->setContentsMargins(pad, pad, pad, pad);
    layout->setSpacing(0);
    layout->addStretch(3);

    // Downscale in device pixels so the logo stays sharp on high-DPI displays.
    QLabel* mark = new QLabel(this);
    QPixmap logo(":/images/logo-transparent-dark.png");
    const qreal dpr = devicePixelRatioF();
    const int markW = ScaleHeightForDPI(340);
    if (logo.width() > markW * dpr)
        logo = logo.scaledToWidth(qRound(markW * dpr), Qt::SmoothTransformation);
    logo.setDevicePixelRatio(dpr);
    mark->setPixmap(logo);
    mark->setStyleSheet("background: transparent;");
    layout->addWidget(mark, 0, Qt::AlignHCenter);

    layout->addSpacing(ScaleHeightForDPI(14));

    QLabel* credit = new QLabel(
        QString("<div align=\"center\">"
                "<span style=\"color:%1; font-style:italic; font-size:%2px;\">%3</span><br>"
                "<span style=\"color:%4; font-size:%5px;\">%6</span></div>")
            .arg(dim)
            .arg(ScaleHeightForDPI(17))
            .arg(tr("created by"))
            .arg(accent)
            .arg(ScaleHeightForDPI(27))
            .arg(tr("Sam Aaron")),
        this);
    credit->setTextFormat(Qt::RichText);
    credit->setAlignment(Qt::AlignHCenter);
    credit->setStyleSheet("background: transparent;");
    layout->addWidget(credit, 0, Qt::AlignHCenter);

    layout->addSpacing(ScaleHeightForDPI(30));

    // Only the strapline words animate: each starts hidden and pops in on
    // its beat.
    auto enters = [&](QWidget* w, int delayMs) {
        QGraphicsOpacityEffect* effect = new QGraphicsOpacityEffect(w);
        effect->setOpacity(0.0);
        w->setGraphicsEffect(effect);
        m_introTargets.append({ w, delayMs });
    };

    QWidget* tagline = new QWidget(this);
    tagline->setStyleSheet("background: transparent;");
    QHBoxLayout* taglineLayout = new QHBoxLayout(tagline);
    taglineLayout->setContentsMargins(0, 0, 0, 0);
    taglineLayout->setSpacing(ScaleHeightForDPI(14));
    const QStringList words = { "Code", "Music", "Live" };
    for (int i = 0; i < words.size(); ++i)
    {
        QLabel* word = new QLabel(
            QString("%1<span style=\"color: %2;\">.</span>").arg(words[i]).arg(accent), tagline);
        word->setTextFormat(Qt::RichText);
        word->setStyleSheet(QString("color: %1; background: transparent; font-family: Hack;"
                                    " font-size: %2px; font-style: italic; font-weight: 700;")
                                .arg(fg)
                                .arg(ScaleHeightForDPI(40)));
        taglineLayout->addWidget(word);
        enters(word, 800 * (i + 1));
    }
    layout->addWidget(tagline, 0, Qt::AlignHCenter);

    layout->addStretch(2);

    QLabel* thanks = new QLabel(
        QString("<div align=\"center\">"
                "<span style=\"color:%1; font-style:italic; font-size:%2px;\">%3</span><br>"
                "<span style=\"color:%4; font-size:%2px;\">%5</span></div>")
            .arg(dim)
            .arg(ScaleHeightForDPI(17))
            .arg(tr("Love and thanks to all the kind people<br>"
                    "who supported this release on Patreon:"))
            .arg(accent)
            .arg("https://patreon.com/samaaron"),
        this);
    thanks->setTextFormat(Qt::RichText);
    thanks->setAlignment(Qt::AlignHCenter);
    thanks->setStyleSheet("background: transparent;");
    layout->addWidget(thanks, 0, Qt::AlignHCenter);

    // Positioned by hand in showEvent; sits outside the centred layout.
    m_version = new QLabel(
        QString("<div align=\"right\">"
                "<span style=\"color:%1; font-size:%2px;\">%3</span><br>"
                "<span style=\"color:%4; font-size:%5px; font-weight:700;\">%6</span></div>")
            .arg(dim)
            .arg(ScaleHeightForDPI(18))
            .arg(tr("Version"))
            .arg(accent)
            .arg(ScaleHeightForDPI(24))
            .arg(SONIC_PI_VERSION),
        this);
    m_version->setTextFormat(Qt::RichText);
    m_version->setStyleSheet("background: transparent;");

    m_poweredBy = new QWidget(this);
    m_poweredBy->setStyleSheet("background: transparent;");
    QVBoxLayout* poweredLayout = new QVBoxLayout(m_poweredBy);
    poweredLayout->setContentsMargins(0, 0, 0, 0);
    poweredLayout->setSpacing(ScaleHeightForDPI(4));
    QLabel* poweredLabel = new QLabel(tr("powered by"), m_poweredBy);
    poweredLabel->setStyleSheet(QString("color:%1; background: transparent;"
                                        " font-style:italic; font-size:%2px;")
                                    .arg(dim)
                                    .arg(ScaleHeightForDPI(15)));
    poweredLayout->addWidget(poweredLabel, 0, Qt::AlignLeft);
    QLabel* ssMark = new QLabel(m_poweredBy);
    QPixmap ssLogo(":/images/supersonic-dark.png");
    const int ssW = ScaleHeightForDPI(168);
    if (ssLogo.width() > ssW * dpr)
        ssLogo = ssLogo.scaledToWidth(qRound(ssW * dpr), Qt::SmoothTransformation);
    ssLogo.setDevicePixelRatio(dpr);
    ssMark->setPixmap(ssLogo);
    ssMark->setStyleSheet("background: transparent;");
    poweredLayout->addWidget(ssMark, 0, Qt::AlignLeft);
}

void SplashWidget::showEvent(QShowEvent* event)
{
    QWidget::showEvent(event);

    const int mx = ScaleHeightForDPI(40);
    const int my = ScaleHeightForDPI(30);

    if (m_version)
    {
        m_version->adjustSize();
        m_version->move(width() - m_version->width() - mx,
                        height() - m_version->height() - my);
    }

    if (m_poweredBy)
    {
        m_poweredBy->adjustSize();
        m_poweredBy->move(mx, height() - m_poweredBy->height() - my);
    }

    // Frameless windows aren't auto-centred.
    if (QScreen* s = screen())
        move(s->geometry().center() - rect().center());
}

void SplashWidget::paintEvent(QPaintEvent*)
{
    QPainter p(this);
    p.fillRect(rect(), kStageBg);
}

void SplashWidget::startAnimation(bool reduceMotion)
{
    if (m_introStarted)
        return;
    m_introStarted = true;
    m_reduceMotion = reduceMotion;

    if (m_reduceMotion)
    {
        finishIntro();
        return;
    }

    // Snap, not fade: an opacity fade rasterises through the graphics effect
    // every frame and stutters when boot hitches the event loop.
    for (const auto& target : m_introTargets)
    {
        QWidget* w = target.first;
        // The default CoarseTimer fudges intervals by up to 5%, which makes
        // the gaps between words visibly uneven.
        QTimer::singleShot(target.second, Qt::PreciseTimer, this, [this, w] {
            if (m_introDone)
                return;
            w->setGraphicsEffect(nullptr);
        });
    }
    const int last = m_introTargets.isEmpty() ? 0 : m_introTargets.last().second;
    QTimer::singleShot(last + 100, this, &SplashWidget::finishIntro);
}

int SplashWidget::introDurationMs() const
{
    if (m_introTargets.isEmpty())
        return 0;
    return m_introTargets.last().second + 150;
}

void SplashWidget::finishIntro()
{
    if (m_introDone)
        return;
    m_introDone = true;
    for (const auto& target : m_introTargets)
        target.first->setGraphicsEffect(nullptr);
}

void SplashWidget::finishAndClose()
{
    finishIntro();
    if (m_reduceMotion)
    {
        close();
        return;
    }
    QPropertyAnimation* fade = new QPropertyAnimation(this, "windowOpacity", this);
    fade->setDuration(240);
    fade->setStartValue(1.0);
    fade->setEndValue(0.0);
    fade->setEasingCurve(QEasingCurve::InCubic);
    connect(fade, &QPropertyAnimation::finished, this, &QWidget::close);
    fade->start(QAbstractAnimation::DeleteWhenStopped);
}
