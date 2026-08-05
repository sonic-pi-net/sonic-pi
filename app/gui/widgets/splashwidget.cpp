//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#include "splashwidget.h"

#include <cmath>

#include <QConicalGradient>
#include <QGraphicsOpacityEffect>
#include <QGuiApplication>
#include <QHBoxLayout>
#include <QLabel>
#include <QPainter>
#include <QPainterPath>
#include <QPixmap>
#include <QPropertyAnimation>
#include <QScreen>
#include <QSvgRenderer>
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
    : QWidget(parent, Qt::Window | Qt::FramelessWindowHint | Qt::WindowStaysOnTopHint
                          | Qt::NoDropShadowWindowHint)
{
    setAttribute(Qt::WA_DeleteOnClose);
    setAttribute(Qt::WA_TranslucentBackground);
    // Never let closing the splash quit the app. Qt clears this for
    // Qt::SplashScreen windows automatically, but this is a Qt::Window (the
    // splash type can't be translucent on macOS), so it counts towards
    // last-window accounting unless we say otherwise. Boot hides the main
    // window, so without this the splash's own closure quits Sonic Pi —
    // taking down any boot-error dialog already on screen, since
    // QCoreApplication::exit unwinds nested event loops too (#3555).
    setAttribute(Qt::WA_QuitOnClose, false);
    // The splash's visible text is brand English by design; only this screen
    // reader announcement is translated. Construction happens before the
    // translator is installed, so MainWindow re-resolves it via retranslate()
    // once translations are loaded.
    setAccessibleName(tr("Sonic Pi is starting"));
    // A generous stage so the content floats in space, clamped so smaller
    // displays still see the whole splash with a margin around it.
    QSize stage(ScaleHeightForDPI(1160), ScaleHeightForDPI(760));
    if (QScreen* s = QGuiApplication::primaryScreen())
    {
        const QSize avail = s->availableGeometry().size();
        const qreal fit = qMin(1.0, qMin(avail.width() * 0.9 / stage.width(),
                                         avail.height() * 0.9 / stage.height()));
        stage *= fit;
    }
    setFixedSize(stage);

    const QString accent = kAccent.name();
    const QString fg = kStageFg.name();
    const QString dim = kStageDim.name();

    QVBoxLayout* layout = new QVBoxLayout(this);
    const int pad = ScaleHeightForDPI(68);
    layout->setContentsMargins(pad, pad, pad, pad);
    layout->setSpacing(0);
    // Slightly top-weighted (2:3 against the bottom stretch): the logo rides
    // a little high, opening space beneath the tagline.
    layout->addStretch(2);

    // The square logo, rendered from vector at the exact device resolution
    // and used as a mask: the tile silhouette is filled with the accent and
    // the glyphs are punched through to the stage behind. In the source SVG
    // the tile is black and the glyphs are white, so a pixel's whiteness is
    // how strongly it is punched out.
    QLabel* mark = new QLabel(this);
    const qreal dpr = devicePixelRatioF();
    const int markW = ScaleHeightForDPI(280);
    QSvgRenderer svg(QStringLiteral(":/images/logo-square.svg"));
    const QSize logical = svg.defaultSize();
    const QSize px(qRound(markW * dpr),
                   qRound(markW * dpr * qreal(logical.height()) / logical.width()));
    QImage src(px, QImage::Format_ARGB32_Premultiplied);
    src.fill(Qt::transparent);
    {
        QPainter sp(&src);
        sp.setRenderHint(QPainter::Antialiasing);
        svg.render(&sp);
    }
    QImage tinted(px, QImage::Format_ARGB32);
    QColor fill = kAccent;
    for (int y = 0; y < src.height(); ++y)
    {
        for (int x = 0; x < src.width(); ++x)
        {
            const QColor s = src.pixelColor(x, y);
            fill.setAlphaF(s.alphaF() * (1.0 - s.valueF()));
            tinted.setPixelColor(x, y, fill);
        }
    }
    QPixmap logo = QPixmap::fromImage(tinted);
    logo.setDevicePixelRatio(dpr);
    mark->setPixmap(logo);
    mark->setStyleSheet("background: transparent;");
    layout->addWidget(mark, 0, Qt::AlignHCenter);

    layout->addSpacing(ScaleHeightForDPI(22));

    QLabel* credit = new QLabel(
        QString("<div align=\"center\">"
                "<span style=\"color:%1; font-style:italic; font-size:%2px;\">%3</span><br>"
                "<span style=\"color:%4; font-size:%5px;\">%6</span></div>")
            .arg(dim)
            .arg(ScaleHeightForDPI(21))
            .arg(QStringLiteral("created by"))
            .arg(accent)
            .arg(ScaleHeightForDPI(33))
            .arg(QStringLiteral("Sam Aaron")),
        this);
    credit->setTextFormat(Qt::RichText);
    credit->setAlignment(Qt::AlignHCenter);
    credit->setStyleSheet("background: transparent;");
    layout->addWidget(credit, 0, Qt::AlignHCenter);

    layout->addSpacing(ScaleHeightForDPI(46));

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
                                .arg(ScaleHeightForDPI(48)));
        taglineLayout->addWidget(word);
        enters(word, 800 * (i + 1));
    }
    layout->addWidget(tagline, 0, Qt::AlignHCenter);

    layout->addStretch(3);

    QLabel* thanks = new QLabel(
        QString("<div align=\"center\">"
                "<span style=\"color:%1; font-style:italic; font-size:%2px;\">%3</span><br>"
                "<span style=\"color:%4; font-size:%2px;\">%5</span></div>")
            .arg(dim)
            .arg(ScaleHeightForDPI(21))
            .arg(QStringLiteral("Love and thanks to all the kind people<br>"
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
            .arg(ScaleHeightForDPI(22))
            .arg(QStringLiteral("Version"))
            .arg(accent)
            .arg(ScaleHeightForDPI(29))
            .arg(SONIC_PI_VERSION),
        this);
    m_version->setTextFormat(Qt::RichText);
    m_version->setStyleSheet("background: transparent;");

    m_poweredBy = new QWidget(this);
    m_poweredBy->setStyleSheet("background: transparent;");
    QVBoxLayout* poweredLayout = new QVBoxLayout(m_poweredBy);
    poweredLayout->setContentsMargins(0, 0, 0, 0);
    poweredLayout->setSpacing(ScaleHeightForDPI(4));
    QLabel* poweredLabel = new QLabel(QStringLiteral("powered by"), m_poweredBy);
    poweredLabel->setStyleSheet(QString("color:%1; background: transparent;"
                                        " font-style:italic; font-size:%2px;")
                                    .arg(dim)
                                    .arg(ScaleHeightForDPI(19)));
    poweredLayout->addWidget(poweredLabel, 0, Qt::AlignLeft);
    QLabel* ssMark = new QLabel(m_poweredBy);
    QPixmap ssLogo(":/images/supersonic-dark.png");
    const int ssW = ScaleHeightForDPI(196);
    if (ssLogo.width() > ssW * dpr)
        ssLogo = ssLogo.scaledToWidth(qRound(ssW * dpr), Qt::SmoothTransformation);
    ssLogo.setDevicePixelRatio(dpr);
    ssMark->setPixmap(ssLogo);
    ssMark->setStyleSheet("background: transparent;");
    poweredLayout->addWidget(ssMark, 0, Qt::AlignLeft);

    // Started by startAnimation(); until then the border is a static line.
    // Repaints only the edge strips so the labels never re-rasterise.
    m_borderTimer = new QTimer(this);
    m_borderTimer->setInterval(16);
    connect(m_borderTimer, &QTimer::timeout, this, [this] {
        // Unhurried lap: fast enough to read as alive, slow enough to feel
        // deliberate rather than spinning for attention.
        constexpr qreal kRevolutionMs = 5200.0;
        m_borderPos = std::fmod(m_borderClock.elapsed() / kRevolutionMs, 1.0);
        const int t = ScaleHeightForDPI(9);
        update(QRegion(0, 0, width(), t)
               + QRegion(0, height() - t, width(), t)
               + QRegion(0, 0, t, height())
               + QRegion(width() - t, 0, t, height()));
    });
}

void SplashWidget::retranslate()
{
    // Construction runs before the translator is installed, so the screen
    // reader announcement resolves to English there; this re-resolves it
    // against the loaded translations. The visible text is brand English by
    // design and stays untouched.
    setAccessibleName(tr("Sonic Pi is starting"));
}

void SplashWidget::showEvent(QShowEvent* event)
{
    QWidget::showEvent(event);

    const int mx = ScaleHeightForDPI(52);
    const int my = ScaleHeightForDPI(40);

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
    p.setRenderHint(QPainter::Antialiasing);

    // Sit the stroke by the GLOW width, not the core: the wide pass reaches
    // furthest, and it is what decides where the pink ends. Overshoot the
    // stage by a hair so the clip trims the glow on the curve — inset it fully
    // and a sliver of bare stage shows outside the pink instead.
    const qreal glowWidth = ScaleHeightForDPI(7);
    const qreal coreWidth = ScaleHeightForDPI(2);
    const qreal inset = glowWidth / 2.0 - ScaleHeightForDPI(1);
    const qreal radius = ScaleHeightForDPI(kRadiusWindowDx);
    const QRectF border = QRectF(rect()).adjusted(inset, inset, -inset, -inset);
    const qreal borderRadius = radius - inset;

    QPainterPath stage;
    stage.addRoundedRect(QRectF(rect()), radius, radius);
    p.fillPath(stage, kStageBg);
    // The glow is wider than the inset, so its outer half falls off the stage.
    // Clipping to the silhouette cuts that bleed on the curve; against the raw
    // widget rect it would keep a square corner outside the rounded core line.
    p.setClipPath(stage);
    p.setBrush(Qt::NoBrush);

    if (!m_borderTimer || !m_borderTimer->isActive())
    {
        // Static thin border: pre-intro, and the reduce-motion stand-in.
        QColor line = kAccent;
        line.setAlpha(110);
        p.setPen(QPen(line, ScaleHeightForDPI(1)));
        p.drawRoundedRect(border, borderRadius, borderRadius);
        return;
    }

    // Only a segment of the border is lit: a conical gradient that is
    // transparent everywhere except around the glow head, swept by rotating
    // the gradient itself. Angle 0 is at 3 o'clock and grows anticlockwise,
    // so start at the top and subtract to travel clockwise.
    QConicalGradient sweep(border.center(), 90.0 - m_borderPos * 360.0);
    auto accent = [](int alpha) {
        QColor c = kAccent;
        c.setAlpha(alpha);
        return c;
    };
    sweep.setColorAt(0.0, accent(255));
    sweep.setColorAt(0.045, accent(120));
    sweep.setColorAt(0.12, accent(0));
    sweep.setColorAt(0.88, accent(0));
    sweep.setColorAt(0.955, accent(120));
    sweep.setColorAt(1.0, accent(255));

    // Two strokes: a wide translucent pass for the glow bleed, then the
    // thin bright core on top.
    p.setOpacity(0.4);
    p.setPen(QPen(QBrush(sweep), glowWidth));
    p.drawRoundedRect(border, borderRadius, borderRadius);
    p.setOpacity(1.0);
    p.setPen(QPen(QBrush(sweep), coreWidth));
    p.drawRoundedRect(border, borderRadius, borderRadius);
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

    m_borderClock.start();
    m_borderTimer->start();

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
