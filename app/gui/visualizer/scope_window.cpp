//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright (C) 2016 by Adrian Cheater
// All rights reserved.
//
// A new/cleaner scope window, using the API
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++



#include <QDebug>
#include <QEnterEvent>
#include <QIcon>
#include <QKeySequence>
#include <QPaintEvent>
#include <QPainter>
#include <QResizeEvent>
#include <QSvgRenderer>
#include <QTimer>
#include <QToolButton>
#include <QVBoxLayout>

#include <algorithm>
#include <cmath>
#include <set>

#include "dpi.h"
#include "utils/framepacer.h"

#include "qt_api_client.h"
#include "utils/tablericons.h"

#include "scope_window.h"

namespace SonicPi
{

namespace
{
const int LissajousSamples = 1024;
const int PenWidth = 1;
const float FFTDecibelRange = 70.0f;
// Once silent, repaint for this many more frames so the phosphor trail can
// fully fade, then stop repainting until real signal returns. ~20 frames
// (~1/3 s at 60fps) is enough at the current decay rate.
const int SilentSettleFrames = 20;
// Bottom of the Levels meter's scale. Shared with LevelsRunningDown so
// "settled" means exactly "drawn at zero width" — a threshold above the
// floor would freeze the pane with a stub of bar still lit.
const float LevelFloorDb = -60.0f;
// Scope slot the mixer taps its limiter input into. Must match
// pre_limiter_scope_num in etc/synthdefs/designs/supercollider/mixer.scd.
// The top slot deliberately: user code picks scope_num: from the bottom
// (scope_out defaults to 1), so a reservation up here won't be walked over.
const unsigned int PreLimiterScopeSlot = 31;
} // namespace

// A small pause/resume control floating in the scope's top-right corner: a
// tabler pause glyph while the trace runs, a play glyph while it's frozen.
// Freezing is useful in itself — it holds a waveform or spectrum snapshot
// still for inspection. Flat like the help-close and zoom glyphs: muted at
// rest, accent on hover, no backdrop. Custom-painted (no MOC: no new
// signals), keyboard-reachable like ChevronButton.
class ScopePauseButton : public QToolButton
{
public:
    explicit ScopePauseButton(QWidget* parent = nullptr)
        : QToolButton(parent)
    {
        setCursor(Qt::PointingHandCursor);
        // TabFocus: keyboard-reachable without clicks stealing editor focus
        setFocusPolicy(Qt::TabFocus);
    }

    void setPaused(bool p)
    {
        if (m_paused != p)
        {
            m_paused = p;
            update();
        }
    }

    void setColors(const QColor& rest, const QColor& hover)
    {
        m_rest = rest;
        m_hover = hover;
        update();
    }

protected:
    void enterEvent(QEnterEvent*) override { update(); }
    void leaveEvent(QEvent*) override { update(); }

    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing, true);
        const bool hover = underMouse();

        // The glyph flip (bars<->triangle) alone signals paused/running — the
        // colour shift is a control affordance, not a status light, and the
        // trace's own motion (moving vs frozen) disambiguates the state.
        const QColor fg = hover ? m_hover : m_rest;
        const auto glyph = m_paused ? TablerIcons::Glyph::PlayFilled
                                    : TablerIcons::Glyph::PauseFilled;
        QSvgRenderer svg(TablerIcons::svgMarkup(glyph, fg).toUtf8());
        // Same 26px logical glyph size as the title-row zoom/close icons
        // (the button itself is 30).
        const qreal gs = qMin(width(), height()) * (26.0 / 30.0);
        svg.render(&p, QRectF((width() - gs) / 2.0, (height() - gs) / 2.0, gs, gs));

        if (hasFocus())
        {
            p.setRenderHint(QPainter::Antialiasing, false);
            p.setPen(QPen(fg, 1));
            p.setBrush(Qt::NoBrush);
            p.drawRect(rect().adjusted(0, 0, -1, -1));
        }
    }

private:
    bool m_paused = false;
    QColor m_rest{ Qt::gray };
    QColor m_hover{ Qt::white };
};

QList<ScopeWindow*> ScopeWindow::s_instances;

ScopeWindow::ScopeWindow(std::shared_ptr<QtAPIClient> spClient, std::shared_ptr<SonicPiAPI> spAPI, QWidget* parent)
    : QWidget(parent)
    , m_spClient(spClient)
    , m_spAPI(spAPI)
{
    s_instances.append(this);
    // Raster widget: we repaint every pixel ourselves and rely on the backing
    // store being preserved between frames, so the faded background clear in
    // paintEvent leaves a decaying trace (phosphor persistence). Not a
    // QOpenGLWidget — a GL widget would force the whole window onto Qt's RHI
    // texturized-composition flush path (a full backing-store copy per frame).
    setAttribute(Qt::WA_OpaquePaintEvent);

    QVBoxLayout* layout = new QVBoxLayout();
    layout->setContentsMargins(0, 0, 0, 0);
    setLayout(layout);

    // Force the audio to contain at least one sample; since the panels currently expect it!
    auto seed = std::make_shared<ProcessedAudio>();
    for (int i = 0; i < 2; i++)
    {
        seed->m_monoSamples.push_back(0);
        seed->m_samples[i].push_back(0);
        seed->m_spectrumQuantized[i].push_back(0);
        seed->m_spectrumPeaks[i].push_back(0);
    }
    m_audio = seed;

    // Settle watchdog (see SettleTick). Interval sits above the ~60Hz frame
    // spacing so it never fires while frames are flowing.
    m_settleTimer = new QTimer(this);
    m_settleTimer->setInterval(33);
    connect(m_settleTimer, &QTimer::timeout, this, &ScopeWindow::SettleTick);

    // Paced repaints: audio frames only mark the scope dirty; the actual
    // update() lands on the shared tick (see requestFrameRepaint).
    connect(FramePacer::instance(), &FramePacer::tick, this, [this] {
        if (m_repaintPending)
        {
            m_repaintPending = false;
            update();
        }
        else if (m_pacerHeld)
        {
            // One idle tick with nothing to draw: let the pacer stop.
            m_pacerHeld = false;
            FramePacer::instance()->release();
        }
    });

    m_levelClock.start();
    m_panels.push_back({ "Levels", tr("Levels"), ScopeWindowType::Levels });
    m_panels.push_back({ "Lissajous", tr("Lissajous"), ScopeWindowType::Lissajous });
    m_panels.push_back({ "Stereo", tr("Left"), ScopeWindowType::Left });
    m_panels.push_back({ "Stereo", tr("Right"), ScopeWindowType::Right });
    m_panels.push_back({ "Mono", tr("Mono"), ScopeWindowType::Mono });
    m_panels.push_back({ "Mirror Stereo", tr("Mirror Stereo"), ScopeWindowType::MirrorStereo });

    ScopeWindowPanel spec({ "Spectrum", tr("Spectrum"), ScopeWindowType::SpectrumAnalysis });
    spec.requireFFT = true;
    m_panels.push_back(spec);

    for (auto& scope : m_panels)
    {
        scope.pen = QPen();
        scope.pen.setWidth(PenWidth);
        scope.pen2 = QPen();
        scope.pen2.setWidth(PenWidth);
    }

    qRegisterMetaType<ProcessedAudioPtr>("SonicPi::ProcessedAudioPtr");
    connect(m_spClient.get(), &QtAPIClient::ConsumeAudioData, this, &ScopeWindow::OnConsumeAudioData);

    m_pauseButton = new ScopePauseButton(this);
    m_pauseButton->setFixedSize(ScaleForDPI(30, 30));
    m_pauseButton->setToolTip(tr("Pause or resume the audio oscilloscopes. Pausing freezes the current image so you can inspect it."));
    m_pauseButton->setProperty("tipShortcut", QKeySequence("F12").toString(QKeySequence::NativeText));
    m_pauseButton->setAccessibleName(tr("Pause scopes"));
    connect(m_pauseButton, &QToolButton::clicked, this, &ScopeWindow::TogglePause);
    connect(this, &ScopeWindow::PausedChanged, m_pauseButton, [this](bool paused) {
        m_pauseButton->setPaused(paused);
        m_pauseButton->setAccessibleName(paused ? tr("Resume scopes") : tr("Pause scopes"));
    });

    Layout();
}

ScopeWindow::~ScopeWindow()
{
    if (m_pacerHeld)
        FramePacer::instance()->release();
    s_instances.removeAll(this);
}

void ScopeWindow::requestFrameRepaint()
{
    m_repaintPending = true;
    if (!m_pacerHeld)
    {
        m_pacerHeld = true;
        FramePacer::instance()->retain();
    }
}

void ScopeWindow::ShutDown()
{

}

void ScopeWindow::resizeEvent(QResizeEvent* pSize)
{
    QWidget::resizeEvent(pSize);

    // The framebuffer is recreated on resize — clear it opaquely next paint.
    m_fullClear = true;
    Layout();
}

QWidget* ScopeWindow::PauseButton() const
{
    return m_pauseButton;
}

// Stereo master meter: an LED ladder per channel following peak level, with
// 0 dBFS at kWallFrac of the run. Past that line the bar's tip continues in
// the hot accent by the measured overdrive — how far the mixer's pre-limiter
// signal exceeds the ceiling, which is what the master limiter takes off.
void ScopeWindow::DrawLevels(const ProcessedAudio& audio, QPainter& painter, ScopeWindowPanel& panel)
{
    static const float kFloorDb = LevelFloorDb;
    static const float kRmsAttackTauMs = 60.0f;
    static const float kRmsReleaseTauMs = 300.0f;
    static const float kPeakFallDbPerSec = 26.0f;
    static const float kHoldMs = 1500.0f;
    static const float kHoldFallDbPerSec = 36.0f;

    const auto toDb = [](float v) {
        return (v <= 1e-5f) ? kFloorDb : std::max(kFloorDb, 20.0f * std::log10(v));
    };
    // Fraction of the bar at which 0 dBFS sits; the run beyond it is the
    // overdrive zone.
    static const float kWallFrac = 0.78f;
    // Piecewise up to the wall: -60..-18 dB compressed into 45% of the run,
    // the top 18 dB (where level decisions live) gets the rest.
    const auto dbToFrac = [](float db) {
        db = std::min(0.0f, std::max(kFloorDb, db));
        if (db <= -18.0f)
            return kWallFrac * 0.45f * (db - kFloorDb) / (-18.0f - kFloorDb);
        return kWallFrac * (0.45f + 0.55f * (db + 18.0f) / 18.0f);
    };

    const qint64 now = m_levelClock.elapsed();
    qint64 dtClamped = now - m_levelLastMs;
    if (dtClamped < 0) dtClamped = 0;
    if (dtClamped > 200) dtClamped = 200;
    const float dtMs = float(dtClamped);
    m_levelLastMs = now;

    bool snapshotSilent = true;   // this frame's audio, not the ballistics
    for (int ch = 0; ch < 2; ch++)
    {
        const std::vector<float>& s = audio.m_samples[ch];
        float framePeak = 0.0f;
        double sumSq = 0.0;
        for (float v : s)
        {
            framePeak = std::max(framePeak, std::fabs(v));
            sumSq += double(v) * double(v);
        }
        const float rmsTarget = s.empty() ? kFloorDb : toDb(float(std::sqrt(sumSq / double(s.size()))));
        const float peakTarget = s.empty() ? kFloorDb : toDb(framePeak);
        if (peakTarget > kFloorDb + 0.5f)
            snapshotSilent = false;

        LevelChannel& c = m_levels[ch];
        const float tau = (rmsTarget > c.rmsDb) ? kRmsAttackTauMs : kRmsReleaseTauMs;
        c.rmsDb += (rmsTarget - c.rmsDb) * (1.0f - std::exp(-dtMs / tau));
        c.peakDb = std::max(kFloorDb, c.peakDb - kPeakFallDbPerSec * dtMs / 1000.0f);
        if (peakTarget > c.peakDb)
            c.peakDb = peakTarget;   // instant attack
        if (c.peakDb >= c.holdDb)
        {
            c.holdDb = c.peakDb;
            c.holdUntilMs = now + qint64(kHoldMs);
        }
        else if (now > c.holdUntilMs)
        {
            c.holdDb = std::max(kFloorDb, c.holdDb - kHoldFallDbPerSec * dtMs / 1000.0f);
        }
        // The RMS release is exponential and never quite reaches the floor;
        // snap the last half-dB shut or a sliver stays lit once the pane
        // stops repainting.
        const float kSnapDb = 0.5f;
        if (c.rmsDb < kFloorDb + kSnapDb) c.rmsDb = kFloorDb;
        if (c.peakDb < kFloorDb + kSnapDb) c.peakDb = kFloorDb;
        if (c.holdDb < kFloorDb + kSnapDb) c.holdDb = kFloorDb;
    }

    // The mixer's pre-limiter tap. Re-fetch whenever the reader is invalid:
    // the audio processor attaches asynchronously (and reattaches after a
    // cold swap), and on the constrained memory profiles the tap slot
    // doesn't exist at all — then the tip simply never lights.
    if (m_spAPI && !m_preLimReader.valid())
    {
        m_preLimReader = m_spAPI->AudioProcessor_GetScopeReader(PreLimiterScopeSlot);
        m_preLimLastEnd = 0;
    }
    if (m_spAPI && m_preLimReader.valid())
    {
        const uint64_t end = m_spAPI->AudioProcessor_GetSampleClock().audible_end(m_preLimReader);
        if (end != m_preLimLastEnd)
        {
            m_preLimLastEnd = end;
            const uint32_t frames = 1024;
            m_grScratch.resize(size_t(frames) * SHM_SCOPE_STREAM_CHANNELS);
            uint32_t chans = 1;
            const uint32_t got = m_preLimReader.copy_window(end, frames, m_grScratch.data(), &chans);
            float p = 0.0f;
            for (size_t i = 0; i < size_t(got) * chans; i++)
                p = std::max(p, std::fabs(m_grScratch[i]));
            m_grTargetDb = std::min(12.0f, std::max(0.0f, toDb(p)));
            m_grAvailable = true;
        }
    }

    if (m_grAvailable)
    {
        // No fresh windows arrive once the engine pauses on silence, but the
        // settle repaints keep coming — run the overdrive down with them
        // rather than freezing mid-clamp.
        const float target = snapshotSilent ? 0.0f : m_grTargetDb;
        // Instant rise; falls at the same rate as the peak bar so tip and
        // bar recede together.
        m_grDb = std::max(target, m_grDb - kPeakFallDbPerSec * dtMs / 1000.0f);
        // Reduction implies the output is pinned at the ceiling — clear the
        // tip once the level has fallen away from it.
        if (std::max(m_levels[0].peakDb, m_levels[1].peakDb) < -1.0f)
            m_grDb = 0.0f;
        if (m_grDb < 0.3f)
            m_grDb = 0.0f;
    }

    // Opt out of the pane's phosphor persistence: a meter must not leave
    // decaying ghosts, so clear the strip opaquely every frame.
    painter.fillRect(panel.rc, m_backColor);

    // Level in the second scope colour; the hot accent is reserved for
    // overdrive, so it always means limiting rather than just loud.
    const QColor level = panel.pen2.color();
    const QColor neutral = QWidget::palette().color(QWidget::foregroundRole());
    const QRect& g = panel.rcGraph;

    const QRectF bars(g);
    const qreal barH = std::min(qreal(ScaleHeightForDPI(7)), bars.height() * 0.2);
    const qreal gap = std::max<qreal>(ScaleHeightForDPI(5), barH * 0.6);
    const qreal top0 = bars.center().y() - gap / 2 - barH;

    // Segment geometry: discrete cells, like a hardware bargraph.
    const qreal cellW = ScaleWidthForDPI(4);
    const qreal cellGap = std::max<qreal>(1.0, ScaleWidthForDPI(2));
    const int cells = std::max(1, int((bars.width() + cellGap) / (cellW + cellGap)));

    // +12 dB of overdrive spans the whole zone past the line.
    const qreal overEnd = kWallFrac
        + (m_grAvailable ? std::min<qreal>(1.0, m_grDb / 12.0) : 0.0) * (1.0 - kWallFrac);

    painter.setRenderHint(QPainter::Antialiasing, true);
    for (int ch = 0; ch < 2; ch++)
    {
        const qreal barTop = top0 + ch * (barH + gap);
        const qreal peakFrac = dbToFrac(m_levels[ch].peakDb);

        painter.setPen(Qt::NoPen);
        for (int i = 0; i < cells; i++)
        {
            const qreal frac = (i + 0.5) / cells;
            const QRectF cell(bars.left() + i * (cellW + cellGap), barTop, cellW, barH);

            const bool overLine = frac > kWallFrac;
            const bool isLit = overLine ? (frac <= overEnd) : (frac <= peakFrac);
            QColor c = overLine ? m_levelHot : level;
            if (isLit)
            {
                QColor glow = c;
                glow.setAlpha(55);
                painter.setBrush(glow);
                painter.drawRect(cell.adjusted(-cellGap * 0.5, -cellGap * 0.5, cellGap * 0.5, cellGap * 0.5));
                painter.setBrush(c);
                painter.drawRect(cell);
            }
            else
            {
                c.setAlpha(overLine ? 22 : 26);   // unlit LED, still a track
                painter.setBrush(c);
                painter.drawRect(cell);
            }
        }
    }

    // dB scale labels when scope labels are on.
    if (panel.titleVisible)
    {
        QColor label = neutral;
        label.setAlpha(140);
        painter.setPen(label);
        QFont f = painter.font();
        SetFontSizeValue(f, FontSizeValue(f) * 0.75);
        painter.setFont(f);
        const qreal labelY = top0 + 2 * barH + gap;
        for (float t : { -18.0f, -6.0f, -3.0f, 0.0f })
        {
            const qreal x = bars.left() + dbToFrac(t) * bars.width();
            const int labelW = ScaleWidthForDPI(40);
            painter.drawText(QRectF(x - labelW / 2.0, labelY, labelW, ScaleHeightForDPI(14)),
                             Qt::AlignHCenter | Qt::AlignTop, QString::number(int(t)));
        }
    }

}

// Draw a Simple Stereo representation with a mirror of right/left stereo
void ScopeWindow::DrawSpectrumAnalysis(const ProcessedAudio& audio, QPainter& painter, ScopeWindowPanel& panel)
{
    if (audio.m_spectrumQuantized[0].empty() || audio.m_spectrumQuantized[1].empty())
        return;

    uint32_t buckets = uint32_t(audio.m_spectrumQuantized[0].size());
    int centerY = panel.rcGraph.center().y();
    float sampleScale = panel.rcGraph.height() / 2.0f;

    panel.waveRects.resize(buckets * 2);

    // Make a pixel margin between the buckets for a cleaner view
    float stepPerRect = std::ceil(panel.rcGraph.width() / float(buckets));
    int margin = ScaleWidthForDPI(1);

    // ... but discard it if we have to
    if (stepPerRect < (margin * 2) + ScaleWidthForDPI(2))
    {
        margin = 0;
    }
    // Make sure we step enough to make a valid rect
    stepPerRect = std::max(margin * 2.0f + 1.0f, stepPerRect);

    // Stereo
    int rightIndex = int(panel.waveRects.size() / 2);
    for (uint32_t index = 0; index < buckets; index++)
    {
        int x1 = index * stepPerRect;

        // Draw left at the top, right at the bottom
        int size = audio.m_spectrumQuantized[0][index] * sampleScale;
        int size2 = audio.m_spectrumQuantized[1][index] * sampleScale;
        size = std::max(1, size);
        size2 = std::max(1, size2);
        panel.waveRects[index] = QRect(x1 + margin, centerY - 1 - size, stepPerRect - margin * 2, size);
        panel.waveRects[index + rightIndex] = QRect(x1 + margin, centerY + 1, stepPerRect - margin * 2, size2);
    }

    // Live bars at full accent; peak-hold markers ghosted behind them
    for (uint32_t index = 0; index < buckets; index++)
    {
        painter.fillRect(panel.waveRects[index], panel.brush);
    }

    for (uint32_t index = 0; index < buckets; index++)
    {
        painter.fillRect(panel.waveRects[rightIndex + index], panel.brush2);
    }

    // Peak-hold markers: a thin dim line at each bucket's recent maximum
    if (audio.m_spectrumPeaks[0].size() == buckets
        && audio.m_spectrumPeaks[1].size() == buckets)
    {
        QColor cTopDim = panel.brush.color();
        cTopDim.setAlphaF(0.4f);
        QColor cBotDim = panel.brush2.color();
        cBotDim.setAlphaF(0.4f);
        int peakH = std::max(1, ScaleHeightForDPI(2));
        for (uint32_t index = 0; index < buckets; index++)
        {
            int x1 = index * stepPerRect;
            int w = int(stepPerRect) - margin * 2;
            int peakTop = int(audio.m_spectrumPeaks[0][index] * sampleScale);
            int peakBot = int(audio.m_spectrumPeaks[1][index] * sampleScale);
            if (peakTop > 1)
            {
                painter.fillRect(QRect(x1 + margin, centerY - 1 - peakTop, w, peakH), cTopDim);
            }
            if (peakBot > 1)
            {
                painter.fillRect(QRect(x1 + margin, centerY + 1 + peakBot - peakH, w, peakH), cBotDim);
            }
        }
    }

    // Frequency ticks on the centre line (only with labels enabled).
    // Bucket k spans equal log-frequency width, so freq -> x is the same
    // log mapping the buckets use.
    if (panel.titleVisible && audio.m_spectrumFreqMax > audio.m_spectrumFreqMin)
    {
        const struct { float freq; const char* text; } ticks[] = {
            { 100.0f, "100" }, { 1000.0f, "1k" }, { 10000.0f, "10k" }
        };
        float logSpan = std::log(audio.m_spectrumFreqMax / audio.m_spectrumFreqMin);
        QColor tickColor = QWidget::palette().color(QWidget::foregroundRole());
        tickColor.setAlphaF(0.6f);
        painter.save();
        painter.setPen(tickColor);
        QFont tickFont = painter.font();
        SetFontSizeValue(tickFont, FontSizeValue(tickFont) * 0.75);
        painter.setFont(tickFont);
        int w = ScaleWidthForDPI(40);
        int h = ScaleHeightForDPI(14);
        for (const auto& tick : ticks)
        {
            float frac = std::log(tick.freq / audio.m_spectrumFreqMin) / logSpan;
            if (frac <= 0.0f || frac >= 1.0f)
                continue;
            int x = panel.rcGraph.left() + int(frac * panel.rcGraph.width());
            painter.drawText(QRect(x - w / 2, centerY - h / 2, w, h),
                             Qt::AlignCenter, tick.text);
        }
        painter.restore();
    }
}

// Draw a Simple Stereo representation with a mirror of right/left stereo
void ScopeWindow::DrawMirrorStereo(const ProcessedAudio& audio, QPainter& painter, ScopeWindowPanel& panel)
{
    int width = panel.rcGraph.width();
    if (width <= 0 || m_audioFrameSamples == 0)
    {
        return;
    }

    // Same zero-crossing trigger as DrawWave (sonic-pi#1330), searched on the
    // raw left channel since the rectified display has no crossings itself.
    uint32_t searchLimit = m_audioFrameSamples / 4;
    uint32_t trigger = 0;
    for (uint32_t i = 1; i < searchLimit; i++)
    {
        if (audio.m_samples[0][i - 1] < 0.0f && audio.m_samples[0][i] >= 0.0f)
        {
            trigger = i;
            break;
        }
    }
    uint32_t visibleSamples = m_audioFrameSamples - searchLimit;
    if (visibleSamples == 0)
    {
        return;
    }

    // Peak envelope per pixel column: every sample in the column's bin is
    // inspected, so transients can't alias away as they did when we
    // point-sampled one-in-N.
    double step = visibleSamples / double(width);

    // One vertical line per column per channel
    panel.waveLines.resize(width * 2);

    float yScale = float(panel.rcGraph.height() / 2.0f);
    int y = panel.rcGraph.center().y();
    int left = panel.rcGraph.left();

    for (int x = 0; x < width; x++)
    {
        uint32_t start = uint32_t(double(x) * step);
        uint32_t end = std::max(start + 1, uint32_t(double(x + 1) * step));
        end = std::min(end, visibleSamples);

        float peakLeft = 0.0f;
        float peakRight = 0.0f;
        for (uint32_t i = start; i < end; i++)
        {
            peakLeft = std::max(peakLeft, std::abs(audio.m_samples[0][trigger + i]));
            peakRight = std::max(peakRight, std::abs(audio.m_samples[1][trigger + i]));
        }

        int xCoord = left + x;
        panel.waveLines[x] = QLine(xCoord, y, xCoord, int(peakLeft * yScale + y + 1));
        panel.waveLines[width + x] = QLine(xCoord, y, xCoord, int(-peakRight * yScale + y - 1));
    }
    painter.setPen(panel.pen2);
    painter.drawLines(&panel.waveLines[0], width);
    painter.setPen(panel.pen);
    painter.drawLines(&panel.waveLines[width], width);
}

// Draw a Simple Wave
void ScopeWindow::DrawWave(const ProcessedAudio& audio, QPainter& painter, ScopeWindowPanel& panel)
{
    int width = panel.rcGraph.width();
    if (width <= 0 || m_audioFrameSamples == 0)
    {
        return;
    }

    const float* pSamples = nullptr;
    switch (panel.type)
    {
    case ScopeWindowType::Left:
        pSamples = &audio.m_samples[0][0];
        break;
    case ScopeWindowType::Right:
        pSamples = &audio.m_samples[1][0];
        break;
    case ScopeWindowType::Mono:
        pSamples = &audio.m_monoSamples[0];
        break;
    default:
        break;
    }

    if (pSamples == nullptr)
    {
        return;
    }

    // Oscilloscope-style trigger (sonic-pi#1330): the first quarter of the
    // snapshot is a search margin, and the visible window starts at the first
    // rising zero-crossing found in it, so periodic waveforms hold still
    // frame to frame. No crossing (silence, noise) falls back to free-run.
    uint32_t searchLimit = m_audioFrameSamples / 4;
    uint32_t trigger = 0;
    for (uint32_t i = 1; i < searchLimit; i++)
    {
        if (pSamples[i - 1] < 0.0f && pSamples[i] >= 0.0f)
        {
            trigger = i;
            break;
        }
    }
    pSamples += trigger;
    uint32_t visibleSamples = m_audioFrameSamples - searchLimit;
    if (visibleSamples == 0)
    {
        return;
    }

    // Min/max envelope per pixel column (the classic scope/wave-editor
    // rendering): stable image, no aliasing of fast transients.
    double step = visibleSamples / double(width);
    panel.waveLines.resize(width);

    float yScale = float(panel.rcGraph.height() / 2.0f);
    int y = panel.rcGraph.center().y();
    int left = panel.rcGraph.left();

    for (int x = 0; x < width; x++)
    {
        uint32_t start = uint32_t(double(x) * step);
        uint32_t end = std::max(start + 1, uint32_t(double(x + 1) * step));
        end = std::min(end, visibleSamples);

        float lo = pSamples[start];
        float hi = lo;
        for (uint32_t i = start + 1; i < end; i++)
        {
            lo = std::min(lo, pSamples[i]);
            hi = std::max(hi, pSamples[i]);
        }
        panel.waveLines[x] = QLine(left + x, int(lo * yScale + y),
                                   left + x, int(hi * yScale + y) + 1);
    }
    painter.setPen(panel.pen);
    painter.drawLines(&panel.waveLines[0], width);
}

void ScopeWindow::DrawLissajous(const ProcessedAudio& audio, QPainter& painter, ScopeWindowPanel& panel)
{
    float scale = float(std::min(panel.rcGraph.height(), panel.rcGraph.width()) / 2.0f);

    QPoint center = panel.rcGraph.center();

    // Use the newest tail of the rolling window, not the oldest — the
    // figure tracks the live sound instead of lagging ~64ms behind.
    auto samples = std::min(LissajousSamples, int(m_audioFrameSamples));
    int offset = int(m_audioFrameSamples) - samples;
    panel.wavePoints.resize(samples);
    for (int sample = 0; sample < samples; sample++)
    {
        auto left = audio.m_samples[0][offset + sample];
        auto right = audio.m_samples[1][offset + sample];
        panel.wavePoints[sample] = center + QPoint(left * scale, right * scale);
    }
    // The only diagonal-line panel; AA is cheap here and removes the jaggies.
    painter.setRenderHint(QPainter::Antialiasing, true);
    painter.setPen(panel.pen);
    painter.drawPolyline(&panel.wavePoints[0], int(panel.wavePoints.size()));
    painter.setRenderHint(QPainter::Antialiasing, false);
}

void ScopeWindow::paintEvent(QPaintEvent* pEv)
{
    if (size().isEmpty())
        return;

    auto backColor = m_backColor;
    auto textColor = QWidget::palette().color(QWidget::foregroundRole());
    auto shadowColor = QWidget::palette().color(QPalette::ColorRole::AlternateBase);

    // Draw into our own offscreen buffer so the phosphor trail (the faded clear
    // that lets the previous frame linger and decay) survives between frames
    // regardless of whether Qt preserves the widget backing store — it doesn't on
    // macOS, where the translucent clear accumulated over a grey backing and read
    // as window grey. The buffer is blitted to the widget at the end.
    const qreal dpr = devicePixelRatioF();
    const QSize bufPx = size() * dpr;
    if (m_trail.size() != bufPx)
    {
        m_trail = QImage(bufPx, QImage::Format_ARGB32_Premultiplied);
        m_trail.setDevicePixelRatio(dpr);
        m_fullClear = true;
    }

    QPainter painter(&m_trail);
    if (m_fullClear)
    {
        painter.fillRect(rect(), backColor);   // opaque clear on first paint / resize
        m_fullClear = false;
    }
    else
    {
        constexpr int kDecayAlpha = 70;   // higher = faster decay / shorter trail
        QColor fade = backColor;
        fade.setAlpha(kDecayAlpha);
        painter.fillRect(rect(), fade);    // soft decay of the previous frame
    }

    // m_audio is an immutable snapshot; slot + paint share the GUI thread.
    const ProcessedAudio& processedAudio = *m_audio;

    // If we have new data, we have used it here
    if (m_audioAvailable)
    {
        m_spAPI->AudioProcessor_ConsumedAudio();
        m_audioAvailable = false;
    }

    m_audioFrameSamples = uint32_t(processedAudio.m_samples[0].size());

    for (auto& panel : m_panels)
    {
        if (!panel.visible)
        {
            continue;
        }

        if (panel.titleVisible)
        {
            painter.setPen(textColor);
            painter.drawText(panel.rcTitle, Qt::AlignCenter, panel.name);
        }

        if (panel.type == ScopeWindowType::Lissajous)
        {
            DrawLissajous(processedAudio, painter, panel);
        }
        else if (panel.type == ScopeWindowType::Left || panel.type == ScopeWindowType::Right || panel.type == ScopeWindowType::Mono)
        {
            DrawWave(processedAudio, painter, panel);
        }
        else if (panel.type == ScopeWindowType::MirrorStereo)
        {
            DrawMirrorStereo(processedAudio, painter, panel);
        }
        else if (panel.type == ScopeWindowType::SpectrumAnalysis)
        {
            if (!processedAudio.m_spectrumQuantized[0].empty())
            {
                DrawSpectrumAnalysis(processedAudio, painter, panel);
            }
        }
        else if (panel.type == ScopeWindowType::Levels)
        {
            DrawLevels(processedAudio, painter, panel);
        }
    }

    painter.end();
    QPainter widgetPainter(this);
    widgetPainter.drawImage(0, 0, m_trail);
}

void ScopeWindow::Layout()
{
    QRect rc = rect();
    int yMargin = ScaleWidthForDPI(4);
    int yFontMargin = ScaleWidthForDPI(2);
    int xMargin = ScaleHeightForDPI(4);

    int visibleCount = std::count_if(m_panels.begin(), m_panels.end(), [&visibleCount](ScopeWindowPanel& p) { return p.visible; });

    // The Levels meter is a strip, not a scope canvas: it takes a fixed slim
    // height and the remaining panels share what's left.
    const int levelsHeight = ScaleHeightForDPI(44);
    int levelsCount = 0;
    for (auto& p : m_panels)
    {
        if (p.visible && p.type == ScopeWindowType::Levels)
            levelsCount++;
    }
    const int normalCount = visibleCount - levelsCount;

    const int panelWidth = rc.width() - (xMargin * 2);
    int totalHeight = rc.height() - (yMargin * (visibleCount + 1));
    int normalHeight = totalHeight;
    if (normalCount > 0)
        normalHeight = int((totalHeight - levelsCount * levelsHeight) / float(normalCount));

    QSize panelSize = QSize(panelWidth, normalHeight);

    QPoint currentTopLeft(xMargin, yMargin);

    QFontMetrics metrics(qApp->font());

    for (auto& panel : m_panels)
    {
        if (!panel.visible)
        {
            continue;
        }
        const int h = (panel.type == ScopeWindowType::Levels) ? levelsHeight : normalHeight;
        panel.rc = QRect(currentTopLeft, QSize(panelWidth, h));
        panel.rcGraph = panel.rc;
        panel.rcTitle = QRect(currentTopLeft, QSize(panelWidth, 0));

        if (panel.titleVisible)
        {
            panel.rcTitle.setHeight(metrics.height() + yFontMargin * 2);
            panel.rcGraph.setTop(panel.rcTitle.bottom());
        }
        currentTopLeft.setY(currentTopLeft.y() + h + yMargin);
    }

    m_fftBucketsWanted = panelSize.width() / 4;
    ApplyFFTSettings();

    update();
}

std::vector<QString> ScopeWindow::GetScopeCategories() const
{
    std::set<QString> cat;
    for (auto& scope : m_panels)
    {
        cat.insert(scope.category);
    }
    return std::vector<QString>(cat.begin(), cat.end());
}

bool ScopeWindow::EnableScope(const QString& category, bool on)
{
    bool any = false;
    bool shown = false;
    for (auto& scope : m_panels)
    {
        if (scope.category == category)
        {
            shown |= on && !scope.visible;
            scope.visible = on;
            any = true;
        }
    }

    // A newly-shown panel would otherwise draw the stale snapshot left from
    // when it was last visible — and keep drawing it if the engine is silent.
    if (shown)
    {
        ResetToRest();
    }

    ApplyProcessorEnable();
    ApplyFFTSettings();

    Layout();
    Refresh();
    return any ? on : true;
}

bool ScopeWindow::SetScopeLabels(bool on)
{
    for (auto& scope : m_panels)
    {
        scope.titleVisible = on;
    }
    Layout();
    Refresh();
    return on;
}

void ScopeWindow::Booted()
{
    ApplyProcessorEnable();
}

void ScopeWindow::TogglePause()
{
    m_pendingPause = false;
    m_paused = !m_paused;
    ApplyProcessorEnable();
    // Unpausing ends the frozen inspection image; start from rest rather
    // than letting it linger (live signal refills on the next frame anyway).
    if (!m_paused)
    {
        ResetToRest();
    }
    emit PausedChanged(m_paused);
}

void ScopeWindow::Pause()
{
    const bool was = m_paused;
    m_pendingPause = false;
    m_paused = true;
    ApplyProcessorEnable();
    if (!was)
        emit PausedChanged(m_paused);
}

void ScopeWindow::PauseWhenSilent()
{
    if (!m_paused)
    {
        m_pendingPause = true;
    }
}

void ScopeWindow::Resume()
{
    const bool was = m_paused;
    m_pendingPause = false;
    m_paused = false;
    ApplyProcessorEnable();
    if (was)
    {
        ResetToRest();
        emit PausedChanged(m_paused);
    }
}

void ScopeWindow::SetSuspended(bool suspended)
{
    if (m_suspended == suspended)
        return;
    m_suspended = suspended;
    // Coming out of suspension the snapshot is as old as the hide was long;
    // start the re-shown scopes at rest, not at the pre-hide trace.
    if (!suspended && !m_paused)
    {
        ResetToRest();
    }
    ApplyProcessorEnable();
}

bool ScopeWindow::WantsProcessor() const
{
    if (!isVisible())
        return false;
    for (auto& scope : m_panels)
        if (scope.visible)
            return !m_paused && !m_suspended;
    return false;
}

void ScopeWindow::showEvent(QShowEvent* e)
{
    QWidget::showEvent(e);
    ApplyProcessorEnable();
}

void ScopeWindow::hideEvent(QHideEvent* e)
{
    QWidget::hideEvent(e);
    ApplyProcessorEnable();
}

void ScopeWindow::ApplyProcessorEnable()
{
    // AudioProcessor_Enable is one global switch, but there is more than one
    // ScopeWindow: the dock's, and the Levels copy in audio preferences. Each
    // answering only for itself means the last to speak wins, and a suspended
    // dock scope turns the feed off underneath a visible prefs meter. The
    // answer is whether ANY live instance wants it.
    bool wanted = false;
    for (ScopeWindow* w : s_instances)
        if (w->WantsProcessor())
        {
            wanted = true;
            break;
        }
    m_spAPI->AudioProcessor_Enable(wanted);
}

void ScopeWindow::ApplyFFTSettings()
{
    // EnableFFT and SetMaxFFTBuckets are engine-wide, exactly like
    // AudioProcessor_Enable above, so the same rule applies: aggregate over
    // every instance rather than letting the last caller win. Otherwise the
    // narrow Levels meter in audio preferences clamps the bucket count under
    // the dock's Spectrum scope on every prefs-pane resize. FFT runs if any
    // instance shows a panel that needs it, at the widest such ask.
    bool fft = false;
    int buckets = 0;
    for (ScopeWindow* w : s_instances)
    {
        bool wants = false;
        for (auto& panel : w->m_panels)
            if (panel.visible && panel.requireFFT)
            {
                wants = true;
                break;
            }
        if (!wants)
            continue;
        fft = true;
        buckets = qMax(buckets, w->m_fftBucketsWanted);
    }
    m_spAPI->AudioProcessor_EnableFFT(fft);
    if (fft && buckets > 0)
        m_spAPI->AudioProcessor_SetMaxFFTBuckets(buckets);
}

bool ScopeWindow::LevelsRunningDown() const
{
    for (const auto& panel : m_panels)
    {
        if (panel.visible && panel.type == ScopeWindowType::Levels)
        {
            if (m_grDb > 0.0f)
            {
                return true;
            }
            for (int ch = 0; ch < 2; ch++)
            {
                if (m_levels[ch].rmsDb > LevelFloorDb || m_levels[ch].peakDb > LevelFloorDb
                    || m_levels[ch].holdDb > LevelFloorDb)
                {
                    return true;
                }
            }
        }
    }
    return false;
}

// True when the sample window is (audibly) silent and, if a spectrum
// panel is showing, its bars and peak markers have fully decayed.
bool ScopeWindow::SnapshotSilent(const ProcessedAudio& audio) const
{
    const float sampleEps = 1e-4f; // ~-80dB
    for (int ch = 0; ch < 2; ch++)
    {
        for (float s : audio.m_samples[ch])
        {
            if (std::abs(s) > sampleEps)
            {
                return false;
            }
        }
    }

    bool fftVisible = false;
    for (const auto& panel : m_panels)
    {
        if (panel.visible && panel.requireFFT)
        {
            fftVisible = true;
        }
    }
    if (fftVisible)
    {
        const float displayEps = 0.005f;
        for (int ch = 0; ch < 2; ch++)
        {
            for (float v : audio.m_spectrumQuantized[ch])
            {
                if (v > displayEps)
                {
                    return false;
                }
            }
            for (float v : audio.m_spectrumPeaks[ch])
            {
                if (v > displayEps)
                {
                    return false;
                }
            }
        }
    }

    // A visible Levels panel is only settled once its bars, peak-hold and
    // overdrive have run down to the floor — freezing earlier would trap a
    // half-lit meter.
    if (LevelsRunningDown())
    {
        return false;
    }
    return true;
}

void ScopeWindow::Refresh()
{
    update();
}

void ScopeWindow::SetColor(QColor c)
{
    for (auto& scope : m_panels)
    {
        scope.pen.setColor(c);
        scope.brush = QBrush(c);
    }
}

void ScopeWindow::SetColor2(QColor c)
{
    for (auto& scope : m_panels)
    {
        scope.pen2.setColor(c);
        scope.brush2 = QBrush(c);
    }
}

void ScopeWindow::SetLevelHotColour(QColor hot)
{
    m_levelHot = hot;
    update();
}

void ScopeWindow::SetBackgroundColor(QColor c)
{
    m_backColor = c;
    m_fullClear = true;   // repaint the whole area in the new colour
    update();
}

void ScopeWindow::SetPauseButtonColors(QColor rest, QColor hover)
{
    if (m_pauseButton)
        m_pauseButton->setColors(rest, hover);
}

void ScopeWindow::OnConsumeAudioData(SonicPi::ProcessedAudioPtr audio)
{
    if (!m_paused && isVisible())
    {
        const bool silent = !audio || SnapshotSilent(*audio);
        if (audio && !audio->m_samples[0].empty())
        {
            m_audio = audio;
        }
        else if (m_silentFrames < SilentSettleFrames)
        {
            // Silent gaps deliver empty buffers; without substitution the
            // settle repaints re-burn the stale waveform into the fading
            // trail instead of letting it decay to a flat line.
            m_audio = MakeSilentSnapshot();
        }

        m_audioAvailable = true;

        // The audio thread delivers a frame at the refresh rate whether or not
        // there's signal, and repainting every one of them wastes CPU at idle.
        // While there's signal we repaint normally; once the snapshot is silent
        // (samples below ~-80dB and, for a spectrum panel, its bars and peaks
        // decayed) we issue only SilentSettleFrames more repaints to let the
        // phosphor trail fade out, then stop until real signal returns. This
        // slot keeps being called regardless (it only drives painting), so a
        // returning signal resumes the scope on the very next frame.
        if (!silent)
        {
            m_silentFrames = 0;
            requestFrameRepaint();
        }
        else if (m_silentFrames < SilentSettleFrames)
        {
            m_silentFrames++;
            requestFrameRepaint();
        }

        // The engine pauses itself once its output is silent, which can stop
        // this frame stream mid-fade; the watchdog then finishes the settle.
        // The Levels ballistics can outlive the phosphor fade (peak fall is
        // ~26dB/s), so the watchdog also runs until they reach the floor.
        if (m_silentFrames < SilentSettleFrames || LevelsRunningDown())
        {
            m_settleTimer->start();
        }
        else
        {
            m_settleTimer->stop();
        }

        // Deferred pause once everything has visually run down
        if (m_pendingPause && audio && silent)
        {
            Pause();
        }
    }
}

void ScopeWindow::SettleTick()
{
    const bool doneFading = m_silentFrames >= SilentSettleFrames && !LevelsRunningDown();
    if (m_paused || !isVisible() || doneFading)
    {
        m_settleTimer->stop();
        return;
    }
    m_audio = MakeSilentSnapshot();
    if (m_silentFrames < SilentSettleFrames)
    {
        m_silentFrames++;
    }
    requestFrameRepaint();
    if (m_silentFrames >= SilentSettleFrames && !LevelsRunningDown())
    {
        m_settleTimer->stop();
        if (m_pendingPause)
        {
            Pause();
        }
    }
}

void ScopeWindow::ResetToRest()
{
    m_audio = MakeSilentSnapshot();
    for (int ch = 0; ch < 2; ch++)
    {
        m_levels[ch].rmsDb = LevelFloorDb;
        m_levels[ch].peakDb = LevelFloorDb;
        m_levels[ch].holdDb = LevelFloorDb;
        m_levels[ch].holdUntilMs = 0;
    }
    m_grDb = 0.0f;
    m_grTargetDb = 0.0f;
    // The opaque clear removes the trail in one paint, so no settle fade is
    // owed; leaving m_silentFrames below the cap would just schedule idle
    // repaints of an already-blank scope.
    m_silentFrames = SilentSettleFrames;
    m_fullClear = true;
    update();
}

SonicPi::ProcessedAudioPtr ScopeWindow::MakeSilentSnapshot() const
{
    auto snap = std::make_shared<ProcessedAudio>();
    const ProcessedAudio& cur = *m_audio;
    for (int i = 0; i < 2; i++)
    {
        snap->m_samples[i].assign(std::max<size_t>(cur.m_samples[i].size(), 1), 0.0f);
        snap->m_spectrumQuantized[i].assign(cur.m_spectrumQuantized[i].size(), 0.0f);
        snap->m_spectrumPeaks[i].assign(cur.m_spectrumPeaks[i].size(), 0.0f);
    }
    snap->m_monoSamples.assign(std::max<size_t>(cur.m_monoSamples.size(), 1), 0.0f);
    return snap;
}

} // namespace SonicPi
