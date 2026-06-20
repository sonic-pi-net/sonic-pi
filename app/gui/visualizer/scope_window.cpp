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
#include <QIcon>
#include <QPaintEvent>
#include <QPainter>
#include <QResizeEvent>
#include <QTimer>
#include <QVBoxLayout>

#include <algorithm>
#include <cmath>
#include <set>

#include "dpi.h"

#include "qt_api_client.h"

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
} // namespace

ScopeWindow::ScopeWindow(std::shared_ptr<QtAPIClient> spClient, std::shared_ptr<SonicPiAPI> spAPI, QWidget* parent)
    : QWidget(parent)
    , m_spClient(spClient)
    , m_spAPI(spAPI)
{
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

    Layout();
}

ScopeWindow::~ScopeWindow()
{
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
        tickFont.setPointSizeF(tickFont.pointSizeF() * 0.75);
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

    // Peak envelope per pixel column: every sample in the column's bin is
    // inspected, so transients can't alias away as they did when we
    // point-sampled one-in-N.
    double step = m_audioFrameSamples / double(width);

    // One vertical line per column per channel
    panel.waveLines.resize(width * 2);

    float yScale = float(panel.rcGraph.height() / 2.0f);
    int y = panel.rcGraph.center().y();
    int left = panel.rcGraph.left();

    for (int x = 0; x < width; x++)
    {
        uint32_t start = uint32_t(double(x) * step);
        uint32_t end = std::max(start + 1, uint32_t(double(x + 1) * step));
        end = std::min(end, m_audioFrameSamples);

        float peakLeft = 0.0f;
        float peakRight = 0.0f;
        for (uint32_t i = start; i < end; i++)
        {
            peakLeft = std::max(peakLeft, std::abs(audio.m_samples[0][i]));
            peakRight = std::max(peakRight, std::abs(audio.m_samples[1][i]));
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

    // Min/max envelope per pixel column (the classic scope/wave-editor
    // rendering): stable image, no aliasing of fast transients.
    double step = m_audioFrameSamples / double(width);
    panel.waveLines.resize(width);

    float yScale = float(panel.rcGraph.height() / 2.0f);
    int y = panel.rcGraph.center().y();
    int left = panel.rcGraph.left();

    for (int x = 0; x < width; x++)
    {
        uint32_t start = uint32_t(double(x) * step);
        uint32_t end = std::max(start + 1, uint32_t(double(x + 1) * step));
        end = std::min(end, m_audioFrameSamples);

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
    float yScale = float(panel.rcGraph.height() / 2.0f);
    float xScale = float(panel.rcGraph.width() / 2.0f);

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
        panel.wavePoints[sample] = center + QPoint(left * xScale, right * yScale);
    }
    // The only diagonal-line panel; AA is cheap here and removes the jaggies.
    painter.setRenderHint(QPainter::Antialiasing, true);
    painter.setPen(panel.pen);
    painter.drawPolyline(&panel.wavePoints[0], int(panel.wavePoints.size()));
    painter.setRenderHint(QPainter::Antialiasing, false);
}

void ScopeWindow::paintEvent(QPaintEvent* pEv)
{
    QPainter painter(this);

    auto backColor = QWidget::palette().color(QWidget::backgroundRole());
    auto textColor = QWidget::palette().color(QWidget::foregroundRole());
    auto shadowColor = QWidget::palette().color(QPalette::ColorRole::AlternateBase);

    // Clear with a small alpha so the previous frame's trace lingers and fades
    // out over a few frames — a soft decay. The first paint (and any resize)
    // does one opaque clear so no stale framebuffer pixels show through.
    if (m_fullClear)
    {
        painter.fillRect(rect(), backColor);
        m_fullClear = false;
    }
    else
    {
        constexpr int kDecayAlpha = 70;   // higher = faster decay / shorter trail
        QColor fade = backColor;
        fade.setAlpha(kDecayAlpha);
        painter.fillRect(rect(), fade);
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
    }
}

void ScopeWindow::Layout()
{
    QRect rc = rect();
    int yMargin = ScaleWidthForDPI(4);
    int yFontMargin = ScaleWidthForDPI(2);
    int xMargin = ScaleHeightForDPI(4);

    int visibleCount = std::count_if(m_panels.begin(), m_panels.end(), [&visibleCount](ScopeWindowPanel& p) { return p.visible; });

    QSize panelSize = QSize(rc.width() - (xMargin * 2), rc.height() - (yMargin * (visibleCount + 1)));
    panelSize.setHeight(int(panelSize.height() / float(visibleCount)));

    QPoint currentTopLeft(xMargin, yMargin);

    QFontMetrics metrics(qApp->font());

    for (auto& panel : m_panels)
    {
        if (!panel.visible)
        {
            continue;
        }
        panel.rc = QRect(currentTopLeft, panelSize);
        panel.rcGraph = panel.rc;
        panel.rcTitle = QRect(currentTopLeft, QSize(panelSize.width(), 0));

        if (panel.titleVisible)
        {
            panel.rcTitle.setHeight(metrics.height() + yFontMargin * 2);
            panel.rcGraph.setTop(panel.rcTitle.bottom());
        }
        currentTopLeft.setY(currentTopLeft.y() + panelSize.height() + yMargin);
    }

    m_spAPI->AudioProcessor_SetMaxFFTBuckets(panelSize.width() / 4);

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
    bool doFFT = false;
    for (auto& scope : m_panels)
    {
        if (scope.category == category)
        {
            scope.visible = on;
            any = true;
        }

        if (scope.visible && scope.requireFFT)
        {
            doFFT = true;
        }
    }

    m_spAPI->AudioProcessor_Enable(any);
    m_spAPI->AudioProcessor_EnableFFT(doFFT);

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
    m_spAPI->AudioProcessor_Enable(!m_paused);
}

void ScopeWindow::TogglePause()
{
    m_pendingPause = false;
    m_paused = !m_paused;
    m_spAPI->AudioProcessor_Enable(!m_paused);
}

void ScopeWindow::Pause()
{
    m_pendingPause = false;
    m_paused = true;
    m_spAPI->AudioProcessor_Enable(!m_paused);
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
    m_pendingPause = false;
    m_paused = false;
    m_spAPI->AudioProcessor_Enable(!m_paused);
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

void ScopeWindow::OnConsumeAudioData(SonicPi::ProcessedAudioPtr audio)
{
    if (!m_paused && isVisible())
    {
        if (audio && !audio->m_samples[0].empty())
        {
            m_audio = audio;
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
        const bool silent = !audio || SnapshotSilent(*audio);
        if (!silent)
        {
            m_silentFrames = 0;
            update();
        }
        else if (m_silentFrames < SilentSettleFrames)
        {
            m_silentFrames++;
            update();
        }

        // Deferred pause once everything has visually run down
        if (m_pendingPause && audio && silent)
        {
            Pause();
        }
    }
}

} // namespace SonicPi
