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
const float ScopeWindowRefreshRate = 50.0f;
} // namespace

ScopeWindow::ScopeWindow(std::shared_ptr<QtAPIClient> spClient, std::shared_ptr<SonicPiAPI> spAPI, QWidget* parent)
    : QOpenGLWidget(parent)
    , m_spClient(spClient)
    , m_spAPI(spAPI)
{
    QVBoxLayout* layout = new QVBoxLayout();
    layout->setContentsMargins(0, 0, 0, 0);
    setLayout(layout);

    // Force the audio to contain at least one sample; since the panels currently expect it!
    for (int i = 0; i < 2; i++)
    {
        m_audio.m_monoSamples.push_back(0);
        m_audio.m_samples[i].push_back(0);
        m_audio.m_spectrum[i].push_back(0);
        m_audio.m_spectrumQuantized[i].push_back(0);
    }

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

    qRegisterMetaType<ProcessedAudio>("ProcessedAudio");
    connect(m_spClient.get(), &QtAPIClient::ConsumeAudioData, this, &ScopeWindow::OnConsumeAudioData);

    Layout();
}

ScopeWindow::~ScopeWindow()
{
}

void ScopeWindow::ShutDown()
{
    if (m_spAPI)
    {
        m_spAPI->AudioProcessor_Enable(false);
    }
}

void ScopeWindow::resizeEvent(QResizeEvent* pSize)
{
    QOpenGLWidget::resizeEvent(pSize);

    Layout();
}

// Draw a Simple Stereo representation with a mirror of right/left stereo
void ScopeWindow::DrawSpectrumAnalysis(const ProcessedAudio& audio, QPainter& painter, ScopeWindowPanel& panel)
{
    if (audio.m_spectrumQuantized[0].empty() || audio.m_spectrumQuantized[1].empty())
        return;

    int centerY = panel.rcGraph.center().y();
    float sampleScale = panel.rcGraph.height() / 2.0f;

    panel.waveRects.resize(audio.m_spectrumQuantized[0].size() * 2);

    // Make a pixel margin between the buckets for a cleaner view
    float stepPerRect = std::ceil(panel.rcGraph.width() / float(audio.m_spectrumQuantized[0].size()));
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
    for (uint32_t index = 0; index < audio.m_spectrumQuantized[0].size(); index++)
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

    // Batch by brush
    for (uint32_t index = 0; index < uint32_t(panel.waveRects.size() / 2); index++)
    {
        painter.fillRect(panel.waveRects[index], panel.brush);
    }

    for (uint32_t index = 0; index < uint32_t(panel.waveRects.size() / 2); index++)
    {
        painter.fillRect(panel.waveRects[rightIndex + index], panel.brush2);
    }
}

// Draw a Simple Stereo representation with a mirror of right/left stereo
void ScopeWindow::DrawMirrorStereo(const ProcessedAudio& audio, QPainter& painter, ScopeWindowPanel& panel)
{
    // Just sample the data at intervals; we should probably filter it too
    double step = m_audioFrameSamples / double(panel.rcGraph.width());

    // Make a list of points; it's better to gather them and submit in a batch
    // Here we are just drawing in pixel space
    // Note: resize will be a no-op when it doesn't change ;)

    // We have a 4 points for 2 lines on every row of the display
    panel.wavePoints.resize(panel.rcGraph.width() * 4, QPoint(0, 0));

    float yScale = float(panel.rcGraph.height() / 2.0f);
    int y = panel.rcGraph.center().y();

    int rightIndex = int(panel.wavePoints.size()) / 2;
    int left = panel.rcGraph.left();
    for (int x = 0; x < panel.rcGraph.width(); x++)
    {
        auto sampleLeft = int(std::abs(audio.m_samples[0][int(double(x) * step)]) * yScale + y + 1);
        auto sampleRight = int(std::abs(audio.m_samples[1][int(double(x) * step)]) * -yScale + y - 1);

        int index = x * 2;
        int xCoord = left + x;
        panel.wavePoints[index] = QPoint(xCoord, sampleLeft);
        panel.wavePoints[index + 1] = QPoint(xCoord, y);
        panel.wavePoints[rightIndex + index] = QPoint(xCoord, y);
        panel.wavePoints[rightIndex + index + 1] = QPoint(xCoord, sampleRight);
    }
    painter.setPen(panel.pen2);
    painter.drawLines(&panel.wavePoints[0], rightIndex / 2);
    painter.setPen(panel.pen);
    painter.drawLines(&panel.wavePoints[rightIndex], rightIndex / 2);
}

// Draw a Simple Wave
void ScopeWindow::DrawWave(const ProcessedAudio& audio, QPainter& painter, ScopeWindowPanel& panel)
{
    // Just sample the data at intervals; we should probably filter it too
    double step = m_audioFrameSamples / double(panel.rcGraph.width());

    // Make a list of points; it's better to gather them and submit in a batch
    // Here we are just drawing in pixel space
    // Note: resize will be a no-op when it doesn't change ;)
    panel.wavePoints.resize(panel.rcGraph.width());

    float yScale = float(panel.rcGraph.height() / 2.0f);
    int y = panel.rcGraph.center().y();

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

    for (int x = 0; x < panel.rcGraph.width(); x++)
    {
        auto sample = pSamples[int(double(x) * step)];
        panel.wavePoints[x] = QPoint(x + panel.rcGraph.left(), sample * yScale + y);
    }
    painter.setPen(panel.pen);
    painter.drawPolyline(&panel.wavePoints[0], int(panel.wavePoints.size()));
}

void ScopeWindow::DrawLissajous(const ProcessedAudio& audio, QPainter& painter, ScopeWindowPanel& panel)
{
    float yScale = float(panel.rcGraph.height() / 2.0f);
    int y = panel.rcGraph.center().y();

    float xScale = float(panel.rcGraph.width() / 2.0f);
    float scale = std::min(xScale, yScale);

    QPoint center = panel.rcGraph.center();

    auto samples = std::min(LissajousSamples, int(m_audioFrameSamples));
    panel.wavePoints.resize(samples);
    for (int sample = 0; sample < samples; sample++)
    {
        auto left = audio.m_samples[0][sample];
        auto right = audio.m_samples[1][sample];
        panel.wavePoints[sample] = center + QPoint(left * xScale, right * yScale);
    }
    painter.setPen(panel.pen);
    painter.drawPolyline(&panel.wavePoints[0], int(panel.wavePoints.size()));
}

void ScopeWindow::paintEvent(QPaintEvent* pEv)
{
    QPainter painter(this);

    auto backColor = QWidget::palette().color(QWidget::backgroundRole());
    auto textColor = QWidget::palette().color(QWidget::foregroundRole());
    auto shadowColor = QWidget::palette().color(QPalette::ColorRole::AlternateBase);

    painter.fillRect(rect(), backColor);

    std::scoped_lock lock(m_dataMutex);
    auto& processedAudio = m_audio;

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
            painter.drawText(panel.rcTitle, Qt::AlignCenter, tr(panel.name.toUtf8().data()));
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
            if (!m_audio.m_spectrum[0].empty())
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

    repaint();
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
    m_paused = !m_paused;
    m_spAPI->AudioProcessor_Enable(!m_paused);
}

void ScopeWindow::Pause()
{
    m_paused = true;
    m_spAPI->AudioProcessor_Enable(!m_paused);
}

void ScopeWindow::Resume()
{
    m_paused = false;
    m_spAPI->AudioProcessor_Enable(!m_paused);
}

void ScopeWindow::Refresh()
{
    repaint();
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

void ScopeWindow::OnConsumeAudioData(const ProcessedAudio& audio)
{
    if (!m_paused && isVisible())
    {
        std::scoped_lock lk(m_dataMutex);

        if (!audio.m_samples[0].empty())
        {
            m_audio = audio;
        }

        m_audioAvailable = true;

        update();
    }
}

} // namespace SonicPi
