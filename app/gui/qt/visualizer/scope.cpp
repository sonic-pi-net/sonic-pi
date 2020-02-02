//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
//
// Copyright (C) 2016 by Adrian Cheater
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include "scope.h"

#include <QDebug>
#include <QIcon>
#include <QPaintEvent>
#include <QPainter>
#include <QResizeEvent>
#include <QTimer>
#include <QVBoxLayout>
#include <cmath>
#include <set>

#include "dpi.h"

namespace
{
const int FrameSamples = 4096;
const int LissajousSamples = 1024;
const int PenWidth = 1;
} // namespace

Scope::Scope(int scsynthPort, QWidget* parent)
    : QOpenGLWidget(parent)
    , m_scsynthPort(scsynthPort)
{
    QVBoxLayout* layout = new QVBoxLayout();
    layout->setContentsMargins(0, 0, 0, 0);
    setLayout(layout);

    m_samples.resize(2);
    m_samples[0].resize(FrameSamples, 0.0);
    m_samples[1].resize(FrameSamples, 0.0);

    m_monoSamples.resize(FrameSamples, 0.0);

    m_panels.push_back({ "Lissajous", "Lissajous", ScopeType::Lissajous });
    m_panels.push_back({ "Stereo", "Left", ScopeType::Left });
    m_panels.push_back({ "Stereo", "Right", ScopeType::Right });
    m_panels.push_back({ "Mono", "Mono", ScopeType::Mono });
    m_panels.push_back({ "Mirror Stereo", "Stereo", ScopeType::MirrorStereo });

    for (auto& scope : m_panels)
    {
      scope.pen = QPen();
      scope.pen.setWidth(PenWidth);
      scope.pen2 = QPen();
      scope.pen2.setWidth(PenWidth);
    }

    QTimer* scopeTimer = new QTimer(this);
    connect(scopeTimer, &QTimer::timeout, this, &Scope::OnTimer);
    scopeTimer->start(20);

    Layout();
}

Scope::~Scope()
{
}

void Scope::resizeEvent(QResizeEvent* pSize)
{
    QOpenGLWidget::resizeEvent(pSize);
    Layout();
}

// Draw a Simple Stereo representation with a mirror of right/left stereo
void Scope::DrawMirrorStereo(QPainter& painter, Panel& panel)
{
    // Just sample the data at intervals; we should probably filter it too
    double step = FrameSamples / double(panel.rcGraph.width());

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
        auto sampleLeft = int(std::abs(m_samples[0][int(double(x) * step)]) * yScale + y + 1);
        auto sampleRight = int(std::abs(m_samples[1][int(double(x) * step)]) * -yScale + y - 1);

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

    /*
    TBD: Composition modes not working?
    painter.setCompositionMode(QPainter::CompositionMode::CompositionMode_Multiply);
    panel.redBlueGradient = QLinearGradient(0, panel.rcGraph.top(), 0, panel.rcGraph.bottom());
    panel.redBlueGradient.setColorAt(0.3, QColor(255, 255, 255, 255));
    panel.redBlueGradient.setColorAt(0.5, QColor(0, 0, 0, 0));
    panel.redBlueGradient.setColorAt(0.7, QColor(255, 255, 255, 255));
    painter.fillRect(panel.rcGraph, panel.redBlueGradient);
    */
}

// Draw a Simple Wave
void Scope::DrawWave(QPainter& painter, Panel& panel)
{
    // Just sample the data at intervals; we should probably filter it too
    double step = FrameSamples / double(panel.rcGraph.width());

    // Make a list of points; it's better to gather them and submit in a batch
    // Here we are just drawing in pixel space
    // Note: resize will be a no-op when it doesn't change ;)
    panel.wavePoints.resize(panel.rcGraph.width());

    float yScale = float(panel.rcGraph.height() / 2.0f);
    int y = panel.rcGraph.center().y();

    double* pSamples = nullptr;
    switch (panel.type)
    {
    case ScopeType::Left:
        pSamples = &m_samples[0][0];
        break;
    case ScopeType::Right:
        pSamples = &m_samples[1][0];
        break;
    case ScopeType::Mono:
        pSamples = &m_monoSamples[0];
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

void Scope::DrawLissajous(QPainter& painter, Panel& panel)
{
    float yScale = float(panel.rcGraph.height() / 2.0f);
    int y = panel.rcGraph.center().y();

    float xScale = float(panel.rcGraph.width() / 2.0f);
    float scale = std::min(xScale, yScale);

    QPoint center = panel.rcGraph.center();

    // First point at 0
    panel.wavePoints.resize(LissajousSamples + 1);
    panel.wavePoints[0] = center;
    for (int sample = 0; sample < LissajousSamples; sample++)
    {
        auto left = m_samples[0][FrameSamples - LissajousSamples + sample];
        auto right = m_samples[1][FrameSamples - LissajousSamples + sample];
        panel.wavePoints[sample + 1] = center + QPoint(left * xScale, right * yScale);
    }
    painter.setPen(panel.pen);
    painter.drawPolyline(&panel.wavePoints[0], int(panel.wavePoints.size()));
}

void Scope::paintEvent(QPaintEvent* pEv)
{
    QPainter painter(this);

    auto backColor = QWidget::palette().color(QWidget::backgroundRole());
    auto textColor = QWidget::palette().color(QWidget::foregroundRole());
    auto shadowColor = QWidget::palette().color(QPalette::ColorRole::AlternateBase);

    painter.fillRect(rect(), backColor);

    for (auto& panel : m_panels)
    {
        if (!panel.visible)
        {
            continue;
        }

        if (panel.titleVisible)
        {
            // Optional fill title area background
            //painter.fillRect(panel.rcTitle, shadowColor);

            painter.setPen(textColor);
            painter.drawText(panel.rcTitle, Qt::AlignCenter, tr(panel.name.toUtf8().data()));
        }

        if (panel.type == ScopeType::Lissajous)
        {
            DrawLissajous(painter, panel);
        }
        else if (panel.type == ScopeType::Left || panel.type == ScopeType::Right || panel.type == ScopeType::Mono)
        {
            DrawWave(painter, panel);
        }
        else if (panel.type == ScopeType::MirrorStereo)
        {
            DrawMirrorStereo(painter, panel);
        }
    }
}

void Scope::Layout()
{
    QRect rc = rect();
    int yMargin = ScaleWidthForDPI(4);
    int yFontMargin = ScaleWidthForDPI(2);
    int xMargin = ScaleHeightForDPI(4);

    int visibleCount = std::count_if(m_panels.begin(), m_panels.end(), [&visibleCount](Panel& p) { return p.visible; });

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

    repaint();
}

std::vector<QString> Scope::GetScopeCategories() const
{
    std::set<QString> cat;
    for (auto& scope : m_panels)
    {
        cat.insert(scope.category);
    }
    return std::vector<QString>(cat.begin(), cat.end());
}

bool Scope::EnableScope(const QString& category, bool on)
{
    bool any = false;
    for (auto& scope : m_panels)
    {
        if (scope.category == category)
        {
            scope.visible = on;
            any = true;
        }
    }
    Layout();

    return any ? on : true;
}

bool Scope::SetScopeAxes(bool on)
{
    for (auto& scope : m_panels)
    {
        scope.axisVisible = on;
    }
    return on;
}

void Scope::ScsynthBooted()
{
    m_scsynthIsBooted = true;
}

void Scope::TogglePause()
{
    m_paused = !m_paused;
}

void Scope::Pause()
{
    m_paused = true;
}

void Scope::Resume()
{
    m_paused = false;
}

void Scope::SetColor(QColor c)
{
    for (auto& scope : m_panels)
    {
        scope.pen.setColor(c);
    }
}

void Scope::SetColor2(QColor c)
{
    for (auto& scope : m_panels)
    {
        scope.pen2.setColor(c);
    }
}

void Scope::ResetScope()
{
    m_shmClient.reset(new server_shared_memory_client(m_scsynthPort));
    m_shmReader = m_shmClient->get_scope_buffer_reader(0);
}

void Scope::Refresh()
{
    if (!m_scsynthIsBooted)
        return;

    if (!m_shmReader.valid())
    {
        ResetScope();
    }

    unsigned int frames;
    if (m_shmReader.pull(frames))
    {
        m_emptyFrames = 0;
        float* data = m_shmReader.data();
        for (unsigned int j = 0; j < 2; ++j)
        {
            unsigned int offset = m_shmReader.max_frames() * j;
            for (unsigned int i = 0; i < FrameSamples - frames; ++i)
            {
                m_samples[j][i] = m_samples[j][i + frames];
                if (j == 0)
                {
                    m_monoSamples[i] = m_monoSamples[i + frames];
                }
            }

            for (unsigned int i = 0; i < frames; ++i)
            {
                m_samples[j][FrameSamples - frames + i] = data[i + offset];
                auto d = data[i + offset] + 1.0;
                if (j == 0)
                {
                    m_monoSamples[FrameSamples - frames + i] = d * d;
                }
                else
                {
                    m_monoSamples[FrameSamples - frames + i] += d * d;
                    m_monoSamples[FrameSamples - frames + i] /= 2.0f;
                    m_monoSamples[FrameSamples - frames + i] = sqrt(m_monoSamples[FrameSamples - frames + i]) - 1.0;
                }
            }
        }
    }
    else
    {
        ++m_emptyFrames;
        if (m_emptyFrames > 10)
        {
            ResetScope();
            m_emptyFrames = 0;
        }
    }

    repaint();
}

void Scope::OnTimer()
{
    // short circuit if possible
    if (m_paused)
    {
        return;
    }

    if (!isVisible())
    {
        return;
    }

    Refresh();
}
