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
}

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

    m_panels.push_back({"Lissajous", ScopeType::Lissajous} );
    m_panels.push_back({"Stereo", ScopeType::Left} );
    m_panels.push_back({"Stereo", ScopeType::Right} );
    m_panels.push_back({"Mono", ScopeType::Mono} );

    SetColor(QColor("deeppink"));

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
    Layout();
}

void Scope::paintEvent(QPaintEvent* pEv)
{
    QPainter painter(this);

    painter.begin(this);
    painter.fillRect(rect(), Qt::white);

    for (auto& panel : m_panels)
    {
        if (!panel.visible)
        {
            continue;
        }

        float yScale = float(panel.rc.height() / 2.0f);
        int y = panel.rc.center().y();

        if (panel.type == ScopeType::Lissajous)
        {
            float xScale = float(panel.rc.width() / 2.0f);
            float scale = std::min(xScale, yScale);

            QPoint center = panel.rc.center();

            // First point at 0
            panel.wavePoints.resize(LissajousSamples + 1);
            panel.wavePoints[0] = center;
            for (int sample = 0; sample < LissajousSamples; sample++)
            {
                auto left = m_samples[0][FrameSamples - LissajousSamples + sample];
                auto right = m_samples[1][FrameSamples - LissajousSamples + sample];
                panel.wavePoints[sample + 1] = center + QPoint(left * xScale, right * yScale);
            }
        }
        else
        {
            // A crude sampling of the source data, with no bounds checking...
            // and a nasty hack to extract the data from the scope (the data processing should be in a seperate thread somewhere).
            double step = FrameSamples / double(panel.rc.width());

            // Make a list of points; it's better to gather them and submit in a batch
            // Here we are just drawing in pixel space
            // Note: resize will be a no-op when it doesn't change ;)
            panel.wavePoints.resize(panel.rc.width());

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
                continue;
            }

            for (int x = 0; x < panel.rc.width(); x++)
            {
                auto sample = pSamples[int(double(x) * step)];
                panel.wavePoints[x] = QPoint(x + panel.rc.left(), sample * yScale + y);
            }
        }

        painter.setPen(panel.pen);
        painter.drawPolyline(&panel.wavePoints[0], int(panel.wavePoints.size()));

    }
    painter.end();
}

void Scope::Layout()
{
    QRect rc = rect();
    int yMargin = ScaleWidthForDPI(4);
    int xMargin = ScaleHeightForDPI(4);

    int visibleCount = std::count_if(m_panels.begin(), m_panels.end(), [&visibleCount](Panel& p) { return p.visible; });

    QSize panelSize = QSize(rc.width() - (xMargin * 2), rc.height() - (yMargin * (visibleCount + 1)));
    panelSize.setHeight(int(panelSize.height() / float(visibleCount)));

    QPoint currentTopLeft(xMargin, yMargin);

    for (auto& panel : m_panels)
    {
        if (!panel.visible)
        {
            continue;
        }
        panel.rc = QRect(currentTopLeft, panelSize);
        currentTopLeft.setY(currentTopLeft.y() + panelSize.height() + yMargin);
    }
    repaint();
}

std::vector<QString> Scope::GetScopeNames() const
{
    std::set<QString> names;
    for (auto& scope : m_panels)
    {
        names.insert(scope.name);
    }
    return std::vector<QString>(names.begin(), names.end());
}

bool Scope::EnableScope(const QString& name, bool on)
{
    bool any = false;
    for (auto& scope : m_panels)
    {
        if (scope.name == name)
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
        scope.pen = QPen(c, PenWidth);
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

