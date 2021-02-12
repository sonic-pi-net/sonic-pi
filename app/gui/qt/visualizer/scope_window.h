//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright (C) 2016 by Adrian Cheater
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++
#pragma once
 
#include <QWidget>
#include <QOpenGLWidget>
#include <QPen>
#include <QThread>

#include <memory>
#include <string>
#include <mutex>

#include <qt_api_client.h>

#include "config.h"

QT_FORWARD_DECLARE_CLASS(QPaintEvent)
QT_FORWARD_DECLARE_CLASS(QResizeEvent)

namespace SonicPi
{

enum class ScopeWindowType
{
    Left,
    Right,
    Mono,
    Lissajous,
    MirrorStereo,
    SpectrumAnalysis
};

struct ScopeWindowPanel
{
    ScopeWindowPanel(const QString& cat, const QString& n, ScopeWindowType t)
        : category(cat)
        , name(n)
        , type(t)
    {
    }
    QString category;
    QString name;
    ScopeWindowType type;

    QRect rc;
    QRect rcGraph;
    QRect rcTitle;
    QPen pen;
    QPen pen2;
    QBrush brush;
    QBrush brush2;
    bool visible = true;
    bool titleVisible = true;
    bool requireFFT = false;

    std::vector<QPoint> wavePoints;
    std::vector<QRect> waveRects;
    QLinearGradient redBlueGradient;
};

class ScopeWindow : public QOpenGLWidget
{
    Q_OBJECT

public:
    ScopeWindow(std::shared_ptr<QtAPIClient> spClient, std::shared_ptr<SonicPiAPI> spAPI, QWidget* parent = nullptr);
    virtual ~ScopeWindow();

    std::vector<QString> GetScopeCategories() const;
    bool EnableScope(const QString& name, bool on);
    bool SetScopeLabels(bool on);
    void TogglePause();
    void Pause();
    void Resume();
    void SetColor(QColor c);
    void SetColor2(QColor c);

    void DrawWave(const ProcessedAudio& audio, QPainter& painter, ScopeWindowPanel& panel);
    void DrawMirrorStereo(const ProcessedAudio& audio, QPainter& painter, ScopeWindowPanel& panel);
    void DrawLissajous(const ProcessedAudio& audio, QPainter& painter, ScopeWindowPanel& panel);
    void DrawSpectrumAnalysis(const ProcessedAudio& audio, QPainter& painter, ScopeWindowPanel& panel);

    void Booted();
    void ShutDown();

private slots:
    void OnConsumeAudioData(const ProcessedAudio& audio);

public slots :
    void Refresh();

protected:
    virtual void paintEvent(QPaintEvent* pEv) override;
    virtual void resizeEvent(QResizeEvent* pSize) override;

private:
    void Layout();


private:
    std::shared_ptr<SonicPiAPI> m_spAPI;
    std::shared_ptr<QtAPIClient> m_spClient;
    std::vector<ScopeWindowPanel> m_panels;
    bool m_paused = false;
    ProcessedAudio m_audio;
    std::mutex m_dataMutex;
    uint32_t m_audioFrameSamples = 0;
    std::atomic<bool> m_audioAvailable = false;
};

} // namespace SonicPi
