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

#pragma once

#include <QWidget>
#include <QOpenGLWidget>
#include <QPen>

#include <complex> 
#include <memory>
#include <string>
#include <visualizer/server_shm.hpp>

#include "kiss_fft/kiss_fft.h"

QT_FORWARD_DECLARE_CLASS(QPaintEvent)
QT_FORWARD_DECLARE_CLASS(QResizeEvent)

enum class ScopeType
{
    Left,
    Right,
    Mono,
    Lissajous,
    MirrorStereo,
    SpectrumAnalysis
};

struct Panel
{
    Panel(const QString& cat, const QString& n, ScopeType t)
      : category(cat), name(n), type(t){}
    QString category;
    QString name;
    ScopeType type;

    QRect rc;
    QRect rcGraph;
    QRect rcTitle;
    QPen pen;
    QPen pen2;
    QBrush brush;
    QBrush brush2;
    bool visible = true;
    bool axisVisible = false;
    bool titleVisible = true;
    bool requireFFT = false;

    std::vector<QPoint> wavePoints;
    std::vector<QRect> waveRects;
    QLinearGradient redBlueGradient;
};

class Scope : public QOpenGLWidget
{
    Q_OBJECT

public:
    Scope(int scsynthPort, QWidget* parent = nullptr);
    virtual ~Scope();

    std::vector<QString> GetScopeCategories() const;
    bool EnableScope(const QString& name, bool on);
    bool SetScopeAxes(bool on);
    void TogglePause();
    void Pause();
    void Resume();
    void ResetScope();
    void Refresh();
    void ScsynthBooted();
    void SetColor(QColor c);
    void SetColor2(QColor c);

    void DrawWave(QPainter& painter, Panel& panel);
    void DrawMirrorStereo(QPainter& painter, Panel& panel);
    void DrawLissajous(QPainter& painter, Panel& panel);
    void DrawSpectrumAnalysis(QPainter& painter, Panel& panel);

    void SetupFFT();
    void CalculateFFT();

protected:
    virtual void paintEvent(QPaintEvent* pEv) override;
    virtual void resizeEvent(QResizeEvent* pSize) override;

private:
    void Layout();

private slots:
    void OnTimer();

private:
    std::vector<Panel> m_panels;

    std::unique_ptr<server_shared_memory_client> m_shmClient;
    scope_buffer_reader m_shmReader;

    std::vector<std::vector<double>> m_samples;
    std::vector<double> m_monoSamples;

    unsigned int m_emptyFrames = 0;
    int m_scsynthPort = 0;

    bool m_scsynthIsBooted = false;
    bool m_paused = false;
    bool m_calculateFFT = false;

    // FFT
    mkiss_fft_cfg m_cfg;
    std::vector<std::complex<float>> m_fftIn[2];
    std::vector<std::complex<float>> m_fftOut[2];
    std::vector<float> m_fftMag[2];
    std::vector<float> m_spectrum[2];
    std::vector<float> m_spectrumQuantized[2];
    std::vector<float> m_window;
    float m_totalWin = 0.0f;
};
