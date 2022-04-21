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

#include <complex>
#include <memory>
#include <string>
#include <thread>
#include <mutex>
#include <visualizer/server_shm.hpp>

#include "kiss_fft/kiss_fft.h"
#include "profiler.h"

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
    bool titleVisible = true;
    bool requireFFT = false;

    std::vector<QPoint> wavePoints;
    std::vector<QRect> waveRects;
    QLinearGradient redBlueGradient;
};

// This is the processed audio data from the thread
struct ProcessedAudio
{
    SP_Lockable(std::mutex, m_mutex);
    std::vector<float> m_spectrum[2];
    std::vector<float> m_spectrumQuantized[2];
    std::vector<std::vector<double>> m_samples;
    std::vector<double> m_monoSamples;
    std::atomic<bool> m_consumed = {true};
};

class AudioProcessingThread : public QThread
{
    Q_OBJECT

public:
    AudioProcessingThread(int synthPort);

    virtual void run() override;

    ProcessedAudio& GetCurrentProcessedAudio();
    void EnableFFT(bool enable);
    void Enable(bool start);
    void SetMaxBuckets(int maxBuckets);
    void Quit();

signals:
    void update();

private:
    void ResetConnection();
    void GenLogSpace(uint32_t limit, uint32_t n);
    void GenLinSpace(uint32_t limit, uint32_t n);
    void SetupFFT();
    void CalculateFFT(ProcessedAudio& audio);

private:
    std::unique_ptr<server_shared_memory_client> m_shmClient;
    scope_buffer_reader m_shmReader;

    unsigned int m_emptyFrames = 0;
    int m_scsynthPort = 0;

    std::atomic<bool> m_calculateFFT = {false};
    std::atomic<bool> m_running = {false};
    std::atomic<int> m_maxBuckets = {0};
    std::atomic<bool> m_quit = {false};

    // FFT
    mkiss_fft_cfg m_cfg;
    std::vector<std::complex<float>> m_fftIn[2];
    std::vector<std::complex<float>> m_fftOut[2];
    std::vector<float> m_fftMag[2];
    std::vector<float> m_window;
    std::vector<float> m_spectrumPartitions;
    std::pair<uint32_t, uint32_t> m_lastSpectrumPartitions = { 0, 0 };

    // Output data, double buffered
    ProcessedAudio m_processedAudio;

    float m_totalWin = 0.0f;
};

class Scope : public QOpenGLWidget
{
    Q_OBJECT

public:
    Scope(int scsynthPort, QWidget* parent = nullptr);
    virtual ~Scope();

    std::vector<QString> GetScopeCategories() const;
    bool EnableScope(const QString& name, bool on);
    bool SetScopeLabels(bool on);
    void TogglePause();
    void Pause();
    void Resume();
    void Refresh();
    void ScsynthBooted();
    void SetColor(QColor c);
    void SetColor2(QColor c);

    void DrawWave(const ProcessedAudio& audio, QPainter& painter, Panel& panel);
    void DrawMirrorStereo(const ProcessedAudio& audio, QPainter& painter, Panel& panel);
    void DrawLissajous(const ProcessedAudio& audio, QPainter& painter, Panel& panel);
    void DrawSpectrumAnalysis(const ProcessedAudio& audio, QPainter& painter, Panel& panel);

    void ShutDown();

protected:
    virtual void paintEvent(QPaintEvent* pEv) override;
    virtual void resizeEvent(QResizeEvent* pSize) override;

private:
    void Layout();

private slots:
    void OnNewAudioData();

private:
    std::vector<Panel> m_panels;
    int m_scsynthPort = 0;
    bool m_paused = false;
    AudioProcessingThread* m_pAudioThread = nullptr;
};
