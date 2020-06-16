//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright (C) 2016 by Adrian Cheater
// All rights reserved.
//
// Changes by @cmaughan
// - Cleaned up and simplified code, removed the Qt graph library
// - Use QPainter for drawing with a QOpenGLWidget backing store
// - Add the mirror scope & spectrum analyzer
// - Moved the audio processing to a background thread, add FFT
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

#include <algorithm>
#include <cmath>
#include <set>

#include "dpi.h"

#include "kiss_fft/kiss_fft.h"

namespace
{
const int FrameSamples = 4096;
const int LissajousSamples = 1024;
const int PenWidth = 1;
const float FFTDecibelRange = 70.0f;
const float ScopeRefreshRate = 50.0f;

/// Creates a Hamming Window for FFT
/// FFT requires a window function to get smooth results
inline std::vector<float> createWindow(uint32_t size)
{
    const float PI = float(std::atan(1.0) * 4);
    std::vector<float> ret(size);
    for (uint32_t i = 0; i < size; i++)
    {
        ret[i] = (0.5f * (1 - cos(2.0f * PI * (i / (float)(size - 1)))));
    }

    return ret;
}
} // namespace

AudioProcessingThread::AudioProcessingThread(int synthPort)
    : m_scsynthPort(synthPort)
{
    SetupFFT();
}

ProcessedAudio& AudioProcessingThread::GetCurrentProcessedAudio()
{
    return m_processedAudio;
}

void AudioProcessingThread::Quit()
{
    m_quit.store(true);
}

void AudioProcessingThread::SetMaxBuckets(int maxBuckets)
{
    m_maxBuckets.store(maxBuckets);
}

void AudioProcessingThread::SetupFFT()
{
    m_processedAudio.m_samples.resize(2);
    m_processedAudio.m_samples[0].resize(FrameSamples, 0.0);
    m_processedAudio.m_samples[1].resize(FrameSamples, 0.0);
    m_processedAudio.m_monoSamples.resize(FrameSamples, 0.0);

    // FFT output is half the size of the input
    m_processedAudio.m_spectrum[0].resize(FrameSamples / 2, (0));
    m_processedAudio.m_spectrum[1].resize(FrameSamples / 2, (0));

    // Hamming window
    m_window = createWindow(FrameSamples);
    m_totalWin = 0.0f;
    for (auto& win : m_window)
    {
        m_totalWin += win;
    }

    // Imaginary part of audio input always 0.
    for (int i = 0; i < 2; i++)
    {
        m_fftIn[i].resize(FrameSamples, std::complex<float>{ 0.0, 0.0 });
        m_fftOut[i].resize(FrameSamples);
        m_fftMag[i].resize(FrameSamples);
    }

    m_cfg = kiss_fft_alloc(FrameSamples, 0, 0, 0);
}

// Generate a sequentially increasing space of numbers.
// The idea here is to generate partitions of the frequency spectrum that cover the whole
// range of values, but concentrate the results on the 'interesting' frequencies at the bottom end
// This code ported from the python below:
// https://stackoverflow.com/questions/12418234/logarithmically-spaced-integers
void AudioProcessingThread::GenLogSpace(uint32_t limit, uint32_t n)
{
    if (m_lastSpectrumPartitions == std::make_pair(limit, n) && !m_spectrumPartitions.empty())
    {
        return;
    }

    // Remember what we did last
    m_lastSpectrumPartitions = std::make_pair(limit, n);

    m_spectrumPartitions.resize(1);
    m_spectrumPartitions[0] = 1;

    float ratio = std::pow(float(limit), (1.0f / float(n - 1)));
    while (m_spectrumPartitions.size() < n)
    {
        auto lastValue = m_spectrumPartitions[m_spectrumPartitions.size() - 1];

        auto next_value = lastValue * ratio;
        if ((next_value - lastValue) >= 1)
        {
            // safe zone.next_value will be a different integer
            m_spectrumPartitions.push_back(next_value);
        }
        else
        {
            // problem !same integer.we need to find next_value by artificially incrementing previous value
            m_spectrumPartitions.push_back(lastValue + 1);

            // recalculate the ratio so that the remaining values will scale correctly
            ratio = pow(limit / (lastValue + 1), (1.0f / (n - float(m_spectrumPartitions.size()))));
        }
    }
}

// This is a simple linear partitioning of the frequencies
void AudioProcessingThread::GenLinSpace(uint32_t limit, uint32_t n)
{
    if (m_lastSpectrumPartitions == std::make_pair(limit, n) && !m_spectrumPartitions.empty())
    {
        return;
    }

    // Remember what we did last
    m_lastSpectrumPartitions = std::make_pair(limit, n);

    m_spectrumPartitions.resize(n);
    for (uint32_t i = 0; i < n; i++)
    {
        m_spectrumPartitions[i] = int(float(n) / (float(limit)));
    }
}

void AudioProcessingThread::CalculateFFT(ProcessedAudio& audio)
{
    SP_ZoneScopedN("FFT");

    if (!m_calculateFFT.load())
    {
        return;
    }

    // Magnitude
    const float ref = 1.0f; // Source reference value, but we are +/1.0f

    if (m_fftOut[0].size() == 0)
    {
        return;
    }

    for (int channel = 0; channel < 2; channel++)
    {
        for (uint32_t i = 0; i < FrameSamples; i++)
        {
            // Hamming window * audio
            m_fftIn[channel][i] = std::complex<float>(audio.m_samples[channel][i] * m_window[i], 0.0f);
        }

        // Do the FFT
        kiss_fft(m_cfg, (const mkiss_fft_cpx*)&m_fftIn[channel][0], (mkiss_fft_cpx*)&m_fftOut[channel][0]);

        // Sample 0 is the all frequency component
        m_fftOut[channel][0] = std::complex<float>(0.0f, 0.0f);

        for (uint32_t i = 0; i < FrameSamples / 2; i++)
        {
            // Magnitude
            m_fftMag[channel][i] = std::abs(m_fftOut[channel][i]);

            audio.m_spectrum[channel][i] = m_fftMag[channel][i] * 2.0f / m_totalWin;
            audio.m_spectrum[channel][i] = std::max(audio.m_spectrum[channel][i], std::numeric_limits<float>::min());

            // Log based on a reference value of 1
            audio.m_spectrum[channel][i] = 20 * std::log10(audio.m_spectrum[channel][i] / ref);

            // Normalize by moving up and dividing
            // Decibels are now positive from 0->1;
            audio.m_spectrum[channel][i] += FFTDecibelRange;
            audio.m_spectrum[channel][i] /= FFTDecibelRange;
            audio.m_spectrum[channel][i] = std::max(0.0f, audio.m_spectrum[channel][i]);
            audio.m_spectrum[channel][i] = std::min(1.0f, audio.m_spectrum[channel][i]);
        }

        // Quantize into bigger buckets; filtering helps smooth the graph, and gives a more pleasant effect
        uint32_t SpectrumSamples = uint32_t(audio.m_spectrum[channel].size());

        // Make less buckets on a big window, but at least 4
        uint32_t buckets = std::min(SpectrumSamples / 8, uint32_t(m_maxBuckets.load()));
        buckets = std::max(buckets, uint32_t(4));

        // Linear space shows lower frequencies, log space shows all freqencies but focused
        // on the lower buckets more
//#define LINEAR_SPACE
#ifdef LINEAR_SPACE
        GenLinSpace(SpectrumSamples / 4, buckets);
#else
        GenLogSpace(SpectrumSamples, buckets);
#endif
        auto itrPartition = m_spectrumPartitions.begin();

        if (buckets > 0)
        {
            float countPerBucket = (float)SpectrumSamples / (float)buckets;
            uint32_t currentBucket = 0;

            float av = 0.0f;
            uint32_t averageCount = 0;

            audio.m_spectrumQuantized[channel].resize(buckets);

            // Ignore the first spectrum sample
            for (uint32_t i = 1; i < SpectrumSamples; i++)
            {
                av += audio.m_spectrum[channel][i];
                averageCount++;

                if (i >= *itrPartition)
                {
                    audio.m_spectrumQuantized[channel][currentBucket++] = av / (float)averageCount;
                    av = 0.0f; // reset sum for next average
                    averageCount = 0;
                    itrPartition++;
                }

                // Sanity
                if (itrPartition == m_spectrumPartitions.end()
                    || currentBucket >= buckets)
                    break;
            }
        }
    }
}

void AudioProcessingThread::ResetConnection()
{
    SP_ZoneScopedN("ResetConnection");
    m_shmClient.reset(new server_shared_memory_client(m_scsynthPort));
    m_shmReader = m_shmClient->get_scope_buffer_reader(0);
}

void AudioProcessingThread::run()
{
    for (;;)
    {
        // We are done
        if (m_quit.load())
        {
            break;
        }

        auto startTime = std::chrono::high_resolution_clock::now();
        auto nextTime = startTime + std::chrono::milliseconds(int(1000.0f / ScopeRefreshRate));

        if (!m_running.load())
        {
            // Sleep for a second and try again
            std::this_thread::sleep_for(std::chrono::seconds(1));
            m_processedAudio.m_consumed.store(true);
            continue;
        }

        if (!m_shmReader.valid())
        {
            ResetConnection();
            if (!m_shmReader.valid())
            {
                // Not getting a connection, sleep for a second before trying again
                std::this_thread::sleep_for(std::chrono::seconds(1));
            }
            m_processedAudio.m_consumed.store(true);
            continue;
        }

        // If the GUI hasn't processed the last of our outputs, then yield our threads remaining slice.
        // We want to try again pretty soon, but we don't want to spin while the UI is doing its thing
        if (!m_processedAudio.m_consumed.load())
        {
            std::this_thread::yield();
            continue;
        }

        // Lock the data while we update it
        std::unique_lock<SP_LockableBase(std::mutex)> lock(m_processedAudio.m_mutex);
        {
            unsigned int frames;
            if (m_shmReader.pull(frames))
            {
                {
                    SP_ZoneScopedN("Shared Memory");
                    m_emptyFrames = 0;
                    float* data = m_shmReader.data();
                    for (unsigned int j = 0; j < 2; ++j)
                    {
                        unsigned int offset = m_shmReader.max_frames() * j;
                        for (unsigned int i = 0; i < FrameSamples - frames; ++i)
                        {
                            m_processedAudio.m_samples[j][i] = m_processedAudio.m_samples[j][i + frames];
                            if (j == 0)
                            {
                                m_processedAudio.m_monoSamples[i] = m_processedAudio.m_monoSamples[i + frames];
                            }
                        }

                        for (unsigned int i = 0; i < frames; ++i)
                        {
                            m_processedAudio.m_samples[j][FrameSamples - frames + i] = data[i + offset];
                            auto d = data[i + offset] + 1.0;
                            if (j == 0)
                            {
                                m_processedAudio.m_monoSamples[FrameSamples - frames + i] = d * d;
                            }
                            else
                            {
                                m_processedAudio.m_monoSamples[FrameSamples - frames + i] += d * d;
                                m_processedAudio.m_monoSamples[FrameSamples - frames + i] /= 2.0f;
                                m_processedAudio.m_monoSamples[FrameSamples - frames + i] = sqrt(m_processedAudio.m_monoSamples[FrameSamples - frames + i]) - 1.0;
                            }
                        }
                    }
                }

                CalculateFFT(m_processedAudio);

                m_processedAudio.m_consumed.store(false);

                // Tell the UI to update
                emit update();
            }
            else
            {
                ++m_emptyFrames;
                if (m_emptyFrames > 10)
                {
                    ResetConnection();
                    m_emptyFrames = 0;
                }
            }
        }
        lock.unlock();

        // Sleep until means we will still use the same frequency of update, regardless of how much time we took to do the processing
        std::this_thread::sleep_until(nextTime);
    }

    qDebug() << "Shutting down audio thread";
}

Scope::Scope(int scsynthPort, QWidget* parent)
    : QOpenGLWidget(parent)
    , m_scsynthPort(scsynthPort)
{
    QVBoxLayout* layout = new QVBoxLayout();
    layout->setContentsMargins(0, 0, 0, 0);
    setLayout(layout);

    m_panels.push_back({ "Lissajous", tr("Lissajous"), ScopeType::Lissajous });
    m_panels.push_back({ "Stereo", tr("Left"), ScopeType::Left });
    m_panels.push_back({ "Stereo", tr("Right"), ScopeType::Right });
    m_panels.push_back({ "Mono", tr("Mono"), ScopeType::Mono });
    m_panels.push_back({ "Mirror Stereo", tr("Mirror Stereo"), ScopeType::MirrorStereo });

    Panel spec({ "Spectrum", tr("Spectrum"), ScopeType::SpectrumAnalysis });
    spec.requireFFT = true;
    m_panels.push_back(spec);

    for (auto& scope : m_panels)
    {
        scope.pen = QPen();
        scope.pen.setWidth(PenWidth);
        scope.pen2 = QPen();
        scope.pen2.setWidth(PenWidth);
    }

    m_pAudioThread = new AudioProcessingThread(scsynthPort);
    m_pAudioThread->SetMaxBuckets(width() / 4);
    connect(m_pAudioThread, &AudioProcessingThread::update, this, &Scope::OnNewAudioData);
    m_pAudioThread->start();

    Layout();
}

void AudioProcessingThread::Enable(bool enable)
{
    m_running.store(enable);
}

void AudioProcessingThread::EnableFFT(bool enable)
{
    m_calculateFFT.store(enable);
}

Scope::~Scope()
{
}

void Scope::ShutDown()
{
    if (m_pAudioThread)
    {
        m_pAudioThread->Quit();
    }
    m_pAudioThread->wait();
    delete m_pAudioThread;
}

void Scope::resizeEvent(QResizeEvent* pSize)
{
    if (m_pAudioThread)
    {
        // Enure max buckets is every 4th pixel, so that on a small window things don't look bad
        m_pAudioThread->SetMaxBuckets(width() / 4);
    }
    QOpenGLWidget::resizeEvent(pSize);
    Layout();
}

// Draw a Simple Stereo representation with a mirror of right/left stereo
void Scope::DrawSpectrumAnalysis(const ProcessedAudio& audio, QPainter& painter, Panel& panel)
{
    if (audio.m_spectrumQuantized[0].empty() || audio.m_spectrumQuantized[1].empty())
        return;

    SP_ZoneScopedN("Draw Spectrum");
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
void Scope::DrawMirrorStereo(const ProcessedAudio& audio, QPainter& painter, Panel& panel)
{
    SP_ZoneScopedN("Draw Mirror Stereo");

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
void Scope::DrawWave(const ProcessedAudio& audio, QPainter& painter, Panel& panel)
{
    // Just sample the data at intervals; we should probably filter it too
    double step = FrameSamples / double(panel.rcGraph.width());

    // Make a list of points; it's better to gather them and submit in a batch
    // Here we are just drawing in pixel space
    // Note: resize will be a no-op when it doesn't change ;)
    panel.wavePoints.resize(panel.rcGraph.width());

    float yScale = float(panel.rcGraph.height() / 2.0f);
    int y = panel.rcGraph.center().y();

    const double* pSamples = nullptr;
    switch (panel.type)
    {
    case ScopeType::Left:
        pSamples = &audio.m_samples[0][0];
        break;
    case ScopeType::Right:
        pSamples = &audio.m_samples[1][0];
        break;
    case ScopeType::Mono:
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

void Scope::DrawLissajous(const ProcessedAudio& audio, QPainter& painter, Panel& panel)
{
    float yScale = float(panel.rcGraph.height() / 2.0f);
    int y = panel.rcGraph.center().y();

    float xScale = float(panel.rcGraph.width() / 2.0f);
    float scale = std::min(xScale, yScale);

    QPoint center = panel.rcGraph.center();
    panel.wavePoints.resize(LissajousSamples);
    for (int sample = 0; sample < LissajousSamples; sample++)
    {
        auto left = audio.m_samples[0][FrameSamples - LissajousSamples + sample];
        auto right = audio.m_samples[1][FrameSamples - LissajousSamples + sample];
        panel.wavePoints[sample] = center + QPoint(left * xScale, right * yScale);
    }
    painter.setPen(panel.pen);
    painter.drawPolyline(&panel.wavePoints[0], int(panel.wavePoints.size()));
}

void Scope::paintEvent(QPaintEvent* pEv)
{
    SP_ZoneScopedN("Draw Scopes");

    QPainter painter(this);

    auto backColor = QWidget::palette().color(QWidget::backgroundRole());
    auto textColor = QWidget::palette().color(QWidget::foregroundRole());
    auto shadowColor = QWidget::palette().color(QPalette::ColorRole::AlternateBase);

    painter.fillRect(rect(), backColor);

    auto& processedAudio = m_pAudioThread->GetCurrentProcessedAudio();
    std::unique_lock<SP_LockableBase(std::mutex)> lock(processedAudio.m_mutex);

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
            DrawLissajous(processedAudio, painter, panel);
        }
        else if (panel.type == ScopeType::Left || panel.type == ScopeType::Right || panel.type == ScopeType::Mono)
        {
            DrawWave(processedAudio, painter, panel);
        }
        else if (panel.type == ScopeType::MirrorStereo)
        {
            DrawMirrorStereo(processedAudio, painter, panel);
        }
        else if (panel.type == ScopeType::SpectrumAnalysis)
        {
            DrawSpectrumAnalysis(processedAudio, painter, panel);
        }
    }

    processedAudio.m_consumed.store(true);

    lock.unlock();

    SP_FrameMark;
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

    if (m_pAudioThread)
    {
        m_pAudioThread->EnableFFT(doFFT);
    }

    Layout();
    Refresh();
    return any ? on : true;
}

bool Scope::SetScopeLabels(bool on)
{
  for (auto& scope : m_panels)
  {
      scope.titleVisible = on;
  }
  Layout();
  Refresh();
  return on;
}

void Scope::ScsynthBooted()
{
    assert(m_pAudioThread);
    if (m_pAudioThread)
    {
        m_pAudioThread->Enable(true);
    }
}

void Scope::TogglePause()
{
    m_paused = !m_paused;
    m_pAudioThread->Enable(!m_paused);
}

void Scope::Pause()
{
    m_paused = true;
}

void Scope::Resume()
{
    m_paused = false;
}

void Scope::Refresh()
{
    repaint();
}

void Scope::SetColor(QColor c)
{
    for (auto& scope : m_panels)
    {
        scope.pen.setColor(c);
        scope.brush = QBrush(c);
    }
}

void Scope::SetColor2(QColor c)
{
    for (auto& scope : m_panels)
    {
        scope.pen2.setColor(c);
        scope.brush2 = QBrush(c);
    }
}

void Scope::OnNewAudioData()
{
    // short circuit if possible
    if (m_paused || !isVisible())
    {
        // Inform the audio thread that we don't want the data, so it doesn't get stuck waiting
        if (m_pAudioThread)
        {
            m_pAudioThread->GetCurrentProcessedAudio().m_consumed.store(true);
        }
        return;
    }

    Refresh();
}
