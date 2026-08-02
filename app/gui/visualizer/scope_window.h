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
#include <QPen>
#include <QLine>
#include <QImage>
#include <QThread>
#include <QElapsedTimer>

#include <memory>
#include <string>
#include <vector>

#include "api/audio/server_shm.hpp"

class QTimer;

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
    SpectrumAnalysis,
    Levels
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
    std::vector<QLine> waveLines;
    std::vector<QRect> waveRects;
    QLinearGradient redBlueGradient;
};

class ScopePauseButton;

class ScopeWindow : public QWidget
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
    // Deferred pause: keep consuming until the audio is silent and the
    // spectrum ballistics have decayed, then Pause(). Lets tails ring
    // out visually instead of freezing the scope mid-image.
    void PauseWhenSilent();
    void Resume();
    bool IsPaused() const { return m_paused; }
    // Dock hidden/minimized: stop the engine-side audio processor entirely
    // (sample copies, FFT, ~60Hz cross-thread frames) without touching the
    // user-facing pause state, so showing the dock again picks up where the
    // pause button left it.
    void SetSuspended(bool suspended);
    void SetColor(QColor c);
    void SetColor2(QColor c);
    // Levels meter: the accent for the overdrive tip (the theme's attention
    // colour).
    void SetLevelHotColour(QColor hot);
    // Scope background (the faded-clear/phosphor colour). Set from the theme's
    // LogBackground so it's the dark content colour, not the window-chrome grey.
    void SetBackgroundColor(QColor c);
    // Pause/resume glyph colours: muted at rest, accent on hover — the same
    // flat treatment as the help-close and zoom glyphs.
    void SetPauseButtonColors(QColor rest, QColor hover);

    void DrawLevels(const ProcessedAudio& audio, QPainter& painter, ScopeWindowPanel& panel);
    void DrawWave(const ProcessedAudio& audio, QPainter& painter, ScopeWindowPanel& panel);
    void DrawMirrorStereo(const ProcessedAudio& audio, QPainter& painter, ScopeWindowPanel& panel);
    void DrawLissajous(const ProcessedAudio& audio, QPainter& painter, ScopeWindowPanel& panel);
    void DrawSpectrumAnalysis(const ProcessedAudio& audio, QPainter& painter, ScopeWindowPanel& panel);

    void Booted();
    void ShutDown();

    // The pause/resume toggle. Displayed in the scope dock's title row (placed
    // there by MainWindow) rather than overlaid on the trace, so it sits beside
    // the SCOPE title.
    QWidget* PauseButton() const;

signals:
    // Emitted whenever the running/frozen state actually flips, from any
    // path: F12, the Visuals menu, the overlay button, or pause-when-silent.
    void PausedChanged(bool paused);

private slots:
    void OnConsumeAudioData(SonicPi::ProcessedAudioPtr audio);

public slots :
    void Refresh();

protected:
    virtual void paintEvent(QPaintEvent* pEv) override;
    virtual void resizeEvent(QResizeEvent* pSize) override;
    // Widget visibility feeds WantsProcessor: a hidden instance (prefs pane
    // closed, dock collapsed, window minimised) must not keep the global
    // audio processor running on its behalf.
    virtual void showEvent(QShowEvent* e) override;
    virtual void hideEvent(QHideEvent* e) override;

private:
    void Layout();
    bool SnapshotSilent(const ProcessedAudio& audio) const;
    // True while a visible Levels panel still has meter state above the
    // floor — bars, peak-hold or gain reduction that hasn't run down yet.
    // The ballistics only advance per paint, so the settle machinery must
    // keep repainting until this clears.
    bool LevelsRunningDown() const;
    // A zeroed snapshot matching the current one's geometry: what the settle
    // repaints draw so the phosphor trail fades to a flat line rather than
    // re-burning the last waveform (empty/missing frames leave m_audio stale).
    ProcessedAudioPtr MakeSilentSnapshot() const;
    // Finishes the settle fade if audio frames stop arriving mid-decay (the
    // engine pauses itself once silent, taking the frame stream with it).
    void SettleTick();
    // Snap every panel to its rest state: silent snapshot, Levels ballistics
    // at the floor, phosphor trail cleared opaquely. Called when the feed
    // (re)starts — unpause, unsuspend, a panel newly shown — so the scopes
    // wake at rest instead of at whatever the last trace showed. Necessary
    // because a silent engine sends no frames, which would otherwise leave
    // the stale image on screen indefinitely.
    void ResetToRest();
    // Single authority for AudioProcessor_Enable: on only when a panel is
    // visible, not user-paused and not suspended.
    void ApplyProcessorEnable();
    // Single authority for AudioProcessor_EnableFFT and SetMaxFFTBuckets,
    // aggregated across instances the same way as ApplyProcessorEnable.
    void ApplyFFTSettings();
    // Whether this instance alone wants the audio processor running.
    bool WantsProcessor() const;
    // Every live instance, so ApplyProcessorEnable can OR across them.
    static QList<ScopeWindow*> s_instances;


private:
    std::shared_ptr<SonicPiAPI> m_spAPI;
    std::shared_ptr<QtAPIClient> m_spClient;
    std::vector<ScopeWindowPanel> m_panels;
    bool m_paused = false;
    bool m_pendingPause = false;
    bool m_suspended = false;
    // This instance's FFT resolution ask, set by Layout() from its panel
    // width; ApplyFFTSettings takes the max across instances.
    int m_fftBucketsWanted = 0;

    // Levels panel: stereo RMS/peak ballistics state, advanced each paint
    // (the pane repaints per audio frame; the settle window animates the
    // final decay before pause-when-silent freezes it).
    struct LevelChannel
    {
        float rmsDb = -60.0f;
        float peakDb = -60.0f;
        float holdDb = -60.0f;
        qint64 holdUntilMs = 0;
    };
    LevelChannel m_levels[2];
    qint64 m_levelLastMs = 0;
    QElapsedTimer m_levelClock;
    QColor m_levelHot = QColor(255, 20, 147);

    // Overdrive / gain reduction: the mixer taps its limiter input into a
    // reserved scope slot (see etc/synthdefs/designs/supercollider/mixer.scd).
    // How far that input pushes past the ceiling is what the limiter must
    // take off — drawn as the bar's tip crossing into the hot zone.
    shm_scope_stream_reader m_preLimReader;
    uint64_t m_preLimLastEnd = 0;
    std::vector<float> m_grScratch;
    float m_grTargetDb = 0.0f;    // latest window's dB past the ceiling
    float m_grDb = 0.0f;          // displayed, bar ballistics, always >= 0
    bool m_grAvailable = false;   // false until the tap produces a window
    // Pause/resume toggle (hosted in the scope dock title row); also freezes the
    // image for inspection (waveform shapes, spectrum peaks).
    ScopePauseButton* m_pauseButton = nullptr;
    // Latest snapshot from the audio processor; never null after the
    // constructor seeds it. Slot and paintEvent both run on the GUI
    // thread, so no lock is needed.
    ProcessedAudioPtr m_audio;
    uint32_t m_audioFrameSamples = 0;
    std::atomic<bool> m_audioAvailable = false;
    // Persistence/decay: normally the background is cleared with a low alpha so
    // the previous trace fades a little each frame (a soft phosphor trail).
    // m_fullClear forces one opaque clear on the first paint and after a resize,
    // where the preserved framebuffer would otherwise hold stale/garbage pixels.
    bool m_fullClear = true;
    QColor m_backColor{ Qt::black };   // theme LogBackground; set in SetBackgroundColor
    // Offscreen phosphor-trail buffer (owned + DPI-scaled). The decay lives here
    // rather than the widget backing store, which Qt doesn't preserve on macOS.
    QImage m_trail;
    // Consecutive silent frames painted since the signal stopped. Caps idle
    // repaints at SilentSettleFrames so the scope stops repainting once the
    // trail has decayed; reset to 0 when signal returns.
    int m_silentFrames = 0;
    // Watchdog for the settle fade: re-armed on every delivered frame, fires
    // only when delivery stops before the trail has fully decayed.
    QTimer* m_settleTimer = nullptr;
    // Frame-path repaints ride the shared FramePacer (~30 Hz) instead of
    // repainting per delivered ~60 Hz audio frame: newest frame wins, and the
    // repaint lands in the same composite pass as the other paced widgets.
    void requestFrameRepaint();
    bool m_repaintPending = false;
    bool m_pacerHeld = false;
};

} // namespace SonicPi
