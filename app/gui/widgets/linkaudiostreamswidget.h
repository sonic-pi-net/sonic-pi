//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron.
// All rights reserved.
//++

#ifndef LINKAUDIOSTREAMSWIDGET_H
#define LINKAUDIOSTREAMSWIDGET_H

#include <QString>
#include <QVector>
#include <QWidget>
#include <memory>

class QTableWidget;
class QGridLayout;
class QLabel;
class QLineEdit;
class QPushButton;
class QSlider;
class QUdpSocket;
class QTimer;
class LinkVisibilityToggle;

namespace SonicPi { class SonicPiAPI; }

// Panel: peer-name field, Share-Audio toggle, and the list of Link Audio
// channels announced by other peers. Persists peer-name + share-audio to QSettings.
class LinkAudioStreamsWidget : public QWidget
{
    Q_OBJECT
public:
    // One row per (peer, channel): Live publishes several independently
    // subscribable channels (Main, 1, 2, …) under one peer name.
    struct PeerChannel {
        QString peerName;
        QString channelName;
        bool operator==(const PeerChannel& o) const {
            return peerName == o.peerName && channelName == o.channelName;
        }
    };

    // Mirrors SuperClock::LinkAudioConnectionState.
    enum ConnState { CS_NotSubscribed = 0, CS_Connecting = 1,
                     CS_Connected = 2, CS_Dropout = 3 };

    // One active Link Audio subscription, as reported by /clock/audio/inputs.reply.
    struct InputStatus {
        QString peerName;
        QString channelName;
        int     busIdx = -1;          // scsynth bus pair start; -1 = unknown.
        int     sampleRate = 0;       // Hz, 0 = unknown.
        int     numChannels = 0;      // source channels (1/2, 0 pre-buffer).
        float   bufferedMs = 0.0f;
        int     state = CS_NotSubscribed;
        float   latencySeconds = 0.0f;
    };

    explicit LinkAudioStreamsWidget(std::shared_ptr<SonicPi::SonicPiAPI> spAPI,
                                    QWidget* parent = nullptr);
    ~LinkAudioStreamsWidget() override;

    // Natural width of the identity controls row (Link Name / Latency / Stream
    // Audio / Visibility). Used to size the whole Link column so the peer table
    // and the metro row below match these controls rather than stretching wider.
    int controlsNaturalWidth() const;

public slots:
    // Called by SonicPiMetro when Visibility scope (Local/Net) changes.
    void applyMasterVisibility(int mode);
    // Called by SonicPiMetro when the Link button toggles.
    void applyLinkEnabled(bool enabled);

signals:
    // In-panel visibility toggle changed; MainWindow handles it via the
    // same path as the prefs radios (persist + propagate to SuperSonic + Tau).
    void requestNetworkVisibilityChange(int mode);

    // Announced peers / channels changed; MainWindow feeds these to the editor
    // autocompletion so link_audio can complete peer and channel names.
    void linkAudioStreamsChanged(const QStringList& peers, const QStringList& channels);

private slots:
    void refresh();
    void readPendingDatagrams();
    void onPeerNameEdited();
    void onShareAudioToggled(bool checked);
    void onLatencySliderChanged(int ms);

protected:
    void showEvent(QShowEvent* e) override;
    void hideEvent(QHideEvent* e) override;

private:
    std::shared_ptr<SonicPi::SonicPiAPI> m_spAPI;
    QLineEdit*            m_peerNameEdit = nullptr;
    QPushButton*          m_shareAudioBox = nullptr;  // checkable pill
    LinkVisibilityToggle* m_visibilityToggle = nullptr;
    QSlider*              m_latencySlider = nullptr;
    QLabel*               m_latencyValueLabel = nullptr;
    QTableWidget*         m_peersTable = nullptr;
    QGridLayout*          m_idGrid = nullptr;   // the identity controls row
    QUdpSocket*   m_socket = nullptr;
    QTimer*       m_pollTimer = nullptr;
    int           m_currentVisibility = 1;  // 1 = Local, 2 = Network
    bool          m_linkEnabled = false;

    QVector<PeerChannel> m_channels;
    // Active subscriptions, refreshed each poll from /clock/audio/inputs.reply.
    QVector<InputStatus> m_inputs;

    void renderPeersTable();
    // SuperSonic's latency contract is per-input; the GUI exposes one
    // engine-wide slider and reconciles every drifting input to it each poll.
    void enforceEngineLatency();
    void showEmptyMessage(const QString& text);
    // Briefly flash the empty-state message (e.g. when a muted control is clicked).
    void flashEmptyMessage();
};

#endif
