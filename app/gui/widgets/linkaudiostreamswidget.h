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

#include <QColor>
#include <QString>
#include <QVector>
#include <QWidget>
#include <memory>
#include <vector>

class QTableWidget;
class QGridLayout;
class QLabel;
class QLineEdit;
class QPushButton;
class QSlider;
class QTimer;
class LinkVisibilityToggle;

namespace SonicPi {
class SonicPiAPI;
struct LinkAudioChannelInfo;
struct LinkAudioInputInfo;
}

// Panel: peer-name field, Share-Audio toggle, and the list of Link Audio
// channels announced by other peers. Persists peer-name + share-audio to QSettings.
//
// It asks the engine through the API's command connection
// (SupersonicSendOSC) and the answers come back through the API's client
// (IAPIClient::LinkAudioChannels / LinkAudioInputs → MainWindow → onChannels /
// onInputs). Not through a socket of its own: the engine serves one command
// transport, and a datagram to the port the daemon calls "scsynth" reaches
// nothing when that transport is a stream — which is how this table stayed
// empty with peers in the session.
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

    // Push the accent (pink / white) into the in-panel visibility pill (it can't
    // reach the theme itself). Called by SonicPiMetro, which owns the theme.
    void setVisibilityColors(const QColor& thumb, const QColor& activeIcon);

public slots:
    // Called by SonicPiMetro when Visibility scope (Local/Net) changes.
    void applyMasterVisibility(int mode);
    // Called by SonicPiMetro when the Link button toggles.
    void applyLinkEnabled(bool enabled);
    // The engine's answers, via MainWindow: every channel other peers
    // announce, and our active subscriptions.
    void onChannels(const std::vector<SonicPi::LinkAudioChannelInfo>& channels);
    void onInputs(const std::vector<SonicPi::LinkAudioInputInfo>& inputs);

signals:
    // In-panel visibility toggle changed; MainWindow handles it via the
    // same path as the prefs radios (persist + propagate to SuperSonic + Tau).
    void requestNetworkVisibilityChange(int mode);

    // Announced peers / channels changed; MainWindow feeds these to the editor
    // autocompletion so link_audio can complete peer and channel names.
    void linkAudioStreamsChanged(const QStringList& peers, const QStringList& channels);

private slots:
    void refresh();
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
    QTimer*       m_pollTimer = nullptr;
    int           m_currentVisibility = 1;  // 1 = Local, 2 = Network
    bool          m_linkEnabled = false;

    // Announced channels, sorted by peer then channel, refreshed each poll.
    QVector<PeerChannel> m_channels;
    // Active subscriptions, refreshed each poll.
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
