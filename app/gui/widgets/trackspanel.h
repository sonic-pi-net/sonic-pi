//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright (C) 2026 by Sam Aaron
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#ifndef TRACKSPANEL_H
#define TRACKSPANEL_H

#include <QHash>
#include <QSet>
#include <QString>
#include <QTimer>
#include <QVector>
#include <QWidget>
#include <cstdint>
#include <memory>

// TrackInfo and TrackParamInfo are value types carried by the callbacks
// below, so the header is needed here rather than a forward declaration.
#include "api/sonicpi_api.h"
#include "utils/trackparam.h"

class QButtonGroup;
class QComboBox;
class QGridLayout;
class QHBoxLayout;
class QLabel;
class QLineEdit;
class QListWidget;
class QListWidgetItem;
class QPushButton;
class QScrollArea;
class QVBoxLayout;
class CardScope;
class ThinSplitter;
class TutDial;
class SonicPiTheme;

namespace SonicPi
{
class SonicPiAPI;
}

/*
 * TracksPanel — the plugin host's tracks, a top-level south tab beside Cards,
 * Docs, Logs and Debug.
 *
 * A TRACK IS STUDIO STATE THAT CODE NAMES. It is made here, given a name, and
 * a chain of VST3 or CLAP plugins is arranged on it; the code that plays it
 * says `live_track :surge`, `with_send :verb do ... end` or
 * `use_track :surge` and `track_midi :e3`, and never mentions a plugin. That split is the
 * whole design (harness/docs/TRACKS.md): a plugin's identity is a path on
 * one machine, and a name is the thing worth typing.
 *
 * THREE COLUMNS, THE WAY EVERY DEVICE VIEW HAS ENDED UP. A browser of what
 * is installed, the list of tracks, and the chain of whichever track is
 * selected running left to right in the order the audio does. Live's device
 * view, Bitwig's, Reaper's FX chain: the arrangement is the same because the
 * problem is the same. The chain is the thing you look at most, so it gets
 * the width.
 *
 * THE CODE IS ON THE PANEL. Under the track's name sit the lines of code that
 * reach it, highlighted the way the editor would show them, with copy and
 * insert-at-cursor beside each. A track you cannot remember how to address
 * from code is a track you will not use, and Sonic Pi's own idiom is that the
 * interface teaches the language.
 *
 * ONE INSTANCE OF EACH PLUGIN, AND NONE OF THEM HERE. The engine's plugin
 * process hosts the plugins that make sound and owns their editor windows
 * (a native view can only be parented in the process that created it). It
 * also does the SCANNING: a scan runs every installed plugin's own code,
 * which is seconds of work and the likeliest place for one to crash, and
 * that process is the one built to be crashed — it comes back with its
 * tracks. This panel drives it all over OSC and draws its own controls;
 * no plugin code ever runs in the GUI.
 *
 * CURATED, NOT TRUNCATED. Surge XT publishes 2855 parameters. A device starts
 * with no controls and gains the ones you reach for: turn Configure on, move a
 * control in the plugin's own window, and it appears on the device. The set is
 * per plugin and remembered between sessions.
 */
class TracksPanel : public QWidget
{
    Q_OBJECT

public:
    explicit TracksPanel(std::shared_ptr<SonicPi::SonicPiAPI> spAPI, QWidget* parent = nullptr);
    ~TracksPanel() override;

    void applyTheme(SonicPiTheme* theme);

    // Pane-local text zoom, the A-/A+ steps every help tab offers. The
    // buttons are the dock title row's (MainWindow's ZoomBar, beside the
    // other tabs' pairs); the level is a pref MainWindow persists.
    int userZoom() const { return m_userZoom; }
    void setUserZoom(int zoom);

    // There is an engine to talk to: ask it what it has. Called on first
    // boot and again after every engine restart.
    void onEngineReady();

    // The engine's picture of its tracks, after any change. The panel keeps
    // no state of its own that this does not overwrite.
    void onTracks(int laneBase, const std::vector<SonicPi::TrackInfo>& tracks);
    void onTrackState(int id, float gain, bool mute);
    void onTrackFolders(const std::vector<std::string>& extra,
                        const std::vector<std::string>& platform);
    // What a scan found, a page at a time: the browser's contents once the
    // page that reaches `total` has come.
    void onTrackPlugins(unsigned int total, unsigned int offset,
                        const std::vector<SonicPi::TrackPluginInfo>& plugins);
    // One page of a plugin's parameters, from the instance that makes sound.
    void onTrackParams(int handle, uint32_t total, uint32_t offset,
                       const std::vector<SonicPi::TrackParamInfo>& params);
    // A plugin reported one of its OWN edits — a knob turned in its window.
    // Moves the matching dial, and with Configure on, adds the parameter.
    // `own`: the plugin's editor was used (a gesture: with Configure armed it
    // puts the control on the face); otherwise code set it by name, which
    // the face follows and nothing more.
    void onTrackParamEdit(int handle, uint32_t id, double normalized, bool own);
    // Something the user asked for did not happen. `handle` names the plugin
    // it concerns, 0 when none.
    void onTrackError(const QString& verb, const QString& detail, int handle);

    // Ask the engine to write its tracks where they will be found next time.
    void saveCurrentRig();

signals:
    // Same contract as the quickstart cards: the window owns the editor and
    // the status line, so the panel asks rather than reaching.
    void insertRequested(const QString& title, const QString& code);
    void copyRequested(const QString& title, const QString& code);
    void announceRequested(const QString& message);
    // The parameters of everything running on `track`, in chain order, with
    // their ranges and values; sent after any change to the chain or arrival
    // of a plugin's list. The editor completes the track verbs' opts and
    // track_control's parameter slot from it.
    void trackParamsChanged(const QString& track, const QList<SonicPi::TrackParam>& params);

protected:
    bool eventFilter(QObject* watched, QEvent* event) override;

private slots:
    void onRescan();
    void onFilterChanged();
    void onAddSelectedPlugin();
    void onNewTrack();
    void onTrackSelectionChanged();
    void onTrackItemEdited(QListWidgetItem* item);
    void onRemoveTrack();
    void onSaveRig();
    void onLoadRig();
    void onFolders();

private:
    // One parameter shown on a device's face. Only ever a curated one.
    struct ParamRow
    {
        uint32_t id = 0;
        QString  group;
        TutDial* dial = nullptr;
    };

    // One plugin found on disk, as offered in the browser.
    struct Discovered
    {
        QString name;
        QString vendor;
        QString path;
        uint32_t index = 0;
        bool isInstrument = false;
    };

    // The engine's parameter list for one running instance, filled in pages.
    struct ParamList
    {
        QHash<uint32_t, SonicPi::TrackParamInfo> byId;
        QVector<uint32_t> order;     // as the plugin declares them
        uint32_t total = 0;
        uint32_t got = 0;
        bool requested = false;
        bool complete() const { return requested && got >= total; }
    };

    // One node on the selected track, and the device drawn for it.
    struct Device
    {
        SonicPi::TrackNodeInfo info;

        QWidget*     frame = nullptr;
        QLabel*      title = nullptr;
        QLabel*      status = nullptr;
        QLabel*      hint = nullptr;
        QGridLayout* paramGrid = nullptr;
        QPushButton* bypassBtn = nullptr;
        QPushButton* editorBtn = nullptr;
        QPushButton* configureBtn = nullptr;
        QPushButton* leftBtn = nullptr;
        QPushButton* rightBtn = nullptr;
        QPushButton* removeBtn = nullptr;
        QComboBox*   channel = nullptr;     // instruments: the listen channel

        bool configuring = false;
        QVector<ParamRow> rows;
        // Curated ids the face does not show yet, because the parameter list
        // has not arrived: realised as pages come in.
        QVector<uint32_t> pending;
    };

    void buildUi();
    void applySizing();
    void applyFontsIn(QWidget* root);
    int  deviceWidth() const;
    void fitDeviceTitle(Device* dev);
    void fitColumns();
    QWidget* buildBrowserColumn();
    QWidget* buildTracksColumn();
    QWidget* buildChainArea();
    void rebuildBrowser();
    void rebuildTrackList();
    void showTrack(int id);
    void refreshHeader();
    void scopeSized();
    void rebuildChain();
    QWidget* buildDeviceFrame(Device* dev);
    void destroyDevice(Device* dev);
    void relayoutParams(Device* dev);
    void requestParams(int handle);
    void publishParamNames();
    void requestParamsPage(int handle, uint32_t offset);
    void realisePending(Device* dev);
    void addCuratedParam(Device* dev, uint32_t id, bool persist);
    void setDeviceStatus(Device* dev, const QString& text, bool ok);
    QString deviceStatusText(const Device* dev) const;
    void refreshDeviceButtons(Device* dev, int index, int count);
    void tintDeviceGlyph(QPushButton* b, bool hover);
    void setConfiguring(Device* dev, bool on);
    void sendParamFromDial(const Device* dev, uint32_t id);
    void setEditorVisible(Device* dev, bool show);
    void setListenChannel(Device* dev, int channel);
    void rememberPlugins() const;
    void recallPlugins();
    void toggleConfigure(Device* dev);
    void setStatus(const QString& text, bool ok);
    void clearStatus();
    void addPluginToTrack(const Discovered& d);
    Device* deviceFor(int handle);
    const SonicPi::TrackInfo* selectedTrack() const;
    const SonicPi::TrackInfo* trackById(int id) const;
    QString uniqueTrackName() const;
    QString trackNameOrEmpty() const;
    QString rigsDir() const;
    QString currentRigPath() const;
    void setCodeLine(QLabel* label, const QString& code);

    QVector<uint32_t> savedCuration(const SonicPi::TrackNodeInfo& node) const;
    void saveCuration(const Device* dev) const;

    std::shared_ptr<SonicPi::SonicPiAPI> m_spAPI;
    SonicPiTheme* m_theme = nullptr;
    ThinSplitter* m_split = nullptr;
    int       m_userZoom = 0;
    double    m_fontScale = 1.0;   // FontZoomFactor(m_userZoom), cached
    QLabel*   m_browserCaption = nullptr;
    QLabel*   m_tracksCaption = nullptr;

    // Browser
    QListWidget* m_browser = nullptr;
    QLineEdit*   m_filter = nullptr;
    QButtonGroup* m_kindFilter = nullptr;   // All / Instruments / Effects
    QPushButton* m_addButton = nullptr;
    QPushButton* m_rescanButton = nullptr;
    QPushButton* m_foldersButton = nullptr;
    QLabel*      m_browserCount = nullptr;
    QVector<Discovered> m_found;
    QVector<Discovered> m_arriving;      // a scan's pages so far
    bool m_scanning = false;
    bool m_foundIsRecalled = false;  // m_found is last session's, not this engine's
    bool m_scanWanted = false;       // asked before there was an engine to ask
    bool m_engineUp = false;
    QStringList m_extraFolders;
    QStringList m_platformFolders;

    // Tracks
    QListWidget* m_trackList = nullptr;
    QPushButton* m_newTrackButton = nullptr;
    QPushButton* m_saveRigButton = nullptr;
    QPushButton* m_loadRigButton = nullptr;
    std::vector<SonicPi::TrackInfo> m_tracks;
    int  m_laneBase = 0;
    int  m_selectedId = 0;
    bool m_haveList = false;
    bool m_restoreTried = false;
    bool m_userEmptied = false;      // the last track went because we removed it
    bool m_selectNewest = false;     // a create is in flight: select what appears
    bool m_rebuildingList = false;
    bool m_bridgeDown = false;       // the plugin process died; its tracks are coming back

    // The selected track's header
    QWidget*     m_header = nullptr;
    CardScope*   m_scope = nullptr;      // the track's return, as the quickstart cards draw theirs
    int          m_scopeTrack = 0;       // the track the scope is on, and
    int          m_scopeSlot = -1;       // the stream it reads for it
    QLabel*      m_trackTitle = nullptr;
    QLabel*      m_trackDetail = nullptr;
    TutDial*     m_ampDial = nullptr;
    QPushButton* m_muteButton = nullptr;
    QPushButton* m_removeTrackButton = nullptr;
    QLabel*      m_codeLive = nullptr;
    QLabel*      m_codeSend = nullptr;
    QLabel*      m_codeNote = nullptr;
    QWidget*     m_codeSendRow = nullptr;
    QWidget*     m_codeNoteRow = nullptr;

    // The chain
    QLabel*      m_emptyLane = nullptr;
    QLabel*      m_noTracks = nullptr;
    QWidget*     m_codeBlock = nullptr;
    QHBoxLayout* m_lane = nullptr;
    QScrollArea* m_laneScroll = nullptr;
    QLabel*      m_status = nullptr;
    QTimer*      m_statusTimer = nullptr;
    static constexpr int kStatusMs = 6000;
    // What the status line says once the engine's next track list lands:
    // a "Loading…" that is never followed by "loaded" reads as still loading.
    QString      m_statusOnNextList;
    QWidget*     m_browserHost = nullptr;
    QWidget*     m_tracksHost = nullptr;
    QVector<Device*> m_devices;
    // By handle, for every plugin on every track: asked for as soon as a
    // plugin appears, since the editor's completion wants the names whether
    // or not the track is the one on show.
    QHash<int, ParamList> m_params;
};

#endif // TRACKSPANEL_H
