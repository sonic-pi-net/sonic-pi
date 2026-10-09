//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#pragma once

#include <fstream>
#include <memory>
#include <vector>

#include <QDate>
#include <QFuture>
#include <QIcon>
#include <QJsonObject>
#include <QMainWindow>

#include "model/helppanelmodel.h"
#include "model/sidecolumnmodel.h"
#include <QSet>
#include <QSettings>

// On windows, we need to include winsock2 before other instances of winsock
#ifdef WIN32
#include <winsock2.h>
#endif

#include <api/sonicpi_api.h>
#include "api/osc/osc_pkt.hh"

#include "config.h"
#include "utils/announcementpolicy.h"
#include "utils/tutorialdocs.h"

class QAction;
class QActionGroup;
class QMenu;
class QToolBar;
class QLineEdit;
class QsciScintilla;
class QProcess;
class QTextEdit;
class QTextBrowser;
class QString;
class QSlider;
class QSplitter;
class QPushButton;
class ChevronButton;
class DividerOverlay;
class QStackedWidget;
class ThinSplitter;
class TutorialPane;
class IconTabWidget;

namespace SonicPi
{
class QtAPIClient;
class SonicPiAPI;
class ScopeWindow;
} // namespace SonicPi

class QShortcut;
class QAccessibilityHints;
class QDockWidget;
class QListWidget;
class QListWidgetItem;
class QSignalMapper;
class QTabWidget;
class QCheckBox;
class QVBoxLayout;
class SplashWidget;
class QLabel;
class QWebEngineView;

class InfoWidget;
class SettingsWidget;
class Scope;
class ScintillaAPI;
class SonicPii18n;
class SonicPiLog;
class SonicPiScintilla;
class SonicPiErrorCard;
class SonicPiEditor;
class SonicPiTheme;
class SonicPiToolTipManager;
class SonicPiLexer;
class SonicPiSettings;
class SonicPiContext;
class SonicPiMetro;
class LogPanel;
class MetricsPanel;
class TracksPanel;
class QuickstartPane;
class ZoomBar;

struct help_page
{
    QString title = "";
    QString keyword = "";
    QString url = "";
};

// Fixed order of the help nav tabs, as built by initDocsWindow (ruby_help.h)
enum class DocTab
{
    Tutorial = 0,
    Examples,
    Synths,
    Fx,
    Samples,
    Lang
};

struct help_entry
{
    int pageIndex;
    int entryIndex;
};

class MainWindow;

// One row of the keyboard-shortcut catalogue: a stable id, a translatable
// description, the default key string for each built-in mode, and the QAction
// it drives. Single source of truth for the loaders, the apply loop and the
// prefs editor.
struct ShortcutDef
{
    const char* id = nullptr;
    const char* desc = nullptr;
    const char* mac = nullptr;
    const char* win = nullptr;
    const char* emacs = nullptr;
    const char* group = nullptr;
    QAction* MainWindow::* act;
    const char* secondary = nullptr;  // optional extra shortcut(s), comma separated (undocumented fallback), all keymaps
};

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QApplication& ref, SplashWidget* splash);

    static const QList<ShortcutDef>& shortcutDefs();

    // Converts Sonic Pi shortcut notation ("Meta+R", "ShiftMeta+S", …) into a
    // QKeySequence. Static so the prefs editor can render shortcuts natively.
    static QKeySequence resolveShortcut(QString keySequence);

    SonicPiLog* GetOutputPane() const;
    SonicPiLog* GetIncomingPane() const;
    SonicPiTheme* GetTheme() const;

    void addCuePath(QString path, QString val);
    // True when the job ran from an editor buffer (workspace_*), not the help
    // system's playground/example/card workspaces. Jobs with no recorded
    // workspace count as editor runs.
    bool jobRanFromEditor(int jobId) const
    {
        return !m_jobWorkspaces.contains(jobId)
               || m_jobWorkspaces.value(jobId).startsWith(QLatin1String("workspace_"));
    }
    void setLineMarkerinCurrentWorkspace(int num, bool isSyntaxError, const QString& errorToken, int colStart, int colEnd);
    void showError(QString msg);
    // Runtime/syntax errors as a native card (rounded, themed, with a jump button).
    void showErrorCard(bool isSyntax, const QString& header, const QString& location,
                       const QString& reason, const QString& codeLine, int lineNumber,
                       int colStart, int colEnd, const QString& backtrace, bool canJump);
    void jumpToError();
    void dismissErrorCard();
    // Shrink the help dock when the central area is too short to show an
    // error (card/pane) plus a useful strip of editor; the height goes back
    // when the error is dismissed (unless the user re-sized meanwhile).
    void stealHelpHeightForError(int errorH);
    void returnStolenHelpHeight();
    void replaceBuffer(QString id, QString content, int line, int index, int first_line);
    void replaceBufferIdx(int buf_idx, QString content, int line, int index, int first_line);
    void setUpdateInfoText(QString t);
    void allJobsCompleted();
    void updateVersionNumber(QString version, int version_num, QString latest_version, int latest_version_num, QDate last_checked_date, QString platform);
    void updateMIDIInPorts(QString port_info);
    void updateMIDIOutPorts(QString port_info);
    void updateGamepadDevices(QString devices);
    // The plugin host's tracks: everything below goes to the Tracks panel,
    // which is the only thing that cares.
    void updateTracks(int laneBase, const std::vector<SonicPi::TrackInfo>& tracks);
    void updateTrackState(int id, float gain, bool mute);
    void updateTrackFolders(const std::vector<std::string>& extra,
                            const std::vector<std::string>& platform);
    // Link Audio, for the streams panel in the metro pane.
    void updateLinkAudioChannels(const std::vector<SonicPi::LinkAudioChannelInfo>& channels);
    void updateLinkAudioInputs(const std::vector<SonicPi::LinkAudioInputInfo>& inputs);
    void updateTrackPlugins(unsigned int total, unsigned int offset,
                            const std::vector<SonicPi::TrackPluginInfo>& plugins);
    void updateTrackParams(int handle, unsigned int total, unsigned int offset,
                           const std::vector<SonicPi::TrackParamInfo>& params);
    void updateTrackParamEdit(int handle, unsigned int id, double normalized, bool own);
    void updateTrackError(QString verb, QString detail, int handle);
    void updateScsynthInfo(QString description);
    void updateAudioDevices(const SonicPi::AudioDevicesInfo& devicesInfo);
    void updateAudioInputDevices(const SonicPi::AudioInputDevicesInfo& devicesInfo);
    void updateAudioDeviceTable(const SonicPi::AudioDeviceTableInfo& table);
    void updateAudioDeviceConfig(const SonicPi::AudioDeviceConfigInfo& configInfo);
    void homeDirWriteError();
    void replaceLines(QString id, QString content, int first_line, int finish_line, int point_line, int point_index);
    void runBufferIdx(int idx);

    bool loaded_workspaces;
    QSet<QString> initialWorkspaceLoads;
    QString pendingSetPath;
    QString currentSetPath;
    QJsonObject currentSetMeta;   // the set's meta as loaded, kept on save (SetBundle::Load::meta)
    QString hash_salt;
    QString ui_language;

public slots:
    void switchAudioDriver(QString driver);
    void switchAudioDevice(QString device);
    void switchAudioInputDevice(QString device);
    void switchAudioDeviceAndInput(QString device, QString input);
    void changeSampleRate(int rate);
    void onSupersonicSetup(int sampleRate, int bufferSize);
    void onSpiderReady();
    void onMixerSettings(double drive, double outputVolume);
    void onAudioSwitchDone(const SonicPi::AudioSwitchOutcome& outcome);
    void onAudioDeviceReopenReply(bool accepted, QString reason);
    void onAudioStateChanged(const QString& state, const QString& reason);   // the engine's own fallback: say where the sound went
    void changeBufferSize(int size);
    void resetAudioDevice();

private:
    // Empty inputDevice = leave SuperSonic's current input unchanged
    void sendDeviceSwitch(QString device, int sampleRate, int bufferSize,
                          QString inputDevice = QString());

    // One-shot restore of QSettings audio intent — fires after all three
    // initial /supersonic/* broadcasts have landed. The driver goes first
    // (a switch cascades a device re-open), so the restore is two-staged:
    // m_audioDriverRestoreSent marks the driver switch dispatched, and the
    // device/rate/buffer pass runs on a later broadcast once the engine
    // has reported back.
    bool m_audioIntentRestored = false;
    bool m_audioDriverRestoreSent = false;
    // The saved device prefs were ignored this launch because the previous
    // startup never finished (see audioBootDecision); say so once we are up.
    bool m_audioPrefsSkippedAtBoot = false;
    bool m_audioDevicesSeen = false;
    bool m_announceDeviceAfterRollback = false;   // the next device list names where the engine's fallback went
    bool m_audioInputDevicesSeen = false;
    bool m_audioDeviceConfigSeen = false;
    SonicPi::AudioDevicesInfo      m_lastAudioDevices;
    SonicPi::AudioInputDevicesInfo m_lastAudioInputDevices;
    SonicPi::AudioDeviceTableInfo  m_lastAudioDeviceTable;
    SonicPi::AudioDeviceConfigInfo m_lastAudioDeviceConfig;
    // Device output latency in ms; visuals (flash, inline scopes) are
    // delayed by this to align with the audible sound.
    int m_visualLatencyMs = 0;
    void maybeRestoreAudioIntent();

    // Audio device/rate/buffer intent awaiting engine confirmation.
    // Prefs are persisted in onAudioSwitchDone once the engine reports
    // the switch actually succeeded — never at request time.
    struct PendingAudioPrefs
    {
        QString output;
        QString input;
        int sampleRate = 0;
        int bufferSize = 0;
        bool outputFollowsDefault = false;   // the default-follow row was picked
    };
    PendingAudioPrefs m_pendingAudioPrefs;

protected:
    void closeEvent(QCloseEvent* event) override;
    void wheelEvent(QWheelEvent* event) override;

signals:
    void settingsChanged();

private slots:

    // Starts the splash intro, then the deferred daemon-ready poll.
    void completeBoot();
    void beginServerReadyPoll();
    // One tick of that poll; hands off to onServerReady() or errors out.
    void pollServerReady();
    // Finalisation once the daemon has reached the ready state.
    void onServerReady();

    void updateSelectedUILanguageAction(QString lang);
    void updateContext(int line, int index);
    void updateContextWithCurrentWs();
    void docLinkClicked(const QUrl& url);
    void handleCustomUrl(const QUrl& url);
    void zoomInLogs();
    void zoomOutLogs();
    QString sonicPiHomePath();
    QString sonicPiConfigPath();
    QString shortcutsConfigPath();
    void updateLogAutoScroll();
    bool eventFilter(QObject* obj, QEvent* evt) override;
    QString asciiArtLogo();
    void printAsciiArtLogo();
    void runCode();
    void update_check_updates();
    void mixerSettingsChanged();
    void check_for_updates_now();
    void enableCheckUpdates();
    void disableCheckUpdates();
    void stopCode();
    void beautifyCode();
    void completeSnippetListOrIndentLine(QObject* ws);
    void completeSnippetOrIndentCurrentLineOrSelection(SonicPiScintilla* ws);
    void setMarkInCurrentWorkspace();
    void triggerAutocompleteInCurrentWorkspace();
    void readCompletionDetailsInCurrentWorkspace();
    void toggleCommentInCurrentWorkspace();
    void transposeCharsInCurrentWorkspace();
    void moveLineOrSelectionUpInCurrentWorkspace();
    void moveLineOrSelectionDownInCurrentWorkspace();
    void forwardOneLineInCurrentWorkspace();
    void backOneLineInCurrentWorkspace();
    void forwardTenLinesInCurrentWorkspace();
    void backTenLinesInCurrentWorkspace();
    void cutLineFromPointInCurrentWorkspace();
    void copyInCurrentWorkspace();
    void cutInCurrentWorkspace();
    void pasteInCurrentWorkspace();
    void rightInCurrentWorkspace();
    void showFindInCurrentWorkspace();
    void findNextInCurrentWorkspace();
    void findPrevInCurrentWorkspace();
    void leftInCurrentWorkspace();
    void deleteForwardInCurrentWorkspace();
    void deleteBackwardInCurrentWorkspace();
    void upcaseWordOrSelectionInCurrentWorkspace();
    void downcaseWordOrSelectionInCurrentWorkspace();
    void lineStartInCurrentWorkspace();
    void lineEndInCurrentWorkspace();
    void documentStartInCurrentWorkspace();
    void documentEndInCurrentWorkspace();
    void wordRightInCurrentWorkspace();
    void wordLeftInCurrentWorkspace();
    void selectLineStartInCurrentWorkspace();
    void selectLineEndInCurrentWorkspace();
    void selectWordRightInCurrentWorkspace();
    void selectWordLeftInCurrentWorkspace();
    void selectDocStartInCurrentWorkspace();
    void selectDocEndInCurrentWorkspace();
    void applyUserShortcuts(const QString& base, const QMap<QString, QString>& keys);
    void centerCaretInCurrentWorkspace();
    void undoInCurrentWorkspace();
    void redoInCurrentWorkspace();
    void selectAllInCurrentWorkspace();
    void deleteWordRightInCurrentWorkspace();
    void deleteWordLeftInCurrentWorkspace();

    void toggleComment(SonicPiScintilla* ws);
    void stopRunningSynths();
    void mixerInvertStereo();
    void mixerStandardStereo();
    void mixerMonoMode();
    void mixerStereoMode();
    void mixerLpfEnable(float freq);
    void mixerHpfEnable(float freq);
    void mixerHpfDisable();
    void mixerLpfDisable();
    QString currentTabLabel();
    bool loadFile();
    bool saveAs();
    bool loadSet();
    bool saveSet();
    bool saveSetAs();
    bool saveSetToPath(const QString& path);
    void clearAllBuffers();
    void loadSetFromFile(const QString& path);
    void rememberRecentSet(const QString& path);
    void updateRecentSetsMenu();
    // OS open-file requests (Finder double-click, argv); deferred until the
    // workspaces have loaded so the replace confirm can still fire.
    void openSetPath(const QString& path);
    bool confirmAction(const QString& text, const QString& informativeText, const QString& confirmLabel);
    void about();
    void scope();
    void toggleScope();
    void showScopeLabelsMenuChanged();
    void toggleIcons();
    void help();
    void toggleHelpIcon();
    void onExitCleanup();
    void restartApp();
    void toggleRecording();
    void toggleRecordingOnIcon();
    void changeSystemVolume(int val, int silent = 0);
    void changeSystemDrive(int pct, int silent = 0);
    void changeGUITransparency(int val);
    void changeShowLineNumbers();
    void showLineNumbersMenuChanged();
    void showAutoCompletionMenuChanged();
    void showCompletionHelpMenuChanged();
    void audioSafeMenuChanged();
    void changeAudioSafeMode();
    void changeMidiDefaultChannel();
    void midiDefaultChannelMenuChanged(int idx);
    void audioTimingGuaranteesMenuChanged();
    void changeAudioTimingGuarantees();
    void enableExternalSynthsMenuChanged();
    void changeEnableExternalSynths();
    void mixerInvertStereoMenuChanged();
    void mixerForceMonoMenuChanged();
    void enableScsynthInputsMenuChanged();
    void uncheckEnableLinkMenu();
    void checkEnableLinkMenu();
    void toggleLinkMenu();
    void changeEnableScsynthInputs();
    void midiEnabledMenuChanged();
    void gamepadEnabledMenuChanged();
    void changeShowAutoCompletion();
    void changeShowCompletionHelp();
    void changeShowContext();
    void changeSpeakTransport();
    void changeReduceMotion();
    void showContextMenuChanged();
    void flashCodeMenuChanged();
    void flashGutterMenuChanged();
    void showLoopScopesMenuChanged();
    void loopScopeScrollMenuChanged();
    void changeFlashSettings();
    void speakTransportMenuChanged();
    void reduceMotionMenuChanged();
    void oscServerEnabledMenuChanged();
    void allowRemoteOSCMenuChanged();
    void showLogMenuChanged();
    void showCuesMenuChanged();
    void showMetroChanged();
    void updateMetroVisibility();
    void logAutoScrollMenuChanged();
    void changeScopeKindVisibility(QString name);
    void scopeKindVisibilityMenuChanged();
    void toggleLeftScope();
    void toggleRightScope();
    void changeScopeLabels();
    void changeTitleVisibility();
    void titleVisibilityChanged();
    void changeMenuBarInFullscreenVisibility();
    void menuBarInFullscreenVisibilityChanged();
    void scopeVisibilityChanged();
    void logCuesMenuChanged();
    void changeLogCues();
    void logSynthsMenuChanged();
    void changeLogSynths();
    void clearOutputOnRunMenuChanged();
    void changeClearOutputOnRun();
    void autoIndentOnRunMenuChanged();
    void changeAutoIndentOnRun();
    void cycleThemes();
    void updateColourTheme();
    // SONIC_PI_THEME_BENCH=1: measure repeated theme-scheme switches and hue
    // rotations (the two re-theme triggers), print per-apply timings to stdout
    // and exit. Runs without the audio server; see runThemeBench().
    void runThemeBench();
    void colourThemeMenuChanged(int themeID);
    // Follow the OS contrast preference (Windows Contrast Themes, macOS
    // Increase Contrast): auto-switch to the high-contrast theme while the
    // preference is active and restore the user's prior theme when it lifts.
    // An explicit theme pick (menu/prefs/cycle) always wins — see
    // noteExplicitThemeChoice().
    void applyOSContrastPreference();
    void noteExplicitThemeChoice();
    void updatePrefsIcon();
    void togglePrefs();
    void updateDocPane(QListWidgetItem* cur);
    void updateDocPane2(QListWidgetItem* cur, QListWidgetItem* prev);
    void showWindow();
    void splashClose();
    void setMessageBoxStyle();
    void startupError(QString msg);
    void tabNext();
    void tabPrev();
    void tabGoto(int index);
    void helpContext();
    void showHelpForKeyword(QString keyword);
    // Move keyboard focus to `pane` so a screen reader follows it; caller first
    // reveals the pane's host (setFocus is ignored on a hidden widget).
    void focusPane(QWidget* pane);
    // Copy each widget's tooltip into its accessibleDescription when the
    // latter is empty, so screen-reader users get the same explanatory text
    // as sighted hover users. Idempotent; safe to re-run after late UI setup.
    void mirrorToolTipsToAccessibleDescriptions();
    // Speak a short message via the screen reader; a no-op when no screen reader is
    // active or when the user has silenced this announcement's category.
    void announce(const QString& message, bool assertive = false,
                  SonicPi::Announcement category = SonicPi::Announcement::General);
    // Status-bar message that is also spoken — for state changes whose only
    // other feedback is visual.
    void showStatusAndAnnounce(const QString& message, int timeoutMs = 2000);
    void updateRecordingUI();
    void addMenuBarMnemonics();
    // Reveal the Help dock and bring its tab strip to the Docs tab.
    void revealDocsTab();
    void resetErrorPane();
    void helpScrollUp();
    void helpScrollDown();
    void docPrevTab();
    void docNextTab();
    void focusCurrentHelpList();
    void docScrollUp();
    void docScrollDown();
    void updateFullScreenMode();
    void toggleFullScreenMode();
    void fullScreenMenuChanged();
#ifdef Q_OS_MAC
    void syphonPublishMenuChanged();
    void syphonShowCursorMenuChanged();
#endif
#ifdef Q_OS_WIN
    void spoutPublishMenuChanged();
    void spoutShowCursorMenuChanged();
#endif
#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    // Single funnel for Syphon/Spout window publishing from the menubar
    // or the Visuals pill; reflects the actual outcome into both views.
    void setWindowPublishing(bool wantOn);
    void recordShowCursorMenuChanged();
    void recordFlashIconMenuChanged();
    void showRecordingModeMenu(const QPoint& pos);
    void setRecordingMode(int mode);
#endif
    void updateFocusMode();
    void toggleFocusMode();
    void toggleScopePaused();
    void updateLogVisibility();
    void updateCuesVisibility();
    void createDebugAndLogTabs();
    void toggleLogVisibility();
    void toggleCuesVisibility();
    void updateTabsVisibility();
    void toggleTabsVisibility();
    void showTabsMenuChanged();
    void updateButtonVisibility();
    void showButtonsMenuChanged();
    void toggleButtonVisibility();
    void updateEditorToolbarVisibility();
    void showEditorToolbarMenuChanged();

    void requestVersion();
    void heartbeatOSC();
    void zoomCurrentWorkspaceIn();
    void zoomCurrentWorkspaceOut();
    void updateErrorCardZoom();
    void showWelcomeScreen();
    void setupWindowStructure();
    void setupTheme();
    void escapeWorkspaces();
    void toggleMidi(int silent = 0);
    void toggleGamepad(int silent = 0);
    void toggleOSCServer(int silent = 0);
    // Per-device enable/disable, forwarded to the spider which owns the
    // persistent mute list and re-asserts it on device broadcasts.
    void setMidiPortEnabled(QString direction, QString name, bool enabled);
    void setGamepadDeviceEnabled(QString name, bool enabled);
    void honourPrefs();

    // Toggle the bottom Help/Debug dock (double-clicking the divider bar).

    void showBufferCapacityError();
    void checkForStudioMode();

    void focusLogs();
    void focusEditor();
    void focusCues();
    void focusContext();
    void focusPreferences();
    void focusHelpListing();
    void focusHelpDetails();
    void focusErrors();
    // Direct jumps to the remaining south-dock tabs (Docs is covered by the
    // help listing/details pair above) — the screen-reader escape hatch from
    // traversing the whole window.
    void focusHelpCards();
    void focusHelpLogs();
    void focusHelpDebug();
    // F6/Shift+F6: walk keyboard focus through the panes currently on screen
    // (the platform pane-cycling convention on Windows).
    void cycleFocusForward();
    void cycleFocusBack();
    void cycleFocus(int direction);
    void focusBPMScrubber();
    void focusTimeWarpScrubber();
    void shortcutModeMenuChanged(int modeID);

private:
    void resetShortcuts();
    void loadWinShortcuts();
    void loadMacShortcuts();
    void loadEmacsShortcuts();
    void loadUserShortcuts();
    void loadUserShortcut(const QString& id, QSettings& shortcut_settings);

    SonicPiScintilla* getCurrentWorkspace();
    SonicPiEditor* getCurrentEditor();
    // The synth in effect at the cursor (last use_synth/with_synth before it),
    // defaulting to "beep". Drives synth-aware `play` autocompletion.
    QString currentSynthForCompletion();
    // The track in effect at the cursor (last use_track/with_track before
    // it), or empty. Drives the plugin-parameter completion of the track
    // verbs when no track: is written.
    QString currentTrackForCompletion();
    void resizeEvent(QResizeEvent* e) override;
    void movePrefsWidget();
    void slidePrefsWidgetIn();
    void slidePrefsWidgetOut();
    void initPaths();
    QString osDescription();
    QString cpuDescription();

    void blankTitleBars();
    void namedTitleBars();
    // Persistent dock title bar: a #paneTitle label on the left and the given
    // controls packed right. Controls stay visible when titles are hidden (only
    // the label toggles). outLabel receives the label pointer.
    QWidget* makeControlTitleBar(const QString& title, QLabel*& outLabel,
                                 const QVector<QWidget*>& controls);

#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    // A+V session-recorder branch of toggleRecording. Write to a temp
    // file; rename or delete it once the user picks a save location.
    void startSessionRecordingFlow();
    void stopSessionRecordingFlow();
    void recordingFailed(const QString& reason);   // the recorder could not start or died: Record goes back
#endif

    void clearOutputPanels();
    void createToolBar();
    void createExamplesMenu();
    void openExample(const QString& path, const QString& title, int helpRow);
    void showExamplesHelpTab(int row);
    void showQuickstartCards();
    // Path of the card set to load: a set explicitly loaded via the Examples
    // menu (persisted), else the user override, else the shipped default.
    QString cardsFileToLoad();
    void applySouthTabIcons();
    void showHelpListTab(int tabIdx, int row);
    // Render this help-list selection in the docs pane
    bool showInTutorialPane(int tabIdx, int row);
    // Select the help row whose generated page matches `url` (in-doc links)
    void showHelpPageForUrl(const QUrl& url);
    QString prefWrappedCode(QString code);
    void createStatusBar();
    void createInfoPane();
    void createScopePane();
    void readSettings();
    void restoreWindows();
    void restoreScopeState(std::vector<QString> names);
    void writeSettings();
    void loadFile(const QString& fileName, SonicPiScintilla*& text);
    bool saveFile(const QString& fileName, SonicPiScintilla* text);
    void loadWorkspaces();
    void saveWorkspaces();
    void updateShortcuts();
    void updateShortcut(const QString& id, QAction* action, const QString& desc);
    std::string number_name(int);
    std::string workspaceFilename(SonicPiScintilla* text);
    SonicPiScintilla* filenameToWorkspace(std::string filename);

    bool sendOSC(oscpkt::Message m);
    //   void initPrefsWindow();
    void initDocsWindow();
    void refreshDocContent();
    void addHelpPage(QListWidget* nameList, struct help_page* helpPages,
        int len);
    QListWidget* createHelpTab(QString name);
    static QKeySequence metaKey(const QString& key);
    static QKeySequence shiftMetaKey(const QString& key);
    static QKeySequence ctrlMetaKey(const QString& key);
    static QKeySequence ctrlShiftKey(const QString& key);
    static QKeySequence ctrlKey(const QString& key);
    char int2char(int i);
    void updateAction(QAction* action, const QString& desc);
    QString tooltipStrShiftMeta(const QString& key, const QString& str);
    QString tooltipStrMeta(const QString& key, const QString& str);
    QString readFile(QString name);
    QString rootPath();

    void addUniversalCopyShortcuts(QTextEdit* te);
    void updateTranslatedUIText();

    QMenu* shortcutMenu = nullptr;
    QMenu* liveMenu = nullptr;
    QMenu* codeMenu = nullptr;
    QMenu* examplesMenu = nullptr;
    QMenu* audioMenu = nullptr;
    QMenu* displayMenu = nullptr;
    QMenu* viewMenu = nullptr;
    QMenu* focusMenu = nullptr;
    QMenu* tabMenu = nullptr;
    QMenu* ioMenu = nullptr;
    QMenu* ioMidiInMenu = nullptr;
    QMenu* ioMidiOutMenu = nullptr;
    QMenu* ioMidiOutChannelMenu = nullptr;
    QMenu* ioGamepadMenu = nullptr;
    QMenu* localIpAddressesMenu = nullptr;
    QMenu* themeMenu = nullptr;
    QMenu* scopeKindVisibilityMenu = nullptr;
    QMenu* languageMenu = nullptr;
    QMenu* accessibilityMenu = nullptr;
    QMenu* recentSetsMenu = nullptr;
    QAction* examplesPlayOnOpenAct = nullptr;
    QHash<int, QString> m_jobWorkspaces; // live jobs -> source workspace (error routing)
    QStringList tutorialJsonPaths; // sorted generated chapter JSON, row-aligned with the Tutorial help list
    QStringList examplePaths;      // qt-doc glob order, row-aligned with the Examples help list
    QStringList exampleTitles;
    // Generated reference docs (loaded lazily from etc/doc/generated/native)
    QVector<SonicPi::InstrumentPage> synthDocPages;
    QVector<SonicPi::InstrumentPage> fxDocPages;
    QVector<SonicPi::SampleGroup> sampleDocGroups;
    QVector<SonicPi::LangPage> langDocPages;
    bool nativeDocsLoaded = false;
    void loadNativeDocs();
    QHash<int, QStringList> helpTabKeywords; // tab index -> per-row keywords ("" where none)
    // Last tab/row shown in the tutorial pane; itemPressed + currentItemChanged
    // both fire per click, so loads must dedupe
    int lastTutorialDocTab = -1;
    int lastTutorialDocRow = -1;
    QMap<QString, QKeySequence> shortcutMap;

    QSettings* gui_settings = nullptr;
    SonicPiSettings* piSettings = nullptr;
    SonicPii18n* sonicPii18n = nullptr;

    bool fullScreenMode = false;
    bool focusMode;
    struct FocusSnapshot
    {
        bool fullScreen = false;
        bool tabs = true;
        bool buttons = true;
        bool log = true;
        bool cues = true;
        bool metro = true;
        bool scopes = false;
        bool docs = false;
    };
    FocusSnapshot preFocus;
    // Boot restores many prefs through the same paths as user toggles; hold
    // screen-reader status announcements until the boot sequence finishes.
    bool bootAnnouncementsReady = false;
    // Focus mode drives fullscreen as a side effect; suppress the fullscreen
    // message so the focus-mode exit hint isn't clobbered.
    bool quietFullScreenChange = false;
    // Last announced mixer state, so a single pref toggle speaks one message
    // (mixerSettingsChanged always re-applies both axes).
    bool lastMixerInvertStereo = false;
    bool lastMixerForceMono = false;
    bool mixerStateKnown = false;

    QCheckBox* startup_error_reported = nullptr;
    bool is_recording;
    bool show_rec_icon_a;
    QTimer* rec_flash_timer = nullptr;

    SplashWidget* splash = nullptr;
    QTimer* boot_poll_timer = nullptr;
    int boot_poll_tries = 0;
    // Extra poll ticks granted after HasServerErrored() flips, so the daemon's
    // /exited-with-boot-error report (which trails the flag by a few ms) can
    // land and its specific message beat the generic connect error.
    int boot_error_grace_ticks = 0;

    bool i18n;
    static const int workspace_max = 10;
    SonicPiScintilla* workspaces[workspace_max];
    QTabWidget* docsNavTabs = nullptr;
    // The section selector lives outside the splitter's nav column so it can run
    // the full width of the pane, like the Cards deck bar. docsNavTabs keeps the
    // pages and its own tab bar is hidden; these pills drive its current index.
    QWidget* docsPane = nullptr;      // the southTabs page: pill row + docsplit
    QWidget* docsPillRow = nullptr;
    QVector<QPushButton*> docsPills;
    IconTabWidget* southTabs = nullptr;

    SonicPiLog* outputPane = nullptr;
    SonicPiLog* incomingPane = nullptr;
    SonicPiMetro* metroPane = nullptr;
    QTextBrowser* errorPane = nullptr;
    SonicPiErrorCard* errorCard = nullptr;
    void onErrorAnchorClicked(const QUrl& link);
    // "Jump to error" target: the buffer tab, line and column the marker was set on.
    int m_errorJumpTab = -1;
    int m_errorJumpLine = -1;
    int m_errorJumpCol = 0;
    QDockWidget* outputWidget = nullptr;
    QDockWidget* incomingWidget = nullptr;
    QWidget* prefsWidget = nullptr;

    QDockWidget* hudWidget = nullptr;
    QDockWidget* docWidget = nullptr;
    // The help pane's chevrons, top right, as the web's on its divider: up makes
    // the pane full size over the editor's room; down hides it, and from full
    // size first steps back beside the editor.
    ChevronButton* helpFullButton = nullptr;
    ChevronButton* helpHideButton = nullptr;
    QWidget*     helpChevrons = nullptr;      // the two grips, laid over the dock separator (positionHelpChevrons)
    QDockWidget* helpAwayDock = nullptr;
    DividerOverlay* helpAwayDivider = nullptr; // the divider painted over the separator Qt reserves beside it     // a 1px placeholder in the bottom area while the help is away: its separator is the divider, the way back
    // The help panel's one state (model/helppanelmodel.h): the Help icon, the
    // grips, the menu items and focus mode all move it, and applyHelpPanel
    // puts it on screen. Nothing else shows or hides the dock.
    SonicPi::HelpPanelModel m_helpPanel;
    bool m_helpFullApplied = false;           // the room is the panel's right now
    bool m_applyingHelpPanel = false;         // applyHelpPanel re-entered by the dock's own signals
    bool m_restoringLayout = false;           // restoreState() is moving docks: no setting follows them
    QList<QDockWidget*> m_hiddenForHelpFull;  // the panes the full-size help took the room from
    void helpToggle();                        // the Help icon, the down grip, the shortcut, a double-click on the divider
    void helpFull();                          // the up grip
    void helpShow();                          // a pane asked for: beside the code if it was away
    void applyHelpPanel();                    // the dock, the editor's room, the icon, the grips and the bar, from the state
    // The column beside the code (scope, log, cues, metronome): one state
    // (model/sidecolumnmodel.h) moved by the grip on its divider; the panes'
    // own settings stay the View menu's.
    SonicPi::SideColumnModel m_sideColumn;
    ChevronButton* sideGrip = nullptr;        // on the editor | column divider, at its top
    QDockWidget*   sideAwayDock = nullptr;
    DividerOverlay* sideAwayDivider = nullptr;    // a 1px placeholder in the right area while the column is away: its separator is the divider
    void sideToggle();
    void applySideColumn();                   // the panes, the grip and the bar, from the state and the settings
    void positionSideGrip();                  // over the divider at its top, or over the bar

    ZoomBar* logsZoom = nullptr;             // Logs/Debug tab text-size controls in the title row
    ZoomBar* debugZoom = nullptr;
    ZoomBar* tracksZoom = nullptr;
    void updateHelpChevronIcons();            // (re)renders the chevrons for the current theme
    void positionHelpChevrons();              // over the separator above the help pane, at its right
    QList<QAction*> docsFilterSearchActions;  // leading magnifier glyph in each docs filter field
    void updateDocsFilterIcons();             // (re)tints the magnifiers for the current theme
    void applyDocsNavZoom();                  // scales the topic lists/filters to the docs A-/A+ step
    void updateDocsNavMinWidth();             // nav column minimum, independent of the pill row
    void buildDocsPills();                    // one pill per docs section, built once tabs exist
    void syncDocsPills();                     // marks the current pill after an index change
    void ensureDocsSelection();               // current docs tab always has a selected page
    bool infoPanesDirty = true;               // info html needs re-render (styles changed while hidden)
    bool infoPanesLoaded = false;             // info html read+parsed on first open, not at startup
    void loadInfoPaneContent();
    void rerenderInfoPanes();                 // re-render info panes, preserving scroll
    // History page: adds the back-to-top links and records each release
    // anchor's heading text (the jump target). Other pages pass through
    // untouched, leaving releaseTitles empty.
    static QString addReleaseNavigation(const QString& html, QVariantMap& releaseTitles);
    // Brand mark for the About page, generated at the theme accent and given
    // to the pane's document as an image resource. Returns its logical size.
    QSize installInfoLogo(QTextBrowser* pane);
    // Logical width (dx) of that mark on the About page.
    static constexpr int kInfoLogoWidthDx = 230;
    QDockWidget* metroWidget = nullptr;
    LogPanel* debugLogPanel = nullptr;
    MetricsPanel* metricsPanel = nullptr;
    TracksPanel*  tracksPanel = nullptr;
    int m_savedDockH = 0;                    // dock height to restore when re-opening via double-click
    int m_dockHBeforeSteal = -1;             // help-dock height before an error stole from it (-1: nothing stolen)

    QWidget* blankWidgetOutput = nullptr;
    QWidget* blankWidgetIncoming = nullptr;
    QWidget* blankWidgetMetro = nullptr;
    // Custom dock title bars: QLabel#paneTitle (small/muted/left, matching the
    // SuperSonic debug pane). QDockWidget::title's QSS colour isn't honoured for
    // the title text, so we supply our own label widgets.
    QLabel* titleBarOutput = nullptr;
    QLabel* titleBarIncoming = nullptr;
    QLabel* titleBarScope = nullptr;
    QLabel* titleBarDoc = nullptr;
    QLabel* titleBarMetro = nullptr;
    TutorialPane* tutorialPane = nullptr;
    QuickstartPane* quickstartPane = nullptr;

    //  QTextBrowser *hudPane;
    QWidget* mainWidget = nullptr;
    QDockWidget* scopeWidget = nullptr;
    QDockWidget* visualizerWidget = nullptr;
    bool hidingDocPane;
    bool restoreDocPane;

    QTabWidget* editorTabWidget = nullptr;
    QProcess* serverProcess = nullptr;

    SonicPiLexer* lexer = nullptr;
    SonicPiTheme* theme = nullptr;
    // The scheme + icon-set actually applied by updateColourTheme(); lets the
    // prefs themeChanged handler distinguish a real change from a re-click of
    // the active option. themeEverApplied is false until the first application.
    // static_cast<int> of the applied ColourScheme (SonicPiTheme is only
    // forward-declared here, so the enum can't be named directly).
    int appliedColourScheme = -1;
    bool appliedProIcons = false;
    bool themeEverApplied = false;
    // Coalesces rapid theme re-applies (hue dial, monochrome) off the input path.
    QTimer* themeApplyTimer = nullptr;
#if QT_VERSION >= QT_VERSION_CHECK(6, 10, 0)
    // OS contrast-preference watcher (Windows Contrast Themes / macOS
    // Increase Contrast); created lazily on first use by
    // applyOSContrastPreference() or noteExplicitThemeChoice().
    QAccessibilityHints* accessibilityHints = nullptr;
#endif
    SonicPiToolTipManager* toolTipManager = nullptr;

    QToolBar* toolBar = nullptr;
    QAction* textUpcaseWordAct = nullptr;
    QAction* textDowncaseWordAct = nullptr;
    QAction* textDeleteWordRightAct = nullptr;
    QAction* textDeleteWordLeftAct = nullptr;
    QAction* textSelectAllAct = nullptr;
    QAction* textRedoAct = nullptr;
    QAction* textUndoAct = nullptr;
    QAction* textCenterCaretAct = nullptr;
    QAction* textWordLeftAct = nullptr;
    QAction* textWordRightAct = nullptr;
    QAction* textSelectLineStartAct = nullptr;
    QAction* textSelectLineEndAct = nullptr;
    QAction* textSelectWordLeftAct = nullptr;
    QAction* textSelectWordRightAct = nullptr;
    QAction* textSelectDocStartAct = nullptr;
    QAction* textSelectDocEndAct = nullptr;
    QAction* textDocEndAct = nullptr;
    QAction* textDocStartAct = nullptr;
    QAction* textLineEndAct = nullptr;
    QAction* textLineStartAct = nullptr;
    QAction* textDeleteBackAct = nullptr;
    QAction* textDeleteForwardAct = nullptr;
    QAction* textRightAct = nullptr;
    QAction* textLeftAct = nullptr;
    QAction* textCopyAct = nullptr;
    QAction* textCutAct = nullptr;
    QAction* textPasteAct = nullptr;
    QAction* textCutToEndOfLineAct = nullptr;
    QAction* textDownAct = nullptr;
    QAction* textUpAct = nullptr;
    QAction* textDownTenAct = nullptr;
    QAction* textUpTenAct = nullptr;
    QAction* logZoomInAct = nullptr;
    QAction* logZoomOutAct = nullptr;
    QAction* textSetMarkAct = nullptr;
    QAction* triggerAutocompleteAct = nullptr;
    QAction* readCompletionDetailsAct = nullptr;
    QAction* winShortcutModeAct = nullptr;
    QAction* emacsShortcutModeAct = nullptr;
    QAction* macShortcutModeAct = nullptr;
    QAction* userShortcutModeAct = nullptr;
    QAction* tabPrevAct = nullptr;
    QAction* tabNextAct = nullptr;
    QAction* tab1Act = nullptr;
    QAction* tab2Act = nullptr;
    QAction* tab3Act = nullptr;
    QAction* tab4Act = nullptr;
    QAction* tab5Act = nullptr;
    QAction* tab6Act = nullptr;
    QAction* tab7Act = nullptr;
    QAction* tab8Act = nullptr;
    QAction* tab9Act = nullptr;
    QAction* tab0Act = nullptr;
    QAction* cycleThemesAct = nullptr;
    QAction* exitAct = nullptr;
    QAction* runAct = nullptr;
    QAction* stopAct = nullptr;
    QAction* saveAsAct = nullptr;
    QAction* loadFileAct = nullptr;
    QAction* loadSetAct = nullptr;
    QAction* saveSetAct = nullptr;
    QAction* saveSetAsAct = nullptr;
    QAction* clearAllBuffersAct = nullptr;
    QAction* recAct = nullptr;
    QAction* textAlignAct = nullptr;
    QAction* textCommentAct = nullptr;
    QAction* textTransposeAct = nullptr;
    QAction* textShiftLineUpAct = nullptr;
    QAction* textShiftLineDownAct = nullptr;
    QAction* contextHelpAct = nullptr;
    QAction* textIncAct = nullptr;
    QAction* textDecAct = nullptr;
    QAction* scopeAct = nullptr;
    QAction* infoAct = nullptr;
    QAction* helpAct = nullptr;
    QAction* prefsAct = nullptr;
    QAction* focusEditorAct = nullptr;
    QAction* focusLogsAct = nullptr;
    QAction* focusContextAct = nullptr;
    QAction* focusCuesAct = nullptr;
    QAction* focusPreferencesAct = nullptr;
    QAction* focusHelpListingAct = nullptr;
    QAction* focusHelpDetailsAct = nullptr;
    QAction* focusErrorsAct = nullptr;
    QAction* focusHelpCardsAct = nullptr;
    QAction* focusHelpLogsAct = nullptr;
    QAction* focusHelpDebugAct = nullptr;
    QAction* focusBPMScrubberAct = nullptr;
    QAction* focusTimeWarpScrubberAct = nullptr;
    QAction* cycleFocusForwardAct = nullptr;
    QAction* cycleFocusBackAct = nullptr;
    QAction* showLineNumbersAct = nullptr;
    QAction* showAutoCompletionAct = nullptr;
    QAction* showCompletionHelpAct = nullptr;
    QAction* showContextAct = nullptr;
    QAction* flashCodeAct = nullptr;
    QAction* flashGutterAct = nullptr;
    QAction* showLoopScopesAct = nullptr;
    QAction* loopScopeScrollAct = nullptr;
    QAction* speakTransportAct = nullptr;
    QAction* reduceMotionAct = nullptr;
    QAction* audioSafeAct = nullptr;
    QAction* audioTimingGuaranteesAct = nullptr;
    QAction* enableExternalSynthsAct = nullptr;
    QAction* mixerInvertStereoAct = nullptr;
    QAction* mixerForceMonoAct = nullptr;
    QAction* enableScsynthInputsAct = nullptr;
    QAction* midiEnabledAct = nullptr;
    QAction* gamepadEnabledAct = nullptr;
    QAction* enableOSCServerAct = nullptr;
    QAction* allowRemoteOSCAct = nullptr;
    QAction* showLogAct = nullptr;
    QAction* showCuesAct = nullptr;
    QAction* logAutoScrollAct = nullptr;
    QAction* logCuesAct = nullptr;
    QAction* logSynthsAct = nullptr;
    QAction* clearOutputOnRunAct = nullptr;
    QAction* autoIndentOnRunAct = nullptr;
    QAction* showButtonsAct = nullptr;
    QAction* showTabsAct = nullptr;
    QAction* fullScreenAct = nullptr;
    QAction* lightThemeAct = nullptr;
    QAction* darkThemeAct = nullptr;
    QAction* highContrastThemeAct = nullptr;
    QAction* mildThemeAct = nullptr;
    QAction* phosphorThemeAct = nullptr;
    QAction* signalThemeAct = nullptr;
    QAction* proIconsAct = nullptr;
    QAction* showScopeLabelsAct = nullptr;
    QAction* showTitlesAct = nullptr;
    QAction* hideMenuBarInFullscreenAct = nullptr;
    QAction* showMetroAct = nullptr;
    QAction* enableLinkAct = nullptr;
    QAction* linkTapTempoAct = nullptr;
    QAction* scopePausedAct = nullptr;
    QAction* focusModeAct = nullptr;
    QAction* checkUpdatesAct = nullptr;
    QAction* checkUpdatesNowAct = nullptr;
    QAction* findAct = nullptr;
    QAction* findNextAct = nullptr;
    QAction* findPrevAct = nullptr;
    QAction* showEditorToolbarAct = nullptr;
#ifdef Q_OS_MAC
    QAction *syphonPublishAct = nullptr;
    QAction *syphonShowCursorAct = nullptr;
#endif
#ifdef Q_OS_WIN
    QAction *spoutPublishAct = nullptr;
    QAction *spoutShowCursorAct = nullptr;
#endif
#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    QAction *recordShowCursorAct = nullptr;
    QAction *recordFlashIconAct = nullptr;
    // Shared by the IO menubar submenu, the rec-button right-click
    // menu, and (via setRecordingMode) the Preferences radios.
    QAction *recAudioModeAct = nullptr;
    QAction *recAudioVideoModeAct = nullptr;
    // Per-recording temp file; renamed or removed on stop.
    QString m_videoTempPath;
#endif
    QShortcut* textLeftSc = nullptr;
    QShortcut* escapeSc = nullptr;
    QShortcut* escape2Sc = nullptr;
    QActionGroup* langActionGroup = nullptr;

    SettingsWidget* settingsWidget = nullptr;

    QCheckBox* studio_mode = nullptr;
    QLineEdit* user_token = nullptr;

    InfoWidget* infoWidg = nullptr;
    QList<QTextBrowser*> infoPanes;
    QVBoxLayout* mainWidgetLayout = nullptr;

    QList<QListWidget*> helpLists;
    QHash<QString, help_entry> helpKeywords;
    std::streambuf* coutbuf = nullptr;
    std::ofstream stdlog;

    ScintillaAPI* autocomplete = nullptr;
#ifdef QT_OLD_API
    QString fetch_url_path, sample_path, log_path, sp_user_path, sp_user_tmp_path, ruby_server_path, ruby_path, server_error_log_path, server_output_log_path, gui_log_path, init_script_path, exit_script_path, tmp_file_store, process_log_path, port_discovery_path;
#endif
    QString qt_browser_dark_css, qt_browser_light_css, qt_browser_hc_css, qt_app_theme_path;

    QString defaultTextBrowserStyle;

    QString version;
    int version_num;
    QString latest_version;
    int latest_version_num;

    ThinSplitter* docsplit = nullptr;

    QLabel* versionLabel = nullptr;
    bool tmpFileStoreAvailable;
    bool updated_dark_mode_for_help, updated_dark_mode_for_prefs;
    int guiID;

    SonicPi::ScopeWindow* scopeWindow = nullptr;
    // Levels-only duplicate embedded in the audio preferences Level box.
    SonicPi::ScopeWindow* levelPrefsScope = nullptr;
    std::shared_ptr<SonicPi::QtAPIClient> m_spClient;
    std::shared_ptr<SonicPi::SonicPiAPI> m_spAPI;
    std::shared_ptr<QRect> m_appWindowSizeRect;

    QSet<QString> cuePaths;
};
