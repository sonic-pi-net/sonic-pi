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
#include <QMainWindow>
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
class QStackedWidget;
class ThinSplitter;
class TutorialPane;

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
    const char* id;
    const char* desc;
    const char* mac;
    const char* win;
    const char* emacs;
    const char* group;
    QAction* MainWindow::* act;
    const char* secondary = nullptr;  // optional extra shortcut (undocumented fallback), all keymaps
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
    void updateScsynthInfo(QString description);
    void updateAudioDevices(const SonicPi::AudioDevicesInfo& devicesInfo);
    void updateAudioInputDevices(const SonicPi::AudioInputDevicesInfo& devicesInfo);
    void updateAudioDeviceConfig(const SonicPi::AudioDeviceConfigInfo& configInfo);
    void homeDirWriteError();
    void replaceLines(QString id, QString content, int first_line, int finish_line, int point_line, int point_index);
    void runBufferIdx(int idx);

    bool loaded_workspaces;
    QSet<QString> initialWorkspaceLoads;
    QString pendingSetPath;
    QString currentSetPath;
    QString hash_salt;
    QString ui_language;

public slots:
    void switchAudioDriver(QString driver);
    void switchAudioDevice(QString device);
    void switchAudioInputDevice(QString device);
    void changeSampleRate(int rate);
    void onSupersonicSetup(int sampleRate, int bufferSize);
    void onSpiderReady();
    void onAudioSwitchDone(const SonicPi::AudioSwitchOutcome& outcome);
    void changeBufferSize(int size);

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
    bool m_audioDevicesSeen = false;
    bool m_audioInputDevicesSeen = false;
    bool m_audioDeviceConfigSeen = false;
    SonicPi::AudioDevicesInfo      m_lastAudioDevices;
    SonicPi::AudioInputDevicesInfo m_lastAudioInputDevices;
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
    void changeSystemPreAmp(int val, int silent = 0);
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
    void toggleDocPane();

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
    // Spawn / free the supersonic-audio-out synth that feeds the
    // session recorder's audio track.
    void spawnRecordAudioOutSynth();
    void freeRecordAudioOutSynth();
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

    QMenu *shortcutMenu, *liveMenu, *codeMenu, *examplesMenu, *audioMenu, *displayMenu, *viewMenu, *focusMenu, *tabMenu, *ioMenu, *ioMidiInMenu, *ioMidiOutMenu, *ioMidiOutChannelMenu, *ioGamepadMenu, *localIpAddressesMenu, *themeMenu, *scopeKindVisibilityMenu, *languageMenu, *accessibilityMenu, *recentSetsMenu;
    QAction* examplesPlayOnOpenAct;
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

    QSettings* gui_settings;
    SonicPiSettings* piSettings;
    SonicPii18n* sonicPii18n;

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

    QCheckBox* startup_error_reported;
    bool is_recording;
    bool show_rec_icon_a;
    QTimer* rec_flash_timer;

    SplashWidget* splash;
    QTimer* boot_poll_timer = nullptr;
    int boot_poll_tries = 0;

    bool i18n;
    static const int workspace_max = 10;
    SonicPiScintilla* workspaces[workspace_max];
    QTabWidget* docsNavTabs;
    QTabWidget* southTabs;

    SonicPiLog* outputPane;
    SonicPiLog* incomingPane;
    SonicPiMetro* metroPane;
    QTextBrowser* errorPane;
    SonicPiErrorCard* errorCard;
    void onErrorAnchorClicked(const QUrl& link);
    // "Jump to error" target: the buffer tab, line and column the marker was set on.
    int m_errorJumpTab = -1;
    int m_errorJumpLine = -1;
    int m_errorJumpCol = 0;
    QDockWidget* outputWidget;
    QDockWidget* incomingWidget;
    QWidget* prefsWidget;

    QDockWidget* hudWidget;
    QDockWidget* docWidget;
    QPushButton* helpCloseButton = nullptr;  // ✕ = persistent close, top-right of the help pane
    ZoomBar* logsZoom = nullptr;             // Logs/Debug tab text-size controls in the title row
    ZoomBar* debugZoom = nullptr;
    QIcon m_helpCloseIcon;                    // tabler-x, theme-tinted (rest / hover)
    QIcon m_helpCloseIconHover;
    void updateHelpCloseIcon();               // (re)renders the ✕ for the current theme
    QList<QAction*> docsFilterSearchActions;  // leading magnifier glyph in each docs filter field
    void updateDocsFilterIcons();             // (re)tints the magnifiers for the current theme
    void applyDocsNavZoom();                  // scales the topic lists/filters to the docs A-/A+ step
    void updateDocsNavMinWidth();             // keeps the Tutorial/Examples/… chips un-squashed
    void ensureDocsSelection();               // current docs tab always has a selected page
    bool infoPanesDirty = true;               // info html needs re-render (styles changed while hidden)
    bool infoPanesLoaded = false;             // info html read+parsed on first open, not at startup
    void loadInfoPaneContent();
    void rerenderInfoPanes();                 // re-render info panes, preserving scroll
    QDockWidget* metroWidget;
    LogPanel* debugLogPanel = nullptr;
    MetricsPanel* metricsPanel = nullptr;
    int m_savedDockH = 0;                    // dock height to restore when re-opening via double-click
    int m_dockHBeforeSteal = -1;             // help-dock height before an error stole from it (-1: nothing stolen)

    QWidget* blankWidgetOutput;
    QWidget* blankWidgetIncoming;
    QWidget* blankWidgetMetro;
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
    QWidget* mainWidget;
    QDockWidget* scopeWidget;
    QDockWidget* visualizerWidget;
    bool hidingDocPane;
    bool restoreDocPane;

    QTabWidget* editorTabWidget;
    QProcess* serverProcess;

    SonicPiLexer* lexer;
    SonicPiTheme* theme;
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
    SonicPiToolTipManager* toolTipManager;

    QToolBar* toolBar;
    QAction *textUpcaseWordAct, *textDowncaseWordAct, *textDeleteWordRightAct, *textDeleteWordLeftAct, *textSelectAllAct, *textRedoAct, *textUndoAct, *textCenterCaretAct, *textWordLeftAct, *textWordRightAct, *textSelectLineStartAct, *textSelectLineEndAct, *textSelectWordLeftAct, *textSelectWordRightAct, *textSelectDocStartAct, *textSelectDocEndAct, *textDocEndAct, *textDocStartAct, *textLineEndAct, *textLineStartAct, *textDeleteBackAct, *textDeleteForwardAct, *textRightAct, *textLeftAct, *textCopyAct, *textCutAct, *textPasteAct, *textCutToEndOfLineAct, *textDownAct, *textUpAct, *textDownTenAct, *textUpTenAct, *logZoomInAct, *logZoomOutAct, *textSetMarkAct, *triggerAutocompleteAct, *readCompletionDetailsAct, *winShortcutModeAct, *emacsShortcutModeAct, *macShortcutModeAct, *userShortcutModeAct, *tabPrevAct, *tabNextAct, *tab1Act, *tab2Act, *tab3Act, *tab4Act, *tab5Act, *tab6Act, *tab7Act, *tab8Act, *tab9Act, *tab0Act, *cycleThemesAct, *exitAct, *runAct, *stopAct, *saveAsAct, *loadFileAct, *loadSetAct, *saveSetAct, *saveSetAsAct, *clearAllBuffersAct, *recAct, *textAlignAct, *textCommentAct, *textTransposeAct, *textShiftLineUpAct, *textShiftLineDownAct, *contextHelpAct, *textIncAct, *textDecAct, *scopeAct, *infoAct, *helpAct, *prefsAct, *focusEditorAct, *focusLogsAct, *focusContextAct, *focusCuesAct, *focusPreferencesAct, *focusHelpListingAct, *focusHelpDetailsAct, *focusErrorsAct, *focusHelpCardsAct, *focusHelpLogsAct, *focusHelpDebugAct, *focusBPMScrubberAct, *focusTimeWarpScrubberAct, *cycleFocusForwardAct, *cycleFocusBackAct, *showLineNumbersAct, *showAutoCompletionAct, *showCompletionHelpAct, *showContextAct, *flashCodeAct, *flashGutterAct, *showLoopScopesAct, *loopScopeScrollAct, *speakTransportAct, *reduceMotionAct, *audioSafeAct, *audioTimingGuaranteesAct, *enableExternalSynthsAct, *mixerInvertStereoAct, *mixerForceMonoAct, *enableScsynthInputsAct, *midiEnabledAct, *gamepadEnabledAct, *enableOSCServerAct, *allowRemoteOSCAct, *showLogAct, *showCuesAct, *logAutoScrollAct, *logCuesAct, *logSynthsAct, *clearOutputOnRunAct, *autoIndentOnRunAct, *showButtonsAct, *showTabsAct, *fullScreenAct, *lightThemeAct, *darkThemeAct, *highContrastThemeAct, *mildThemeAct, *phosphorThemeAct, *signalThemeAct, *proIconsAct, *showScopeLabelsAct, *showTitlesAct, *hideMenuBarInFullscreenAct, *showMetroAct, *enableLinkAct, *linkTapTempoAct, *scopePausedAct, *focusModeAct, *checkUpdatesAct, *checkUpdatesNowAct, *findAct, *findNextAct, *findPrevAct, *showEditorToolbarAct;
#ifdef Q_OS_MAC
    QAction *syphonPublishAct;
    QAction *syphonShowCursorAct;
#endif
#ifdef Q_OS_WIN
    QAction *spoutPublishAct;
    QAction *spoutShowCursorAct;
#endif
#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    QAction *recordShowCursorAct;
    QAction *recordFlashIconAct;
    // Shared by the IO menubar submenu, the rec-button right-click
    // menu, and (via setRecordingMode) the Preferences radios.
    QAction *recAudioModeAct;
    QAction *recAudioVideoModeAct;
    // Per-recording temp file; renamed or removed on stop.
    QString m_videoTempPath;
#endif
    QShortcut *textLeftSc, *escapeSc, *escape2Sc;
    QActionGroup* langActionGroup;

    SettingsWidget* settingsWidget;

    QCheckBox* studio_mode;
    QLineEdit* user_token;

    InfoWidget* infoWidg;
    QList<QTextBrowser*> infoPanes;
    QVBoxLayout* mainWidgetLayout;

    QList<QListWidget*> helpLists;
    QHash<QString, help_entry> helpKeywords;
    std::streambuf* coutbuf;
    std::ofstream stdlog;

    ScintillaAPI* autocomplete;
#ifdef QT_OLD_API
    QString fetch_url_path, sample_path, log_path, sp_user_path, sp_user_tmp_path, ruby_server_path, ruby_path, server_error_log_path, server_output_log_path, gui_log_path, init_script_path, exit_script_path, tmp_file_store, process_log_path, port_discovery_path;
#endif
    QString qt_browser_dark_css, qt_browser_light_css, qt_browser_hc_css, qt_app_theme_path;

    QString defaultTextBrowserStyle;

    QString version;
    int version_num;
    QString latest_version;
    int latest_version_num;

    ThinSplitter* docsplit;

    QLabel* versionLabel;
    bool tmpFileStoreAvailable;
    bool updated_dark_mode_for_help, updated_dark_mode_for_prefs;
    int guiID;

    SonicPi::ScopeWindow* scopeWindow;
    std::shared_ptr<SonicPi::QtAPIClient> m_spClient;
    std::shared_ptr<SonicPi::SonicPiAPI> m_spAPI;
    std::shared_ptr<QRect> m_appWindowSizeRect;

    QSet<QString> cuePaths;
};
