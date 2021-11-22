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
#include <vector>
#include <memory>


#include <QDate>
#include <QMainWindow>
#include <QFuture>
#include <QSet>
#include <QIcon>
#include <QSettings>

// On windows, we need to include winsock2 before other instances of winsock
#ifdef WIN32
#include <winsock2.h>
#endif

#include "osc/oscpkt.hh"

#include "config.h"

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

namespace SonicPi
{
class QtAPIClient;
class SonicPiAPI;
class ScopeWindow;
}

class QShortcut;
class QDockWidget;
class QListWidget;
class QListWidgetItem;
class QSignalMapper;
class QTabWidget;
class QCheckBox;
class QVBoxLayout;
class QSplashScreen;
class QLabel;
class QWebEngineView;
class QWebEngineProfile;
class QWebEnginePage;

class InfoWidget;
class SettingsWidget;
class Scope;
class ScintillaAPI;
class SonicPii18n;
class SonicPiLog;
class SonicPiScintilla;
class SonicPiTheme;
class SonicPiLexer;
class SonicPiSettings;
class SonicPiContext;

struct help_page {
    QString title;
    QString keyword;
    QString url;
};

struct help_entry {
    int pageIndex;
    int entryIndex;
};

class MainWindow : public QMainWindow
{
    Q_OBJECT

    public:
        MainWindow(QApplication &ref, QSplashScreen* splash);

        SonicPiLog* GetOutputPane() const;
        SonicPiLog* GetIncomingPane() const;
        SonicPiTheme* GetTheme() const;



        void addCuePath(QString path, QString val);
        void setLineMarkerinCurrentWorkspace(int num);
        void showError(QString msg);
        void replaceBuffer(QString id, QString content, int line, int index, int first_line);
        void replaceBufferIdx(int buf_idx, QString content, int line, int index, int first_line);
        void setUpdateInfoText(QString t);
        void allJobsCompleted();
        void updateVersionNumber(QString version, int version_num, QString latest_version, int latest_version_num, QDate last_checked_date, QString platform);
        void updateMIDIInPorts(QString port_info);
        void updateMIDIOutPorts(QString port_info);
        void replaceLines(QString id, QString content, int first_line, int finish_line, int point_line, int point_index);
        void runBufferIdx(int idx);


        bool loaded_workspaces;
        QString hash_salt;
        QString ui_language;


    protected:
        void closeEvent(QCloseEvent *event);
        void wheelEvent(QWheelEvent *event);

signals:
        void settingsChanged();

       private slots:

        void updateSelectedUILanguageAction(QString lang);
        void updateContext(int line, int index);
        void updateContextWithCurrentWs();
        void docLinkClicked(const QUrl &url);
        void handleCustomUrl(const QUrl &url);
        void zoomInLogs();
        void zoomOutLogs();
        QString sonicPiHomePath();
        QString sonicPiConfigPath();
        void updateLogAutoScroll();
        bool eventFilter(QObject *obj, QEvent *evt);
        void changeTab(int id);
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
        void completeSnippetListOrIndentLine(QObject *ws);
        void completeSnippetOrIndentCurrentLineOrSelection(SonicPiScintilla *ws);
        void toggleCommentInCurrentWorkspace();
        void toggleComment(SonicPiScintilla *ws);
        void reloadServerCode();
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
        void changeSystemPreAmp(int val, int silent=0);
        void changeGUITransparency(int val);
        void changeShowLineNumbers();
        void showLineNumbersMenuChanged();
        void showAutoCompletionMenuChanged();
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
        void midiEnabledMenuChanged();
        void changeShowAutoCompletion();
        void changeShowContext();
        void showContextMenuChanged();
        void oscServerEnabledMenuChanged();
        void allowRemoteOSCMenuChanged();
        void showLogMenuChanged();
        void showCuesMenuChanged();
        void logAutoScrollMenuChanged();
        void changeScopeKindVisibility(QString name);
        void scopeKindVisibilityMenuChanged();
        void toggleLeftScope();
        void toggleRightScope();
        void changeScopeLabels();
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
        void updatePrefsIcon();
        void togglePrefs();
        void updateDocPane(QListWidgetItem *cur);
        void updateDocPane2(QListWidgetItem *cur, QListWidgetItem *prev);
        void showWindow();
        void splashClose();
        void setMessageBoxStyle();
        void startupError(QString msg);
        void showLanguageLoadingError();
        void tabNext();
        void tabPrev();
        void tabGoto(int index);
        void helpContext();
        void resetErrorPane();
        void helpScrollUp();
        void helpScrollDown();
        void docPrevTab();
        void docNextTab();
        void docScrollUp();
        void docScrollDown();
        void updateFullScreenMode();
        void toggleFullScreenMode();
        void fullScreenMenuChanged();
        void updateFocusMode();
        void toggleFocusMode();
        void toggleScopePaused();
        void updateLogVisibility();
        void updateCuesVisibility();
        void toggleLogVisibility();
        void toggleCuesVisibility();
        void updateTabsVisibility();
        void toggleTabsVisibility();
        void showTabsMenuChanged();
        void updateButtonVisibility();
        void showButtonsMenuChanged();
        void toggleButtonVisibility();

        void requestVersion();
        void heartbeatOSC();
        void zoomCurrentWorkspaceIn();
        void zoomCurrentWorkspaceOut();
        void showWelcomeScreen();
        void setupWindowStructure();
        void setupTheme();
        void escapeWorkspaces();
        void toggleMidi(int silent=0);
        void toggleOSCServer(int silent=0);
        void resetMidi();
        void honourPrefs();

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

    private:
        void initPaths();
        QString osDescription();
        QSignalMapper *signalMapper;

        void clearOutputPanels();
        void createShortcuts();
        void createToolBar();
        void createStatusBar();
        void createInfoPane();
        void createScopePane();
        void readSettings();
        void restoreWindows();
        void restoreScopeState(std::vector<QString> names);
        void writeSettings();
        void loadFile(const QString &fileName, SonicPiScintilla* &text);
        bool saveFile(const QString &fileName, SonicPiScintilla* text);
        void loadWorkspaces();
        void saveWorkspaces();
        std::string number_name(int);
        std::string workspaceFilename(SonicPiScintilla* text);
        SonicPiScintilla* filenameToWorkspace(std::string filename);

        bool sendOSC(oscpkt::Message m);
        //   void initPrefsWindow();
        void initDocsWindow();
        void refreshDocContent();
        void addHelpPage(QListWidget *nameList, struct help_page *helpPages,
                int len);
        QListWidget *createHelpTab(QString name);
        QKeySequence metaKey(char key);
        Qt::Modifier metaKeyModifier();
        QKeySequence shiftMetaKey(char key);
        QKeySequence ctrlMetaKey(char key);
        QKeySequence ctrlShiftMetaKey(char key);
        QKeySequence ctrlShiftKey(char key);
        QKeySequence ctrlKey(char key);
        char int2char(int i);
        void updateAction(QAction *action, QShortcut *sc, QString tooltip, QString desc);
        QString tooltipStrShiftMeta(char key, QString str);
        QString tooltipStrMeta(char key, QString str);
        QString readFile(QString name);
        QString rootPath();

        void addUniversalCopyShortcuts(QTextEdit *te);
        void updateTranslatedUIText();

  QMenu *liveMenu, *codeMenu, *audioMenu, *displayMenu, *viewMenu, *ioMenu, *ioMidiInMenu, *ioMidiOutMenu, *ioMidiOutChannelMenu, *localIpAddressesMenu, *themeMenu, *scopeKindVisibilityMenu, *languageMenu;

        QSettings *gui_settings;
        SonicPiSettings *piSettings;
        SonicPii18n *sonicPii18n;

        bool focusMode;
        QCheckBox *startup_error_reported;
        bool is_recording;
        bool show_rec_icon_a;
        QTimer *rec_flash_timer;

        QSplashScreen* splash;

        bool i18n;
        static const int workspace_max = 10;
        SonicPiScintilla *workspaces[workspace_max];
        QWidget *prefsCentral;
        QTabWidget *docsNavTabs;
        QTabWidget *southTabs;

        SonicPiLog *outputPane;
        SonicPiLog *incomingPane;
        SonicPiContext *contextPane;
        QTextBrowser *errorPane;
        QDockWidget *outputWidget;
        QDockWidget *incomingWidget;
        QDockWidget *prefsWidget;
        QDockWidget *hudWidget;
        QDockWidget *docWidget;
        QDockWidget *contextWidget;
        QWidget *blankWidget;
        QWidget *outputWidgetTitle;
        QTextBrowser *docPane;
        QWebEngineView *phxView;
        QWebEnginePage *phxPage;
        QWebEngineProfile *phxProfile;
        //  QTextBrowser *hudPane;
        QWidget *mainWidget;
        QDockWidget *scopeWidget;
        QDockWidget *visualizerWidget;
        bool hidingDocPane;
        bool restoreDocPane;

        QTabWidget *tabs;
        QProcess *serverProcess;

        SonicPiLexer *lexer;
        SonicPiTheme *theme;

        QToolBar *toolBar;
  QAction *exitAct, *runAct, *stopAct, *saveAsAct, *loadFileAct, *recAct, *textAlignAct, *textIncAct, *textDecAct, *scopeAct, *infoAct, *helpAct, *prefsAct, *focusEditorAct, *focusLogsAct, *focusContextAct, *focusCuesAct, *focusPreferencesAct, *focusHelpListingAct, *focusHelpDetailsAct, *focusErrorsAct, *showLineNumbersAct, *showAutoCompletionAct, *showContextAct, *audioSafeAct, *audioTimingGuaranteesAct, *enableExternalSynthsAct, *mixerInvertStereoAct, *mixerForceMonoAct, *midiEnabledAct, *enableOSCServerAct, *allowRemoteOSCAct, *showLogAct, *showCuesAct, *logAutoScrollAct, *logCuesAct, *logSynthsAct, *clearOutputOnRunAct, *autoIndentOnRunAct, *showButtonsAct, *showTabsAct, *fullScreenAct, *lightThemeAct, *darkThemeAct, *proLightThemeAct, *proDarkThemeAct, *highContrastThemeAct, *showScopeLabelsAct;
  QShortcut *runSc, *stopSc, *saveAsSc, *loadFileSc, *recSc, *textAlignSc, *textIncSc, *textDecSc, *scopeSc, *infoSc, *helpSc, *prefsSc, *focusEditorSc, *focusLogsSc, *focusContextSc, *focusCuesSc, *focusPreferencesSc, *focusHelpListingSc, *focusHelpDetailsSc, *focusErrorsSc;
        QActionGroup *langActionGroup;

        SettingsWidget *settingsWidget;

        QCheckBox *studio_mode;
        QLineEdit   *user_token;

        InfoWidget *infoWidg;
        QList<QTextBrowser *> infoPanes;
        QTextEdit *startupPane;
        QVBoxLayout *mainWidgetLayout;

        QList<QListWidget *> helpLists;
        QHash<QString, help_entry> helpKeywords;
        std::streambuf *coutbuf;
        std::ofstream stdlog;

        ScintillaAPI *autocomplete;
#ifdef QT_OLD_API
        QString fetch_url_path, sample_path, log_path, sp_user_path, sp_user_tmp_path, ruby_server_path, ruby_path, server_error_log_path, server_output_log_path, gui_log_path, scsynth_log_path, init_script_path, exit_script_path, tmp_file_store, process_log_path, port_discovery_path;
#endif
        QString qt_browser_dark_css, qt_browser_light_css, qt_browser_hc_css, qt_app_theme_path;

        QString defaultTextBrowserStyle;

        QString version;
        int version_num;
        QString latest_version;
        int latest_version_num;

        QSplitter *docsplit;

        QLabel *versionLabel;
        bool tmpFileStoreAvailable;
        bool updated_dark_mode_for_help, updated_dark_mode_for_prefs;
        QString guiID;

        SonicPi::ScopeWindow* scopeWindow;
        std::shared_ptr<SonicPi::QtAPIClient> m_spClient;
        std::shared_ptr<SonicPi::SonicPiAPI> m_spAPI;

        QSet<QString> cuePaths;

};
