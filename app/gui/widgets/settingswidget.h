#ifndef SETTINGSWIDGET_H
#define SETTINGSWIDGET_H

#include "model/settings.h"
#include "utils/sonicpi_i18n.h"
#include <api/sonicpi_api.h>

#include <QWidget>
#include <QMap>

class QSlider;
class QDial;
class QTabWidget;
class QTreeWidget;
class QTreeWidgetItem;
class QDialog;
class QBoxLayout;
class QGroupBox;
class QComboBox;
class QCheckBox;
class QPushButton;
class QRadioButton;
class QLabel;
class QLineEdit;
class QButtonGroup;
class QSignalMapper;
class QVBoxLayout;
class QSizePolicy;
class QTimer;

class SettingsWidget : public QWidget
{
    Q_OBJECT

public:
    SettingsWidget(int tau_osc_cues_port, bool i18n, SonicPiSettings *piSettings, SonicPii18n *sonicPii18n, const QString& shortcutConfigPath, QWidget *parent = nullptr);
    ~SettingsWidget();

    void updateVersionInfo( QString info_string, QString visit, bool sonic_pi_net_visible, bool check_now_visible);
    void updateMidiInPorts( QString in );
    void updateMidiOutPorts( QString out );
    void updateScsynthInfo(QString scsynthInfo);
    void updateAudioDevices(const SonicPi::AudioDevicesInfo& devicesInfo);
    void updateAudioInputDevices(const SonicPi::AudioInputDevicesInfo& devicesInfo);
    void updateAudioDeviceConfig(const SonicPi::AudioDeviceConfigInfo& configInfo);
    // Apply (or remove) ASIO-specific input constraints based on the
    // currently-selected driver. Called whenever the driver dropdown
    // changes or the device config refreshes. ASIO drivers are full-
    // duplex single-device by spec — input cannot be disabled or named
    // separately from output. So when on ASIO we force-tick + grey out
    // the "Enable Audio Inputs" checkbox, mirror the input dropdown to
    // the output selection, and show an explanatory side note.
    void applyAsioInputConstraints();
    void updateScopeNames(std::vector<QString>);
    void updateSelectedUILanguage(QString lang);

public slots:
    void updateUILanguage(int index);

private slots:
    void audioDriverChanged(int index);
    void audioDeviceChanged(int index);
    void audioInputDeviceChanged(int index);
    void audioSampleRateChanged(int index);
    void audioBufferSizeChanged(int index);
    void updateMicPermissionStatus();
    void update_mixer_invert_stereo();
    void update_mixer_force_mono();
    void updateEnableScsynthInputs();
    void toggleOscServer();
    void toggleMidi();
    void forceMidiReset();
    void changeMainVolume(int);
    void toggleLineNumbers();
    void showAutoCompletion();
    void showCompletionHelp();
    void toggleLog();
    void toggleCuesLog();
    void toggleMetro();
    void toggleButtons();
    void toggleFullScreen();
    void toggleTabs();
    void toggleLogAutoScroll();
    void updateColourTheme();
    void toggleScope();
    void toggleScopeLabels();
    void toggleScope( QObject* qo );
    void toggleTitles();
    void openSonicPiNet();
    void toggleCheckUpdates();
    void toggleHideMenuBarInFullscreen();
    void checkForUpdatesNow();
    void updateSettings();
    void updateTransparency(int t);
    void settingsChanged();
    void showContext();
    void checkArgs();
    void synthTriggerTimingGuarantees();
    void enableExternalSynths();
    void midiDefaultChannel();
    void logCues();
    void logSynths();
    void clearOutputOnRun();
    void autoIndentOnRun();
    void showDebugLogPanel();
#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    void recordingTypeChanged(int index);
#endif
signals:
    void driverChanged(QString driver);
    void audioOutputDeviceChanged(QString device);
    void audioInputDeviceChangedSignal(QString device);
    void sampleRateChanged(int rate);
    void bufferSizeChanged(int size);
    void restartApp();
    void uiLanguageChanged(QString lang); // TODO: Implement real-time language switching
    void mixerSettingsChanged();
    void enableScsynthInputsChanged();
    void oscSettingsChanged();
    void midiSettingsChanged();
    // SuperSonic-wide network visibility: 0=Off, 1=Loopback, 2=Network.
    void supersonicNetworkVisibilityChanged(int mode);
    void resetMidi();
    void volumeChanged(int vol);
    void showLineNumbersChanged();
    void showAutoCompletionChanged();
    void showCompletionHelpChanged();
    void showLogChanged();
    void showCuesChanged();
    void showMetroChanged();
    void showButtonsChanged();
    void showFullscreenChanged();
    void showTabsChanged();
    void logAutoScrollChanged();
    void themeChanged();
    void scopeChanged();
    void scopeLabelsChanged();
    void titlesChanged();
    void hideMenuBarInFullscreenChanged();
    void scopeChanged(QString name);
    void transparencyChanged(int t);
    void checkUpdatesChanged();
    void forceCheckUpdates();
    void showContextChanged();
    void checkArgsChanged();
    void synthTriggerTimingGuaranteesChanged();
    void enableExternalSynthsChanged();
    void midiDefaultChannelChanged();
    void logCuesChanged();
    void logSynthsChanged();
    void clearOutputOnRunChanged();
    void autoIndentOnRunChanged();
    void showDebugLogPanelChanged();
#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    // Recording → Type radio toggled. MainWindow::setRecordingMode
    // owns persistence and cross-view sync.
    void recordingModeChangedFromPrefs(int mode);
#endif

    // Keyboard Shortcuts tab. schemeChanged switches the active scheme
    // (1=Emacs, 2=Win, 3=Mac, 4=Custom). applyRequested carries the custom
    // base preset + only the diffs from it; MainWindow owns the write to
    // keyboard-shortcuts.ini + reapply.
    void shortcutSchemeChanged(int mode);
    void shortcutsApplyRequested(QString base, QMap<QString, QString> diffs);

private:
    SonicPiSettings* piSettings;
    SonicPii18n* sonicPii18n;
    std::map<QString, QString> localeNames;
    QStringList available_languages;
    bool i18n;
    int tau_osc_cues_port;

    QTabWidget *prefTabs;

    QCheckBox *mixer_invert_stereo;
    QCheckBox *mixer_force_mono;
    QCheckBox *enable_scsynth_inputs;
    // Side-note shown next to enable_scsynth_inputs ONLY when an ASIO
    // driver is selected. Explains that ASIO is full-duplex by spec —
    // input cannot be disabled separately from output without a
    // driver-level reconfigure that crashes many ASIO drivers.
    QLabel    *asio_input_note;
    // Original tooltip and Qt::Checked state of enable_scsynth_inputs
    // remembered before we override them for ASIO, so they restore
    // intact when leaving ASIO.
    QString    asio_saved_input_tooltip;
    bool       asio_saved_input_checked = false;
    bool       asio_constraint_applied  = false;
    // The driver the engine actually has open right now (last reported
    // by /supersonic/info → updateAudioDeviceConfig). Used by
    // updateAudioDevices to detect "user picked Driver=ASIO but engine
    // hasn't actually moved there yet" — in which case the Output
    // dropdown shows -- None -- instead of the current device name
    // (which would be on a non-ASIO driver and thus misleading).
    QString    m_engineActualDriver;
    // Engine's last-reported sample rate / buffer size. Used by the
    // sample-rate / buffer-size dropdown handlers to suppress no-op
    // switchDevice calls when the user re-picks the already-active
    // value — Qt's `activated(int)` fires on every click whether the
    // selection changed or not, and the engine's setAudioDeviceSetup
    // close-and-reopens the device even on identical params (audible
    // glitch).
    int        m_engineCurrentSampleRate = 0;
    int        m_engineCurrentBufferSize = 0;
    QCheckBox *log_synths;
    QCheckBox *show_debug_log_panel;
    QCheckBox *check_args;
    QCheckBox *clear_output_on_run;
    QCheckBox *log_cues;
    QCheckBox *log_auto_scroll;
    QCheckBox *enable_external_synths_cb;
    QCheckBox *synth_trigger_timing_guarantees_cb;
    QCheckBox *show_line_numbers;
    QCheckBox *auto_indent_on_run;
    QCheckBox *full_screen;
    QCheckBox* goto_buffer_shortcuts;
    QCheckBox *show_log;
    QCheckBox *show_cues;
    QCheckBox *show_metro;
    QCheckBox *show_buttons;
    QCheckBox *show_tabs;
    QCheckBox *check_updates;
    QCheckBox *studio_mode;
    QCheckBox *show_autocompletion;
    QCheckBox *show_completion_help;
    QCheckBox *show_context;

    QComboBox *midi_default_channel_combo;
    QCheckBox *midi_enable_check;
    QCheckBox *osc_public_check;
    QCheckBox *osc_server_enabled_check;


    QButtonGroup *colourModeButtonGroup;
    // Segmented control buttons (checkable, exclusive via the group).
    QPushButton *lightModeCheck;
    QPushButton *darkModeCheck;
    QPushButton *lightProModeCheck;
    QPushButton *darkProModeCheck;
    QPushButton *highContrastModeCheck;

    QSignalMapper *scopeSignalMap;
    QCheckBox *show_scope_labels;
    QCheckBox *show_scopes;
    QCheckBox *show_titles;
    QCheckBox *hide_menubar_in_fullscreen;
    QVBoxLayout *scope_box_kinds_layout;

    QPushButton *check_updates_now;
    QPushButton *visit_sonic_pi_net;
    QPushButton *check_studio_hash;
    QLineEdit   *user_token;
    QLabel *update_info;
    QLabel *midi_in_ports_label;
    QLabel *midi_out_ports_label;
    QLabel *supersonic_ascii_label;
    QLabel *supersonic_version_label;
    QGroupBox *supersonicBox;

    QDial *system_vol_slider;
    QSlider *gui_transparency_slider;

    QComboBox *audio_driver_combo;
    QComboBox *audio_output_combo;
    QComboBox *audio_input_combo;
    QComboBox *audio_sample_rate_combo;
    QComboBox *audio_buffer_size_combo;
#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    // Segmented control buttons (checkable, exclusive via the group).
    QPushButton *recording_type_audio_radio;
    QPushButton *recording_type_av_radio;
    QButtonGroup *recording_type_group;
#endif
    SonicPi::AudioDevicesInfo m_lastAudioDevicesInfo;
    // Cached input-devices payload, used by audioDriverChanged to
    // re-render the input combo with the new driver's filter without
    // waiting for another /supersonic/input-devices push.
    SonicPi::AudioInputDevicesInfo m_lastAudioInputDevicesInfo;
    QLabel *mic_permission_label;
    QPushButton *mic_permission_settings_button;
    QTimer *m_micPermissionTimer = nullptr;
    std::string m_lastMicPermissionStatus;
    QTimer *m_switchTimeoutTimer;

    QComboBox *language_combo;
    QLabel *language_option_label;
    QLabel *language_details_label;
    QLabel *language_info_label;

    QTreeWidget *shortcutTree = nullptr;
    QComboBox *shortcutBaseCombo = nullptr;
    QButtonGroup *shortcutSchemeGroup = nullptr;
    QWidget *shortcutCustomControls = nullptr;
    QPushButton *shortcutEditRowButton = nullptr;
    QLabel *shortcutModifiedLabel = nullptr;
    QString shortcutConfigPath;
    QString shortcutEditBase; // base preset the edit tree is currently built on
    bool m_inShortcutChange = false; // reentrancy guard for the conflict prompt

    // TODO
    QGroupBox* createAudioPrefsTab();
    QGroupBox* createIoPrefsTab();
    QGroupBox* createEditorPrefsTab();
    QGroupBox* createVisualizationPrefsTab();
    QGroupBox* createUpdatePrefsTab();
    QGroupBox* createLanguagePrefsTab();
    QGroupBox* createKeyboardShortcutsTab();
    void reloadShortcutTree();
    void fillShortcutTree(QTreeWidget* tree, const QString& base, const QMap<QString, QString>& overrides, bool editable);
    void currentBaseAndOverrides(QString& base, QMap<QString, QString>& overrides) const;
    void onShortcutItemChanged(QTreeWidgetItem* item, int column);
    void restyleShortcutTree(QTreeWidget* tree, const QString& base);
    void onShortcutSchemeToggled();
    void applyShortcuts();
    void resetShortcutsToBase();
    void importShortcuts();
    void exportShortcuts();
    QMap<QString, QString> collectShortcutDiffs(QString* outBase) const;
    QMap<QString, QString> collectDiffsAgainst(const QString& base) const;

    void add_language_combo_box_entries(QComboBox* combo);

    QString tooltipStrShiftMeta(char key, QString str);

    void connectAll();
    void updateScopeKindVisibility();

};

#endif
