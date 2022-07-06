#ifndef SETTINGSWIDGET_H
#define SETTINGSWIDGET_H

#include "model/settings.h"
#include "utils/sonicpi_i18n.h"

#include <QWidget>

class QSlider;
class QTabWidget;
class QBoxLayout;
class QGroupBox;
class QComboBox;
class QCheckBox;
class QPushButton;
class QLabel;
class QLineEdit;
class QButtonGroup;
class QSignalMapper;
class QVBoxLayout;
class QSizePolicy;

class SettingsWidget : public QWidget
{
    Q_OBJECT

public:
    SettingsWidget(int tau_osc_cues_port, bool i18n, SonicPiSettings *piSettings, SonicPii18n *sonicPii18n, QWidget *parent = nullptr);
    ~SettingsWidget();

    void updateVersionInfo( QString info_string, QString visit, bool sonic_pi_net_visible, bool check_now_visible);
    void updateMidiInPorts( QString in );
    void updateMidiOutPorts( QString out );
    void updateScsynthInfo(QString scsynthInfo);
    void updateScopeNames(std::vector<QString>);
    void updateSelectedUILanguage(QString lang);

public slots:
    void updateUILanguage(int index);

private slots:
    void update_mixer_invert_stereo();
    void update_mixer_force_mono();
    void updateEnableScsynthInputs();
    void toggleOscServer();
    void toggleMidi();
    void forceMidiReset();
    void changeMainVolume(int);
    void toggleLineNumbers();
    void showAutoCompletion();
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

signals:
    void restartApp();
    void uiLanguageChanged(QString lang); // TODO: Implement real-time language switching
    void mixerSettingsChanged();
    void enableScsynthInputsChanged();
    void oscSettingsChanged();
    void midiSettingsChanged();
    void resetMidi();
    void volumeChanged(int vol);
    void showLineNumbersChanged();
    void showAutoCompletionChanged();
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
    QCheckBox *log_synths;
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
    QCheckBox *show_context;

    QComboBox *midi_default_channel_combo;
    QCheckBox *midi_enable_check;
    QCheckBox *osc_public_check;
    QCheckBox *osc_server_enabled_check;

    QButtonGroup *colourModeButtonGroup;
    QCheckBox *lightModeCheck;
    QCheckBox *darkModeCheck;
    QCheckBox *lightProModeCheck;
    QCheckBox *darkProModeCheck;
    QCheckBox *highContrastModeCheck;

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
    QLabel *scsynth_info_label;

    QSlider *system_vol_slider;
    QSlider *gui_transparency_slider;

    QComboBox *language_combo;
    QLabel *language_option_label;
    QLabel *language_details_label;
    QLabel *language_info_label;

    // TODO
    QGroupBox* createAudioPrefsTab();
    QGroupBox* createIoPrefsTab();
    QGroupBox* createEditorPrefsTab();
    QGroupBox* createVisualizationPrefsTab();
    QGroupBox* createUpdatePrefsTab();
    QGroupBox* createLanguagePrefsTab();

    void add_language_combo_box_entries(QComboBox* combo);

    QString tooltipStrShiftMeta(char key, QString str);

    void connectAll();
    void updateScopeKindVisibility();

};

#endif
