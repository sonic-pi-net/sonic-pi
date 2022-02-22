#include "settingswidget.h"
#include "utils/sonicpi_i18n.h"

#include <QSettings>
#include <QVBoxLayout>
#include <QGridLayout>
#include <QGroupBox>
#include <QButtonGroup>
#include <QNetworkInterface>
#include <QDesktopServices>
#include <QCheckBox>
#include <QComboBox>
#include <QUrl>
#include <iostream>
#include <QLabel>
#include <QPushButton>
#include <QSignalMapper>
#include <QVBoxLayout>
#include <QMessageBox>
#include <QSize>

/**
 * Default Constructor
 */
SettingsWidget::SettingsWidget(int tau_osc_cues_port, bool i18n, SonicPiSettings *piSettings, SonicPii18n *sonicPii18n, QWidget *parent) {
    this->piSettings = piSettings;
    this->i18n = i18n;
    this->sonicPii18n = sonicPii18n;
    this->available_languages = sonicPii18n->getAvailableLanguages();
    this->tau_osc_cues_port = tau_osc_cues_port;

    prefTabs = new QTabWidget();

    QGridLayout *grid = new QGridLayout;
    grid->addWidget(prefTabs, 0, 0);

    QGroupBox *audio_prefs_box = createAudioPrefsTab();
    prefTabs->addTab(audio_prefs_box, tr("Audio"));

    QGroupBox *ioTab = createIoPrefsTab();
    prefTabs->addTab(ioTab, tr("IO"));

    QGroupBox *editorTab = createEditorPrefsTab();
    prefTabs->addTab(editorTab, tr("Editor"));

    QGroupBox *visualizationTab = createVisualizationPrefsTab();
    prefTabs->addTab(visualizationTab, tr("Visuals"));

    QGroupBox *update_prefs_box = createUpdatePrefsTab();
    prefTabs->addTab(update_prefs_box, tr("Updates"));

    QGroupBox *language_prefs_box = createLanguagePrefsTab();
    prefTabs->addTab(language_prefs_box, tr("Language"));

    if (piSettings->language == "system_language") {
      if (!sonicPii18n->isSystemLanguageAvailable()) {
          QGroupBox *translation_box = new QGroupBox("Translation");
          QVBoxLayout *translation_box_layout = new QVBoxLayout;
          QLabel *go_translate = new QLabel;
          go_translate->setOpenExternalLinks(true);
          go_translate->setText(
                  "Sonic Pi hasn't been translated to " +
                  QLocale::languageToString(QLocale::system().language()) +
                  " yet.<br/>" +
                  "We rely on crowdsourcing to help create and maintain translations.<br/>" +
                  "<a href=\"https://github.com/sonic-pi-net/sonic-pi/blob/main/TRANSLATION.md\">" +
                  "Please consider helping to translate Sonic Pi to your language.</a> "
                  );
          go_translate->setTextFormat(Qt::RichText);
          translation_box_layout->addWidget(go_translate);
          translation_box->setLayout(translation_box_layout);

          grid->addWidget(translation_box, 3, 0, 1, 2);
      }
    }

    settingsChanged();
    connectAll();
    setLayout(grid);
}

/**
 * Destructor
 */
SettingsWidget::~SettingsWidget() {
}

QSize SettingsWidget::sizeHint() const
{
  return QSize(100, 100);
}

/**
 * Create Audio Preferences Tab of Settings Widget
 */
QGroupBox* SettingsWidget::createAudioPrefsTab() {

    QGroupBox *volBox = new QGroupBox(tr("Main Volume"));
    volBox->setToolTip(tr("Use this slider to change the system volume."));
    QHBoxLayout *vol_box = new QHBoxLayout;
    system_vol_slider = new QSlider(this);
    vol_box->addWidget(system_vol_slider);
    volBox->setLayout(vol_box);

    QGroupBox *advancedAudioBox = new QGroupBox(tr("Audio Output"));
    advancedAudioBox->setToolTip(tr("Advanced audio settings for working with\nexternal PA systems when performing with Sonic Pi."));
    mixer_invert_stereo = new QCheckBox(tr("Invert stereo"));
    mixer_invert_stereo->setToolTip(tr("Toggle stereo inversion.\nIf enabled, audio sent to the left speaker will\nbe routed to the right speaker and vice versa."));
    mixer_force_mono = new QCheckBox(tr("Force mono"));
    mixer_force_mono->setToolTip(tr("Toggle mono mode.\nIf enabled both right and left audio is mixed and\nthe same signal is sent to both speakers.\nUseful when working with external systems that\ncan only handle mono."));


    QVBoxLayout *advanced_audio_box_layout = new QVBoxLayout;
    advanced_audio_box_layout->addWidget(mixer_invert_stereo);
    advanced_audio_box_layout->addWidget(mixer_force_mono);
    advancedAudioBox->setLayout(advanced_audio_box_layout);


    QGroupBox *synths_box = new QGroupBox(tr("Synths and FX"));
    synths_box->setToolTip(tr("Modify behaviour of synths and FX"));

    check_args = new QCheckBox(tr("Safe mode"));
    check_args->setToolTip(tr("Toggle synth argument checking functions.\nIf disabled, certain synth opt values may\ncreate unexpectedly loud or uncomfortable sounds."));

    synth_trigger_timing_guarantees_cb = new QCheckBox(tr("Enforce timing guarantees"));
    synth_trigger_timing_guarantees_cb->setToolTip(tr("When enabled, Sonic Pi will refuse\nto trigger synths and FX if\nit is too late to do so\n\nWhen disabled, Sonic Pi will always\nattempt to trigger synths and FX\neven when a little late."));

    enable_external_synths_cb = new QCheckBox(tr("Enable external synths/FX"));
    enable_external_synths_cb->setToolTip(tr("When enabled, Sonic Pi will allow\nsynths and FX loaded via load_synthdefs\nto be triggered.\n\nWhen disabled, Sonic Pi will complain\nwhen you attempt to use a synth or FX\nwhich isn't recognised."));

    QVBoxLayout *synths_box_layout = new QVBoxLayout;
    synths_box_layout->addWidget(check_args);
    synths_box_layout->addWidget(synth_trigger_timing_guarantees_cb);
    synths_box_layout->addWidget(enable_external_synths_cb);
    synths_box->setLayout(synths_box_layout);

    QGroupBox *audio_prefs_box = new QGroupBox();
    QGridLayout *audio_prefs_box_layout = new QGridLayout;

    audio_prefs_box_layout->addWidget(volBox, 0, 0, 0, 1);
    audio_prefs_box_layout->addWidget(synths_box, 0, 1);
    audio_prefs_box_layout->addWidget(advancedAudioBox, 1, 1);
    audio_prefs_box->setLayout(audio_prefs_box_layout);
    return audio_prefs_box;
}

/**
 * create io tab of settings widget
 */
QGroupBox* SettingsWidget::createIoPrefsTab() {
    QGroupBox *ioTab = new QGroupBox();

    QGroupBox *network_box = new QGroupBox(tr("Networked OSC"));
    network_box->setToolTip(tr("Sonic Pi can send and receive Open Sound Control messages\nto and from other programs or computers\n via the currently connected network."));

    QLabel* osc_disabled_label = new QLabel();
    osc_disabled_label->setAccessibleName("incoming-osc-disabled-label");
    QLabel *network_ip_label = new QLabel();
    QString osc_disabled_trans = tr("(To enable 'Allow OSC from other computers',\nalso enable 'Allow incoming OSC')");
    QString ip_address_trans = tr("Local IP address");
    QString port_num_trans = tr("Incoming OSC port");
    QString ip_address = "";
    QString all_ip_addresses  = "";

    osc_disabled_label->setText(osc_disabled_trans);
    QList<QHostAddress> list = QNetworkInterface::allAddresses();

    for(int nIter=0; nIter<list.count(); nIter++)
    {
        if(!list[nIter].isLoopback()) {
            if (list[nIter].protocol() == QAbstractSocket::IPv4Protocol ) {
                if (ip_address.isEmpty()) {
                    ip_address = list[nIter].toString();
                }
                all_ip_addresses = all_ip_addresses + list[nIter].toString() + "\n";
            }
        }
    }

    if (ip_address.isEmpty()) {
        ip_address = tr("Unavailable");
    }

    network_ip_label->setText(ip_address_trans + ": " + ip_address + "\n" + port_num_trans + + ": " + QString::number(tau_osc_cues_port));
    network_ip_label->setToolTip(all_ip_addresses);

    osc_public_check = new QCheckBox(tr("Allow OSC from other computers"));
    osc_public_check->setToolTip(tr("When checked, Sonic Pi will let you send and receive OSC messages to and from remote machines.\n When unchecked, only sending and receiving from the local machine will be enabled."));

    osc_server_enabled_check = new QCheckBox(tr("Allow incoming OSC"));
    osc_server_enabled_check->setToolTip(tr("When checked, Sonic Pi will listen for OSC messages.\n When unchecked no OSC messages will be received."));

    QVBoxLayout *network_box_layout = new QVBoxLayout;
    network_box_layout->addWidget(osc_server_enabled_check);
    network_box_layout->addWidget(osc_public_check);
    network_box_layout->addWidget(osc_disabled_label);
    network_box_layout->addWidget(network_ip_label);
    network_box->setLayout(network_box_layout);

    QGroupBox *midi_config_box = new QGroupBox(tr("MIDI Configuration"));
    midi_config_box->setToolTip(tr("Configure MIDI behaviour"));

    QGroupBox *midi_ports_box = new QGroupBox(tr("MIDI Ports"));
    midi_ports_box->setToolTip(tr("List all connected MIDI Ports"));

    midi_enable_check = new QCheckBox(tr("Enable incoming MIDI cues"));
    midi_enable_check->setToolTip(tr("Enable or disable automatic conversion of incoming MIDI messages to cue events"));

    QPushButton *midi_reset_button = new QPushButton(tr("Reset MIDI"));
    midi_reset_button->setFlat(true);
    midi_reset_button->setToolTip(tr("Reset MIDI subsystems \n(Required to detect device changes on macOS)" ));

    midi_default_channel_combo = new QComboBox();
    midi_default_channel_combo->addItem("*");
    // TODO Loop
    midi_default_channel_combo->addItem("1");
    midi_default_channel_combo->addItem("2");
    midi_default_channel_combo->addItem("3");
    midi_default_channel_combo->addItem("4");
    midi_default_channel_combo->addItem("5");
    midi_default_channel_combo->addItem("6");
    midi_default_channel_combo->addItem("7");
    midi_default_channel_combo->addItem("8");
    midi_default_channel_combo->addItem("9");
    midi_default_channel_combo->addItem("10");
    midi_default_channel_combo->addItem("11");
    midi_default_channel_combo->addItem("12");
    midi_default_channel_combo->addItem("13");
    midi_default_channel_combo->addItem("14");
    midi_default_channel_combo->addItem("15");
    midi_default_channel_combo->addItem("16");
    midi_default_channel_combo->setMaxVisibleItems(17);
    midi_default_channel_combo->setMinimumContentsLength(2);
    midi_default_channel_combo->setSizeAdjustPolicy(QComboBox::AdjustToMinimumContentsLengthWithIcon) ;

    QLabel *midi_default_channel_label = new QLabel;
    midi_default_channel_label->setText(tr("Default MIDI channel"));
    midi_default_channel_label->setToolTip(tr("Default MIDI Channel to send messages to (* means all)"));

    QGridLayout *midi_default_channel_layout = new QGridLayout();

    midi_default_channel_combo->setToolTip(tr("Default MIDI Channel to send messages to  (* means all)"));

    midi_default_channel_layout->addWidget(midi_default_channel_combo, 0, 0);
    midi_default_channel_layout->addWidget(midi_default_channel_label, 0, 1);

    midi_in_ports_label = new QLabel;
    midi_out_ports_label = new QLabel;
    midi_in_ports_label->setFont(QFont("Hack"));
    midi_out_ports_label->setFont(QFont("Hack"));
    midi_in_ports_label->setAccessibleName("midi-in-ports-label");
    midi_out_ports_label->setAccessibleName("midi-out-ports-label");
    midi_in_ports_label->setText(tr("No connected input devices"));
    midi_out_ports_label->setText(tr("No connected output devices"));
    midi_in_ports_label->setToolTip(tr("MIDI input devices send MIDI messages directly to\nSonic Pi and are received as cue events\n(similar to incoming OSC messages and internal cues)"));
    midi_out_ports_label->setToolTip(tr("MIDI output devices receive MIDI messages directly from\nSonic Pi which can be sent via the midi_* fns"));

    QVBoxLayout *midi_ports_box_layout = new QVBoxLayout;
    QVBoxLayout *midi_config_box_layout = new QVBoxLayout;
    midi_config_box_layout->addWidget(midi_enable_check);
    midi_config_box_layout->addLayout(midi_default_channel_layout);

    midi_ports_box_layout->addWidget(midi_in_ports_label);
    midi_ports_box_layout->addWidget(midi_out_ports_label);


    //midi_ports_box_layout->addWidget(midi_reset_button);


    connect(midi_reset_button, SIGNAL(clicked()), this, SLOT(forceMidiReset()));

    midi_ports_box->setLayout(midi_ports_box_layout);
    midi_config_box->setLayout(midi_config_box_layout);

    QGridLayout *io_tab_layout = new QGridLayout();
    io_tab_layout->addWidget(midi_ports_box, 0, 0, 0, 1);
    io_tab_layout->addWidget(midi_config_box, 0, 1);
    io_tab_layout->addWidget(network_box, 1, 1);

    ioTab->setLayout(io_tab_layout);
    return ioTab;
}

/**
 * create Editor Tab of Preferences Widget
 */
QGroupBox* SettingsWidget::createEditorPrefsTab() {
    QGroupBox *editor_box = new QGroupBox();
    QGroupBox *editor_display_box = new QGroupBox(tr("Show and Hide"));
    editor_display_box->setToolTip(tr("Configure editor display options."));
    QGroupBox *editor_look_feel_box = new QGroupBox(tr("Look and Feel"));
    editor_look_feel_box->setToolTip(tr("Configure editor look and feel."));
    QGroupBox *automation_box = new QGroupBox(tr("Automation / Misc"));
    automation_box->setToolTip(tr("Configure automation and other features."));

    auto_indent_on_run = new QCheckBox(tr("Auto-align"));
    auto_indent_on_run->setToolTip(tr("Automatically align code on Run"));

    show_line_numbers = new QCheckBox(tr("Show line numbers"));
    show_line_numbers->setToolTip(tr("Toggle line number visibility."));

    show_autocompletion = new QCheckBox(tr("Show code completion"));
    show_autocompletion->setToolTip(tr("When enabled, Sonic Pi's editor will attempt to autocomplete your code with suggestions. When disabled, these suggestions will not be visible."));

    show_context = new QCheckBox(tr("Show code context"));
    show_context->setToolTip(tr("When enabled, Sonic Pi's editor will show a pane which will display context-specific information for the code such as the current line and position of the cursor."));

    show_log = new QCheckBox(tr("Show log"));
    show_log->setToolTip(tooltipStrShiftMeta('L', tr("Toggle visibility of the log.")));
    show_log->setChecked(true);

    show_cues = new QCheckBox(tr("Show cue log"));
    show_cues->setToolTip(tooltipStrShiftMeta('C', tr("Toggle visibility of cue log which displays internal cues & incoming OSC/MIDI messages.")));
    show_cues->setChecked(true);

    show_buttons = new QCheckBox(tr("Show buttons"));
    show_buttons->setToolTip(tooltipStrShiftMeta('B', tr("Toggle visibility of the control buttons.")));
    show_buttons->setChecked(true);
    show_tabs = new QCheckBox(tr("Show tabs"));
    show_tabs->setChecked(true);
    show_tabs->setToolTip(tr("Toggle visibility of the buffer selection tabs."));
    full_screen = new QCheckBox(tr("Full screen"));
    full_screen->setToolTip(tooltipStrShiftMeta('F', tr("Toggle full screen mode.")));

    colourModeButtonGroup = new QButtonGroup(this);
    lightModeCheck = new QCheckBox(tr("Light"));
    darkModeCheck = new QCheckBox(tr("Dark"));
    lightProModeCheck = new QCheckBox(tr("Pro Light"));
    darkProModeCheck = new QCheckBox(tr("Pro Dark"));
    highContrastModeCheck = new QCheckBox(tr("High Contrast"));
    colourModeButtonGroup->addButton(lightModeCheck, 0);
    colourModeButtonGroup->addButton(darkModeCheck, 1);
    colourModeButtonGroup->addButton(lightProModeCheck, 2);
    colourModeButtonGroup->addButton(darkProModeCheck, 3);
    colourModeButtonGroup->addButton(highContrastModeCheck, 4);

    QVBoxLayout *editor_display_box_layout = new QVBoxLayout;
    QVBoxLayout *editor_box_look_feel_layout = new QVBoxLayout;
    QVBoxLayout *automation_box_layout = new QVBoxLayout;
    QGridLayout *gridEditorPrefs = new QGridLayout;

    editor_display_box_layout->addWidget(show_line_numbers);
    editor_display_box_layout->addWidget(show_autocompletion);
    editor_display_box_layout->addWidget(show_context);
    editor_display_box_layout->addWidget(show_log);
    editor_display_box_layout->addWidget(show_cues);
    editor_display_box_layout->addWidget(show_buttons);
    editor_display_box_layout->addWidget(show_tabs);
    editor_box_look_feel_layout->addWidget(lightModeCheck);
    editor_box_look_feel_layout->addWidget(darkModeCheck);
    editor_box_look_feel_layout->addWidget(lightProModeCheck);
    editor_box_look_feel_layout->addWidget(darkProModeCheck);
    editor_box_look_feel_layout->addWidget(highContrastModeCheck);

    editor_display_box->setLayout(editor_display_box_layout);
    editor_look_feel_box->setLayout(editor_box_look_feel_layout);

    automation_box_layout->addWidget(auto_indent_on_run);
    automation_box_layout->addWidget(full_screen);

    automation_box->setLayout(automation_box_layout);

    QGroupBox *debug_box = new QGroupBox(tr("Logging"));
    debug_box->setToolTip(tr("Configure debug behaviour"));

    log_synths = new QCheckBox(tr("Log synths"));
    log_synths->setToolTip(tr("Toggle log messages.\nIf disabled, activity such as synth and sample\ntriggering will not be printed to the log by default."));

    clear_output_on_run = new QCheckBox(tr("Clear log on run"));
    clear_output_on_run->setToolTip(tr("Toggle log clearing on run.\nIf enabled, the log is cleared each\ntime the run button is pressed."));

    log_cues = new QCheckBox(tr("Log cues"));
    log_cues->setToolTip(tr("Enable or disable logging of cues.\nIf disabled, cues will still trigger.\nHowever, they will not be visible in the logs."));

    log_auto_scroll = new QCheckBox(tr("Auto-scroll log"));
    log_auto_scroll->setToolTip(tr("Toggle log auto scrolling.\nIf enabled the log is scrolled to the bottom after every new message is displayed."));

    QVBoxLayout *debug_box_layout = new QVBoxLayout;
    debug_box_layout->addWidget(log_synths);
    debug_box_layout->addWidget(log_cues);
    debug_box_layout->addWidget(log_auto_scroll);
    debug_box_layout->addWidget(clear_output_on_run);
    debug_box->setLayout(debug_box_layout);



    gridEditorPrefs->addWidget(editor_display_box, 0, 0);
    gridEditorPrefs->addWidget(editor_look_feel_box, 0, 1);
    gridEditorPrefs->addWidget(automation_box, 1, 1);
    gridEditorPrefs->addWidget(debug_box, 1, 0);


    editor_box->setLayout(gridEditorPrefs);
    return editor_box;
}

/**
 * Create Visualization Preferences Tab of Settings Widget
 */
QGroupBox* SettingsWidget::createVisualizationPrefsTab() {
    QGroupBox *viz_box = new QGroupBox();
    viz_box->setToolTip(tr("Settings useful for performing with Sonic Pi"));

    QGridLayout* viz_tab_layout = new QGridLayout();

    QGroupBox *scope_box = new QGroupBox(tr("Show and Hide Scope"));
    QGroupBox *scope_box_kinds = new QGroupBox(tr("Scope Kinds"));

    //QVBoxLayout *scope_box_kinds_layout = new QVBoxLayout;
    scope_box_kinds_layout = new QVBoxLayout;

    QVBoxLayout *scope_box_layout = new QVBoxLayout;

    scopeSignalMap = new QSignalMapper(this);
    show_scopes = new QCheckBox(tr("Show Scopes"));
    show_scopes->setToolTip(tr("Toggle the visibility of the audio oscilloscopes."));
    show_scope_labels = new QCheckBox(tr("Show Scope Labels"));
    show_scope_labels->setToolTip(tr("Toggle the visibility of the labels for the audio oscilloscopes"));
    show_scope_labels->setChecked(true);
    scope_box_kinds->setLayout(scope_box_kinds_layout);
    scope_box_kinds->setToolTip(tr("The audio oscilloscope comes in several flavours which may\nbe viewed independently or all together:\n\nLissajous - illustrates the phase relationship between the left and right channels\nMirror Stereo - simple left/right composite wave, with left on top, right on bottom\nMono - shows a combined view of the left and right channels (using RMS)\nSpectrum - shows the sound frequencies as a spectrum, from low to high frequencies\nStereo - shows two independent scopes for left and right channels"));
    scope_box_layout->addWidget(show_scopes);
    scope_box_layout->addWidget(show_scope_labels);
    scope_box->setLayout(scope_box_layout);
    viz_tab_layout->addWidget(scope_box, 0, 0);
    viz_tab_layout->addWidget(scope_box_kinds, 1, 0);

    QGroupBox *transparency_box = new QGroupBox(tr("Transparency"));
    QGridLayout *transparency_box_layout = new QGridLayout;
    gui_transparency_slider = new QSlider(this);
    transparency_box_layout->addWidget(gui_transparency_slider);
    transparency_box->setLayout(transparency_box_layout);

//#if defined(Q_OS_LINUX)
//    // do nothing
//#else
    viz_tab_layout->addWidget(transparency_box, 0, 1, 0, 1);
//#endif

    viz_box->setLayout(viz_tab_layout);

    return viz_box;
}

/**
 * create Update Preferences Tab of Settings Widget
 */
QGroupBox* SettingsWidget::createUpdatePrefsTab() {
    QGroupBox *update_box = new QGroupBox(tr("Updates"));
    QSizePolicy updatesPrefSizePolicy(QSizePolicy::Minimum, QSizePolicy::Fixed);
    check_updates = new QCheckBox(tr("Check for updates"));
    update_box->setSizePolicy(updatesPrefSizePolicy);
    check_updates->setToolTip(tr("Toggle automatic update checking.\nThis check involves sending anonymous information about your platform and version."));
    check_updates_now = new QPushButton(tr("Check now"));
    check_updates_now->setFlat(true);
    check_updates_now->setToolTip(tr("Force a check for updates now.\nThis check involves sending anonymous information about your platform and version."));
    visit_sonic_pi_net = new QPushButton(tr("Get update"));
    visit_sonic_pi_net->setToolTip(tr("Visit http://sonic-pi.net to download new version"));
    visit_sonic_pi_net->setVisible(false);

    QGroupBox *update_info_box = new QGroupBox(tr("Update Info"));
    update_info_box->setMaximumWidth(350);
    QVBoxLayout *update_info_box_layout = new QVBoxLayout;
    update_info = new QLabel(tr("Sonic Pi update info"));
    update_info->setWordWrap(true);
    update_info_box_layout->addWidget(update_info);
    update_info_box->setLayout(update_info_box_layout);

    QVBoxLayout *update_box_layout = new QVBoxLayout;
    update_box_layout->addWidget(check_updates);

    update_box_layout->addWidget(check_updates_now);
    update_box_layout->addWidget(visit_sonic_pi_net);
    update_box->setLayout(update_box_layout);



    QGroupBox *update_prefs_box = new QGroupBox();
    QGridLayout *update_prefs_box_layout = new QGridLayout;
    update_prefs_box_layout->addWidget(update_info_box, 0, 0);
    update_prefs_box_layout->addWidget(update_box, 0, 1);
    update_prefs_box->setLayout(update_prefs_box_layout);
    return update_prefs_box;
}

/**
 * create Language Preferences Tab of Settings Widget
 */
QGroupBox* SettingsWidget::createLanguagePrefsTab() {
    QGroupBox *language_box = new QGroupBox(tr("Language"));
    language_box->setToolTip(tr("Configure language settings"));
    QSizePolicy languagePrefSizePolicy(QSizePolicy::Minimum, QSizePolicy::Fixed);
    language_box->setSizePolicy(languagePrefSizePolicy);

    language_option_label = new QLabel;
    language_option_label->setText(tr("UI & Tutorial Language (Requires a restart to take effect)"));
    language_option_label->setToolTip(tr("Change the language of the UI & Tutorial (Requires a restart to take effect)"));

    language_combo = new QComboBox();
    add_language_combo_box_entries(language_combo);
    language_combo->setToolTip(tr("Change the language of the UI & Tutorial"));
    language_combo->setMinimumContentsLength(2);
    language_combo->setSizeAdjustPolicy(QComboBox::AdjustToMinimumContentsLengthWithIcon);

    language_details_label = new QLabel;

    language_info_label = new QLabel;
    language_info_label->setText(tr("Translations have been generously provided by volunteers \non https://hosted.weblate.org/projects/sonic-pi/. Thank you! :)"));

    QVBoxLayout *language_box_layout = new QVBoxLayout;

    language_box_layout->addWidget(language_option_label);
    language_box_layout->addWidget(language_combo);
    language_box_layout->addWidget(language_details_label);
    language_box_layout->addWidget(language_info_label);

    language_box->setLayout(language_box_layout);

    QGroupBox *language_prefs_box = new QGroupBox();
    QGridLayout *language_prefs_box_layout = new QGridLayout;
    language_prefs_box_layout->addWidget(language_box, 0, 0, 0, 0);
    language_prefs_box->setLayout(language_prefs_box_layout);
    return language_prefs_box;
}

// TODO utils?
QString SettingsWidget::tooltipStrShiftMeta(char key, QString str) {
#ifdef Q_OS_MAC
    return QString("%1 (⇧⌘%2)").arg(str).arg(key);
#else
    return QString("%1 (Shift-alt-%2)").arg(str).arg(key);
#endif
}

void SettingsWidget::updateScopeNames( std::vector<QString> names ) {
    piSettings->scope_names = names;
    for( auto name : names ) {
        QCheckBox* cb = new QCheckBox( name );
        cb->setChecked( piSettings->isScopeActive(name));
        scopeSignalMap->setMapping( cb, cb );
        scope_box_kinds_layout->addWidget(cb);
        connect(cb, SIGNAL(clicked()), scopeSignalMap, SLOT(map()));
    }
    connect( scopeSignalMap, SIGNAL(mapped(QWidget*)), this, SLOT(toggleScope(QWidget*)));
}

void SettingsWidget::updateScopeKindVisibility() {
  for (int i = 0; i < scope_box_kinds_layout->count(); ++i) {
    QCheckBox *cb = qobject_cast<QCheckBox*>(scope_box_kinds_layout->itemAt(i)->widget());
    cb->setChecked(piSettings->isScopeActive(cb->text()));
  }
}

void SettingsWidget::updateSelectedUILanguage(QString lang) {
  int index = available_languages.indexOf(lang);
  language_combo->setCurrentIndex(index);
}

void SettingsWidget::toggleScope( QWidget* qw ) {
  QCheckBox* cb = static_cast<QCheckBox*>(qw);
  //QSettings settings(QSettings::IniFormat, QSettings::UserScope,    "sonic-pi.net", "gui-settings");
  //piSettings->setValue("prefs/scope/show-"+cb->text().toLower(), cb->isChecked() );
  QString name = cb->text();
  piSettings->setScopeState( name, cb->isChecked() );
  emit scopeChanged(name);
}

// TODO: Implement real-time language switching
void SettingsWidget::updateUILanguage(int index) {
    QString lang = available_languages[index];
    std::cout << "Changed language to " << lang.toUtf8().constData() << std::endl;
    if (lang != piSettings->language) {
        std::cout << "Current language:  " << piSettings->language.toUtf8().constData() << std::endl;
        std::cout << "New language selected: " << lang.toUtf8().constData() << std::endl;
        QString old_lang = sonicPii18n->getNativeLanguageName(piSettings->language);
        QString new_lang = sonicPii18n->getNativeLanguageName(lang);

        // Load new language
        //QString language = sonicPii18n->determineUILanguage(lang);
        //sonicPii18n->loadTranslations(language);
        //QString title_new = tr("Updated the UI language from %s to %s").arg();

        QMessageBox msgBox(this);
        msgBox.setText(QString(tr("You've selected a new language: %1")).arg(new_lang));
        QString info_text = (
          tr("Do you want to apply this language?")
          + "\n"
          + tr("The new language will be applied when you next start Sonic Pi.")
        );

        if (lang == "system_language") {
          info_text = tr("System languages found: %1").arg(sonicPii18n->getNativeLanguageNames(sonicPii18n->getSystemLanguages()).join(", ")) + "\n" + info_text;
        }

        msgBox.setInformativeText(info_text);
        QPushButton *applyButton = msgBox.addButton(tr("Apply"), QMessageBox::ActionRole);
        QPushButton *dismissButton = msgBox.addButton(tr("Cancel"), QMessageBox::RejectRole);
        msgBox.setDefaultButton(applyButton);
        msgBox.setIcon(QMessageBox::Question);
        msgBox.exec();

        if (msgBox.clickedButton() == (QAbstractButton*)applyButton) {
          piSettings->language = lang;
          updateSelectedUILanguage(piSettings->language);
          emit uiLanguageChanged(piSettings->language);

          language_details_label->setText(
            tr("<b>The new language will be applied when you next start Sonic Pi.</b><br>")
            + tr("Current UI language: %1\n").arg(sonicPii18n->getNativeLanguageName(sonicPii18n->currentlyLoadedLanguage()))
          );

          QMessageBox restartMsgBox(this);
          restartMsgBox.setText(QString(tr("Restart Sonic Pi?")));
          QString info_text = (tr("Do you want to restart Sonic Pi now? This will stop any current runs & recordings."));
          QPushButton *restartButton = restartMsgBox.addButton(tr("Restart"), QMessageBox::ActionRole);
          QPushButton *dismissButton = restartMsgBox.addButton(tr("Dismiss"), QMessageBox::RejectRole);
          restartMsgBox.setInformativeText(info_text);
          restartMsgBox.setDefaultButton(dismissButton);
          restartMsgBox.setIcon(QMessageBox::Question);
          restartMsgBox.exec();
          if (restartMsgBox.clickedButton() == (QAbstractButton*)restartButton) {
            emit restartApp();
          }
            //emit uiLanguageChanged(lang);
        } else if (msgBox.clickedButton() == (QAbstractButton*)dismissButton) {
            // Don't apply the new language settings
            updateSelectedUILanguage(piSettings->language);
            emit uiLanguageChanged(piSettings->language);
        }

    }
}

void SettingsWidget::update_mixer_invert_stereo() {
    emit mixerSettingsChanged();
}

void SettingsWidget::update_mixer_force_mono() {
    emit mixerSettingsChanged();
}

void SettingsWidget::toggleOscServer() {
    emit oscSettingsChanged();
}

void SettingsWidget::toggleMidi() {
    emit midiSettingsChanged();
}

void SettingsWidget::forceMidiReset() {
    emit resetMidi();
}

void SettingsWidget::updateMidiInPorts( QString in ) {
    midi_in_ports_label->setText( in );
}

void SettingsWidget::updateMidiOutPorts( QString out ) {
    midi_out_ports_label->setText( out );
}

void SettingsWidget::changeMainVolume(int vol) {
    emit volumeChanged(vol);
}

void SettingsWidget::toggleLineNumbers() {
    emit showLineNumbersChanged();
}

void SettingsWidget::showAutoCompletion() {
  emit showAutoCompletionChanged();
}

void SettingsWidget::showContext() {
  emit showContextChanged();
}

void SettingsWidget::toggleLog() {
    emit showLogChanged();
}

void SettingsWidget::toggleCuesLog() {
    emit showCuesChanged();
}

void SettingsWidget::toggleButtons() {
    emit showButtonsChanged();
}

void SettingsWidget::toggleFullScreen() {
    emit showFullscreenChanged();
}

void SettingsWidget::toggleTabs() {
    emit showTabsChanged();
}

void SettingsWidget::toggleLogAutoScroll() {
    emit logAutoScrollChanged();
}

void SettingsWidget::updateColourTheme() {
    emit themeChanged();
}

void SettingsWidget::toggleScope() {
    emit scopeChanged();
}

void SettingsWidget::toggleScopeLabels() {
    emit scopeLabelsChanged();
}

void SettingsWidget::updateTransparency(int t) {
    emit transparencyChanged(t);
}

void SettingsWidget::toggleCheckUpdates() {
    emit checkUpdatesChanged();
}

void SettingsWidget::checkForUpdatesNow() {
    emit forceCheckUpdates();
}

void SettingsWidget::checkArgs() {
  emit checkArgsChanged();
}

void SettingsWidget::synthTriggerTimingGuarantees() {
  emit synthTriggerTimingGuaranteesChanged();
}

void SettingsWidget::enableExternalSynths() {
  emit enableExternalSynthsChanged();
}

void SettingsWidget::midiDefaultChannel() {
  emit midiDefaultChannelChanged();
}

void SettingsWidget::logCues() {
  emit logCuesChanged();
}

void SettingsWidget::logSynths() {
  emit logSynthsChanged();
}

void SettingsWidget::clearOutputOnRun() {
  emit clearOutputOnRunChanged();
}


void SettingsWidget::autoIndentOnRun() {
  emit autoIndentOnRunChanged();
}

void SettingsWidget::openSonicPiNet() {
  QDesktopServices::openUrl(QUrl("https://sonic-pi.net", QUrl::TolerantMode));
}

void SettingsWidget::updateVersionInfo( QString info_string, QString visit, bool sonic_pi_net_visible, bool check_now_visible) {
    update_info->setText( info_string );
    visit_sonic_pi_net->setText( visit );
    visit_sonic_pi_net->setVisible(sonic_pi_net_visible);
    check_updates_now->setVisible(check_now_visible);
}

void SettingsWidget::updateSettings() {

    std::cout << "[GUI] - update settings" << std::endl;
    piSettings->language = available_languages[language_combo->currentIndex()];
    piSettings->mixer_invert_stereo = mixer_invert_stereo->isChecked();
    piSettings->mixer_force_mono = mixer_force_mono->isChecked();
    piSettings->check_args = check_args->isChecked();
    piSettings->synth_trigger_timing_guarantees = synth_trigger_timing_guarantees_cb->isChecked();
    piSettings->enable_external_synths = enable_external_synths_cb->isChecked();
    piSettings->main_volume = system_vol_slider->value();

    piSettings->osc_server_enabled = osc_server_enabled_check->isChecked();
    piSettings->osc_public = osc_server_enabled_check->isChecked() && osc_public_check->isChecked();
    osc_public_check->setEnabled(piSettings->osc_server_enabled);
    if(!osc_server_enabled_check->isChecked()) {
      osc_public_check->setChecked(false);
    }
    piSettings->midi_default_channel = midi_default_channel_combo->currentIndex();
    piSettings->midi_default_channel_str = midi_default_channel_combo->currentText(); // TODO find a more elegant solution
    piSettings->midi_enabled = midi_enable_check->isChecked();

    piSettings->auto_indent_on_run = auto_indent_on_run->isChecked();
    piSettings->show_line_numbers = show_line_numbers->isChecked();
    piSettings->show_autocompletion = show_autocompletion->isChecked();
    piSettings->show_context = show_context->isChecked();
    piSettings->show_log = show_log->isChecked();
    piSettings->show_cues = show_cues->isChecked();
    piSettings->show_buttons = show_buttons->isChecked();
    piSettings->show_tabs = show_tabs->isChecked();
    piSettings->full_screen = full_screen->isChecked();
    piSettings->log_synths = log_synths->isChecked();
    piSettings->clear_output_on_run = clear_output_on_run->isChecked();
    piSettings->log_cues = log_cues->isChecked();
    piSettings->log_auto_scroll = log_auto_scroll->isChecked();
    piSettings->gui_transparency = gui_transparency_slider->value();
    if (lightModeCheck->isChecked())        { piSettings->themeStyle = SonicPiTheme::LightMode; }
    if (darkModeCheck->isChecked())         { piSettings->themeStyle = SonicPiTheme::DarkMode; }
    if (lightProModeCheck->isChecked())     { piSettings->themeStyle = SonicPiTheme::LightProMode; }
    if (darkProModeCheck->isChecked())      { piSettings->themeStyle = SonicPiTheme::DarkProMode; }
    if (highContrastModeCheck->isChecked()) { piSettings->themeStyle = SonicPiTheme::HighContrastMode; }

    piSettings->show_scopes = show_scopes->isChecked();
    piSettings->show_scope_labels = show_scope_labels->isChecked();

    piSettings->check_updates = check_updates->isChecked();
}

void SettingsWidget::settingsChanged() {
    language_combo->setCurrentIndex(available_languages.indexOf(piSettings->language));
    QString language_detail_text = "";
    if (!i18n) {
      language_detail_text += "<b>Failed to load language translation. Using English (UK).</b>";
    }
    if (piSettings->language == "system_language") {
      language_detail_text += (
        tr("System languages: %1\n").arg(sonicPii18n->getNativeLanguageNames(sonicPii18n->getSystemLanguages()).join(", "))
        + tr("Current UI language: %1\n").arg(sonicPii18n->getNativeLanguageName(sonicPii18n->currentlyLoadedLanguage()))
      );
    }
    language_details_label->setText(language_detail_text);

    mixer_invert_stereo->setChecked(piSettings->mixer_invert_stereo);
    mixer_force_mono->setChecked(piSettings->mixer_force_mono);
    check_args->setChecked(piSettings->check_args);
    synth_trigger_timing_guarantees_cb->setChecked( piSettings->synth_trigger_timing_guarantees);
    enable_external_synths_cb->setChecked(piSettings->enable_external_synths);
    system_vol_slider->setValue(piSettings->main_volume);

    osc_server_enabled_check->setChecked(piSettings->osc_server_enabled);
    osc_public_check->setEnabled(piSettings->osc_server_enabled);
    osc_public_check->setChecked(piSettings->osc_server_enabled && piSettings->osc_public);
    midi_default_channel_combo->setCurrentIndex(piSettings->midi_default_channel);
    piSettings->midi_default_channel_str = midi_default_channel_combo->currentText(); // TODO find a more elegant solution
    midi_enable_check->setChecked(piSettings->midi_enabled);

    auto_indent_on_run->setChecked(piSettings->auto_indent_on_run);

    show_line_numbers->setChecked(piSettings->show_line_numbers);
    show_log->setChecked(piSettings->show_log);
    show_cues->setChecked(piSettings->show_cues);
    show_buttons->setChecked(piSettings->show_buttons);
    show_tabs->setChecked(piSettings->show_tabs);
    full_screen->setChecked(piSettings->full_screen);
    log_synths->setChecked(piSettings->log_synths);
    clear_output_on_run->setChecked(piSettings->clear_output_on_run);
    log_cues->setChecked(piSettings->log_cues);
    log_auto_scroll->setChecked(piSettings->log_auto_scroll);
    gui_transparency_slider->setValue(piSettings->gui_transparency);
    lightModeCheck->setChecked( piSettings->themeStyle == SonicPiTheme::LightMode );
    darkModeCheck->setChecked( piSettings->themeStyle == SonicPiTheme::DarkMode );
    lightProModeCheck->setChecked( piSettings->themeStyle == SonicPiTheme::LightProMode );
    darkProModeCheck->setChecked( piSettings->themeStyle == SonicPiTheme::DarkProMode );
    highContrastModeCheck->setChecked( piSettings->themeStyle == SonicPiTheme::HighContrastMode );

    show_scopes->setChecked(piSettings->show_scopes);
    show_scope_labels->setChecked(piSettings->show_scope_labels);

    check_updates->setChecked(piSettings->check_updates);
    show_autocompletion->setChecked(piSettings->show_autocompletion);
    show_context->setChecked(piSettings->show_context);
    updateScopeKindVisibility();
}

void SettingsWidget::connectAll() {
    //connect(language_combo, SIGNAL(currentIndexChanged(int)), this, SLOT(updateSettings()));
    connect(language_combo, SIGNAL(currentIndexChanged(int)), this, SLOT(updateUILanguage(int)));
    connect(mixer_invert_stereo, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(mixer_force_mono, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(check_args, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(synth_trigger_timing_guarantees_cb, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(enable_external_synths_cb, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(system_vol_slider, SIGNAL(valueChanged(int)), this, SLOT(updateSettings()));
    connect(mixer_invert_stereo, SIGNAL(clicked()), this, SLOT(update_mixer_invert_stereo()));
    connect(mixer_force_mono, SIGNAL(clicked()), this, SLOT(update_mixer_force_mono()));
    connect(system_vol_slider, SIGNAL(valueChanged(int)), this, SLOT(changeMainVolume(int)));

    connect(midi_default_channel_combo, SIGNAL(currentIndexChanged(int)), this, SLOT(updateSettings()));
    connect(midi_enable_check, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(osc_server_enabled_check, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(osc_public_check, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(midi_enable_check, SIGNAL(clicked()), this, SLOT(toggleMidi()));
    connect(osc_server_enabled_check, SIGNAL(clicked()), this, SLOT(toggleOscServer()));
    connect(osc_public_check, SIGNAL(clicked()), this, SLOT(toggleOscServer()));

    connect(auto_indent_on_run, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_line_numbers, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_log, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_cues, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_buttons, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_tabs, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(full_screen, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(log_synths, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(clear_output_on_run, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(log_cues, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(log_auto_scroll, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(lightModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(darkModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(lightProModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(darkProModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(highContrastModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(gui_transparency_slider, SIGNAL(valueChanged(int)), this, SLOT(updateSettings()));

    connect(show_autocompletion, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_context, SIGNAL(clicked()), this, SLOT(updateSettings()));

    connect(show_line_numbers, SIGNAL(clicked()), this, SLOT(toggleLineNumbers()));
    connect(show_log, SIGNAL(clicked()), this, SLOT(toggleLog()));
    connect(show_cues, SIGNAL(clicked()), this, SLOT(toggleCuesLog()));
    connect(show_buttons, SIGNAL(clicked()), this, SLOT(toggleButtons()));
    connect(full_screen, SIGNAL(clicked()), this, SLOT(toggleFullScreen()));
    connect(show_tabs, SIGNAL(clicked()), this, SLOT(toggleTabs()));
    connect(log_auto_scroll, SIGNAL(clicked()), this, SLOT(toggleLogAutoScroll()));
    connect(lightModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(darkModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(lightProModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(darkProModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(highContrastModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(gui_transparency_slider, SIGNAL(valueChanged(int)), this, SLOT(updateTransparency(int)));

    connect(show_scope_labels, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_scopes, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_scope_labels, SIGNAL(clicked()), this, SLOT(toggleScopeLabels()));
    connect(show_scopes, SIGNAL(clicked()), this, SLOT(toggleScope()));

    connect(check_updates, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(check_updates, SIGNAL(clicked()), this, SLOT(toggleCheckUpdates()));
    connect(visit_sonic_pi_net, SIGNAL(clicked()), this, SLOT(openSonicPiNet()));
    connect(check_updates_now, SIGNAL(clicked()), this, SLOT(checkForUpdatesNow()));

    connect(show_autocompletion, SIGNAL(clicked()), this, SLOT(showAutoCompletion()));
    connect(show_context, SIGNAL(clicked()), this, SLOT(showContext()));
    connect(check_args, SIGNAL(clicked()), this, SLOT(checkArgs()));
    connect(synth_trigger_timing_guarantees_cb, SIGNAL(clicked()), this, SLOT(synthTriggerTimingGuarantees()));
    connect(enable_external_synths_cb, SIGNAL(clicked()), this, SLOT(enableExternalSynths()));
    connect(midi_default_channel_combo, SIGNAL(currentIndexChanged(int)), this, SLOT(midiDefaultChannel()));
    connect(log_cues, SIGNAL(clicked()), this, SLOT(logCues()));
    connect(log_synths, SIGNAL(clicked()), this, SLOT(logSynths()));
    connect(clear_output_on_run, SIGNAL(clicked()), this, SLOT(clearOutputOnRun()));
    connect(auto_indent_on_run, SIGNAL(clicked()), this, SLOT(autoIndentOnRun()));
}

void SettingsWidget::add_language_combo_box_entries(QComboBox* combo) {
  // Add language combo entries
  std::cout << "[Debug] Adding language combo box entries..." << std::endl;
  std::cout << (std::to_string(static_cast<int>(available_languages.size()))) << std::endl;

  for (auto const &language : available_languages) {
    std::cout << "[Debug] Adding language " << language.toUtf8().data() << " to the combo box" << std::endl;
    if (language != "system_language") {
      // Add the language's name to the combo box
      combo->addItem(sonicPii18n->getNativeLanguageName(language));
    } else {
      combo->addItem(tr("Use system language"));
    }
  }
}
