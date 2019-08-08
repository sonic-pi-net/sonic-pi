#include "settingswidget.h"

#include <QSettings>
#include <QVBoxLayout>
#include <QGridLayout>
#include <QGroupBox>
#include <QButtonGroup>
#include <QNetworkInterface>
#include <QDesktopServices>
#include <QUrl>
#include <iostream>

/**
 * Default Constructor
 */
SettingsWidget::SettingsWidget( int port,  QWidget *parent) {
    server_osc_cues_port = port;
    std::cout << "settings created" << std::endl;
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

    if (!i18n) {
        QGroupBox *translation_box = new QGroupBox("Translation");
        QVBoxLayout *translation_box_layout = new QVBoxLayout;
        QLabel *go_translate = new QLabel;
        go_translate->setOpenExternalLinks(true);
        go_translate->setText(
                "Sonic Pi hasn't been translated to " +
                QLocale::languageToString(QLocale::system().language()) +
                " yet.<br/>" +
                "We rely on crowdsourcing to help create and maintain translations.<br/>" +
                "<a href=\"https://github.com/samaaron/sonic-pi/blob/master/TRANSLATION.md\">" +
                "Please consider helping to translate Sonic Pi to your language.</a> "
                );
        go_translate->setTextFormat(Qt::RichText);
        translation_box_layout->addWidget(go_translate);
        translation_box->setLayout(translation_box_layout);

        grid->addWidget(translation_box, 3, 0, 1, 2);
    }

    setLayout(grid);
}

/**
 * Destructor
 */
SettingsWidget::~SettingsWidget() {
}

/**
 * Create Audio Preferences Tab of Settings Widget
 */
QGroupBox* SettingsWidget::createAudioPrefsTab() {
    //TODO redundant
    QSettings settings("sonic-pi.net", "gui-settings");

    QGroupBox *volBox = new QGroupBox(tr("Master Volume"));
    volBox->setToolTip(tr("Use this slider to change the system volume."));
    QHBoxLayout *vol_box = new QHBoxLayout;
    system_vol_slider = new QSlider(this);
    int stored_vol = settings.value("prefs/system-vol", 50).toInt();
    system_vol_slider->setValue(stored_vol);
    vol_box->addWidget(system_vol_slider);
    volBox->setLayout(vol_box);

    QGroupBox *advancedAudioBox = new QGroupBox(tr("Audio Output"));
    advancedAudioBox->setToolTip(tr("Advanced audio settings for working with\nexternal PA systems when performing with Sonic Pi."));
    mixer_invert_stereo = new QCheckBox(tr("Invert stereo"));
    mixer_invert_stereo->setToolTip(tr("Toggle stereo inversion.\nIf enabled, audio sent to the left speaker will\nbe routed to the right speaker and visa versa."));
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

    enable_external_synths_cb = new QCheckBox(tr("Enable external synths and FX"));
    enable_external_synths_cb->setToolTip(tr("When enabled, Sonic Pi will allow\nsynths and FX loaded via load_synthdefs\nto be triggered.\n\nWhen disabled, Sonic Pi will complain\nwhen you attempt to use a synth or FX\nwhich isn't recognised."));

    QVBoxLayout *synths_box_layout = new QVBoxLayout;
    synths_box_layout->addWidget(check_args);
    synths_box_layout->addWidget(synth_trigger_timing_guarantees_cb);
    synths_box_layout->addWidget(enable_external_synths_cb);
    synths_box->setLayout(synths_box_layout);

    connect(mixer_invert_stereo, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(mixer_force_mono, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(check_args, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(synth_trigger_timing_guarantees_cb, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(enable_external_synths_cb, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(system_vol_slider, SIGNAL(valueChanged(int)), this, SLOT(updateSettings()));

    connect(mixer_invert_stereo, SIGNAL(clicked()), this, SLOT(update_mixer_invert_stereo()));
    connect(mixer_force_mono, SIGNAL(clicked()), this, SLOT(update_mixer_force_mono()));
    connect(system_vol_slider, SIGNAL(valueChanged(int)), this, SLOT(changeMainVolume(int)));

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

    QLabel *network_ip_label = new QLabel();
    QString ip_address_trans = tr("Local IP address");
    QString port_num_trans = tr("Listening for OSC messages on port");
    QString ip_address = "";
    QString all_ip_addresses  = "";

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

    network_ip_label->setText(ip_address_trans + ": " + ip_address + "\n" + port_num_trans + + ": " + QString::number(server_osc_cues_port));
    network_ip_label->setToolTip(all_ip_addresses);

    osc_public_check = new QCheckBox(tr("Receive remote OSC messages"));
    osc_public_check->setToolTip(tr("When checked, Sonic Pi will listen for OSC messages from remote machines.\n When unchecked, only messages from the local machine will be received."));

    osc_server_enabled_check = new QCheckBox(tr("Enable OSC server"));
    osc_server_enabled_check->setToolTip(tr("When checked, Sonic Pi will listen for OSC messages.\n When unchecked no OSC messages will be received."));

    QVBoxLayout *network_box_layout = new QVBoxLayout;
    network_box_layout->addWidget(osc_server_enabled_check);
    network_box_layout->addWidget(osc_public_check);
    network_box_layout->addWidget(network_ip_label);
    network_box->setLayout(network_box_layout);

    QGroupBox *midi_config_box = new QGroupBox(tr("MIDI Configuration"));
    midi_config_box->setToolTip(tr("Configure MIDI behaviour"));

    QGroupBox *midi_ports_box = new QGroupBox(tr("MIDI Ports"));
    midi_ports_box->setToolTip(tr("List all connected MIDI Ports"));

    midi_enable_check = new QCheckBox(tr("Enable MIDI subsystems"));
    midi_enable_check->setToolTip(tr("Enable or disable incoming and outgoing MIDI communication"));

    QPushButton *midi_reset_button = new QPushButton(tr("Reset MIDI"));
    midi_reset_button->setToolTip(tr("Reset MIDI subsystems \n(Required to detect device changes on macOS)" ));

    midi_default_channel_combo = new QComboBox();
    midi_default_channel_combo->addItem("*");
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
    midi_default_channel_combo->setSizeAdjustPolicy(QComboBox::AdjustToMinimumContentsLength) ;

    QLabel *midi_default_channel_label = new QLabel;
    midi_default_channel_label->setText(tr("Default MIDI channel (* means all)"));
    midi_default_channel_label->setToolTip(tr("Default MIDI Channel to send messages to"));

    QGridLayout *midi_default_channel_layout = new QGridLayout();

    midi_default_channel_combo->setToolTip(tr("Default MIDI Channel to send messages to"));

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
    midi_ports_box_layout->addWidget(midi_reset_button);

    midi_ports_box->setLayout(midi_ports_box_layout);
    midi_config_box->setLayout(midi_config_box_layout);
    
    connect(midi_default_channel_combo, SIGNAL(currentIndexChanged(int)), this, SLOT(updateSettings()));
    connect(midi_enable_check, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(midi_reset_button, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(osc_server_enabled_check, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(osc_public_check, SIGNAL(clicked()), this, SLOT(updateSettings()));

    connect(midi_reset_button, SIGNAL(clicked()), this, SLOT(forceMidiReset()));
    connect(midi_enable_check, SIGNAL(clicked()), this, SLOT(toggleMidi()));
    connect(osc_server_enabled_check, SIGNAL(clicked()), this, SLOT(toggleOscServer()));
    connect(osc_public_check, SIGNAL(clicked()), this, SLOT(toggleOscServer()));

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
    QGroupBox *automation_box = new QGroupBox(tr("Automation"));
    automation_box->setToolTip(tr("Configure automation features."));

    auto_indent_on_run = new QCheckBox(tr("Auto-align"));
    auto_indent_on_run->setToolTip(tr("Automatically align code on Run"));

    show_line_numbers = new QCheckBox(tr("Show line numbers"));
    show_line_numbers->setToolTip(tr("Toggle line number visibility."));
    show_log = new QCheckBox(tr("Show log"));
    show_log->setToolTip(tooltipStrShiftMeta('L', tr("Toggle visibility of the log.")));
    show_log->setChecked(true);

    show_incoming_osc_log = new QCheckBox(tr("Show cue log"));
    show_incoming_osc_log->setToolTip(tooltipStrShiftMeta('L', tr("Toggle visibility of cue log which displays internal cues & incoming OSC/MIDI messages.")));
    show_incoming_osc_log->setChecked(true);

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

    lightModeCheck->setChecked(true);

    QVBoxLayout *editor_display_box_layout = new QVBoxLayout;
    QVBoxLayout *editor_box_look_feel_layout = new QVBoxLayout;
    QVBoxLayout *automation_box_layout = new QVBoxLayout;
    QGridLayout *gridEditorPrefs = new QGridLayout;

    editor_display_box_layout->addWidget(show_line_numbers);
    editor_display_box_layout->addWidget(show_log);
    editor_display_box_layout->addWidget(show_incoming_osc_log);
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

    print_output = new QCheckBox(tr("Log synths"));
    print_output->setToolTip(tr("Toggle log messages.\nIf disabled, activity such as synth and sample\ntriggering will not be printed to the log by default."));

    clear_output_on_run = new QCheckBox(tr("Clear log on run"));
    clear_output_on_run->setToolTip(tr("Toggle log clearing on run.\nIf enabled, the log is cleared each\ntime the run button is pressed."));

    log_cues = new QCheckBox(tr("Log cues"));
    log_cues->setToolTip(tr("Enable or disable logging of cues.\nIf disabled, cues will still trigger.\nHowever, they will not be visible in the logs."));

    log_auto_scroll = new QCheckBox(tr("Auto-scroll log"));
    log_auto_scroll->setToolTip(tr("Toggle log auto scrolling.\nIf enabled the log is scrolled to the bottom after every new message is displayed."));

    QVBoxLayout *debug_box_layout = new QVBoxLayout;
    debug_box_layout->addWidget(print_output);
    debug_box_layout->addWidget(log_cues);
    debug_box_layout->addWidget(log_auto_scroll);
    debug_box_layout->addWidget(clear_output_on_run);
    debug_box->setLayout(debug_box_layout);

    connect(auto_indent_on_run, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_line_numbers, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_log, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_incoming_osc_log, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_buttons, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_tabs, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(full_screen, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(print_output, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(clear_output_on_run, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(log_cues, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(log_auto_scroll, SIGNAL(clicked()), this, SLOT(updateSettings()));

    connect(lightModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(darkModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(lightProModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(darkProModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(highContrastModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));

    connect(show_line_numbers, SIGNAL(clicked()), this, SLOT(toggleLineNumbers()));
    connect(show_log, SIGNAL(clicked()), this, SLOT(toggleLog()));
    connect(show_incoming_osc_log, SIGNAL(clicked()), this, SLOT(toggleIncommingOscLog()));
    connect(show_buttons, SIGNAL(clicked()), this, SLOT(toggleButtons()));
    connect(full_screen, SIGNAL(clicked()), this, SLOT(toggleFullScreen()));
    connect(show_tabs, SIGNAL(clicked()), this, SLOT(toggleTabs()));
    connect(log_auto_scroll, SIGNAL(clicked()), this, SLOT(toggleLogAutoScroll()));

    connect(lightModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(darkModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(lightProModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(darkProModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(highContrastModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));


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

    QVBoxLayout *scope_box_kinds_layout = new QVBoxLayout;
    QVBoxLayout *scope_box_layout = new QVBoxLayout;

    scopeSignalMap = new QSignalMapper(this);
    //TODO 
    //  for( auto name : scopeInterface->getScopeNames() )
    //  {
    //    QCheckBox* cb = new QCheckBox( tr(name.toLocal8Bit().data()) );
    //    cb->setChecked( scopeInterface->enableScope( name, isScopeEnabled(settings,name) ) );
    //    scopeSignalMap->setMapping( cb, cb );
    //    scope_box_kinds_layout->addWidget(cb);
    //    connect(cb, SIGNAL(clicked()), scopeSignalMap, SLOT(map()));
    //  }
    //connect( scopeSignalMap, SIGNAL(mapped(QWidget*)), this, SLOT(toggleScope(QWidget*)));
    show_scopes = new QCheckBox(tr("Show Scopes"));
    show_scopes->setToolTip(tr("Toggle the visibility of the audio oscilloscopes."));
    show_scope_axes = new QCheckBox(tr("Show Axes"));
    show_scope_axes->setToolTip(tr("Toggle the visibility of the axes for the audio oscilloscopes"));
    show_scope_axes->setChecked(true);
    scope_box_kinds->setLayout(scope_box_kinds_layout);
    scope_box_kinds->setToolTip(tr("The audio oscilloscope comes in three flavours which may\nbe viewed independently or all together:\n\nLissajous - illustrates the phase relationship between the left and right channels\nMono - shows a combined view of the left and right channels (using RMS)\nStereo - shows two independent scopes for left and right channels"));
    scope_box_layout->addWidget(show_scopes);
    scope_box_layout->addWidget(show_scope_axes);
    scope_box->setLayout(scope_box_layout);
    viz_tab_layout->addWidget(scope_box, 0, 0);
    viz_tab_layout->addWidget(scope_box_kinds, 1, 0);

    QGroupBox *transparency_box = new QGroupBox(tr("Transparency"));
    QGridLayout *transparency_box_layout = new QGridLayout;
    gui_transparency_slider = new QSlider(this);
    //connect(gui_transparency_slider, SIGNAL(valueChanged(int)), this, SLOT(changeGUITransparency(int)));
    transparency_box_layout->addWidget(gui_transparency_slider);
    transparency_box->setLayout(transparency_box_layout);

#if defined(Q_OS_LINUX)
    // do nothing
#else
    viz_tab_layout->addWidget(transparency_box, 0, 1, 0, 1);
#endif
    //connect(show_scope_axes, SIGNAL(clicked()), this, SLOT(toggleScopeAxes()));
    //connect(show_scopes, SIGNAL(clicked()), this, SLOT(scope()));
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

    connect(check_updates, SIGNAL(clicked()), this, SLOT(updateSettings()));

    connect(check_updates, SIGNAL(clicked()), this, SLOT(toggleCheckUpdates()));
    connect(visit_sonic_pi_net, SIGNAL(clicked()), this, SLOT(openSonicPiNet()));
    connect(check_updates_now, SIGNAL(clicked()), this, SLOT(checkForUpdatesNow()));

    QGroupBox *update_prefs_box = new QGroupBox();
    QGridLayout *update_prefs_box_layout = new QGridLayout;
    update_prefs_box_layout->addWidget(update_info_box, 0, 0);
    update_prefs_box_layout->addWidget(update_box, 0, 1);
    update_prefs_box->setLayout(update_prefs_box_layout);
    return update_prefs_box;
}


// TODO utils?
QString SettingsWidget::tooltipStrShiftMeta(char key, QString str) {
#ifdef Q_OS_MAC
    return QString("%1 (⇧⌘%2)").arg(str).arg(key);
#else
    return QString("%1 (Shift-alt-%2)").arg(str).arg(key);
#endif
}



void SettingsWidget::update_mixer_invert_stereo() {
    std::cout << "INVERT STEREO" << std::endl;
    emit mixerSettingsChanged();
}

void SettingsWidget::update_mixer_force_mono() {
    std::cout << "FORCE MONO" << std::endl;
    emit mixerSettingsChanged();
}

void SettingsWidget::toggleOscServer() {
    std::cout << "TOGGLE OSC" << std::endl;
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

void SettingsWidget::toggleLog() {
    emit showLogChanged();
}

void SettingsWidget::toggleIncommingOscLog() {
    emit incomingOscLogChanged();
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

void SettingsWidget::toggleCheckUpdates() {
    emit checkUpdatesChanged();
}

void SettingsWidget::checkForUpdatesNow() {
    emit forceCheckUpdates();
}

void SettingsWidget::openSonicPiNet() {
  QDesktopServices::openUrl(QUrl("http://sonic-pi.net", QUrl::TolerantMode));
}

void SettingsWidget::updateVersionInfo( QString info_string, QString visit, bool sonic_pi_net_visible, bool check_now_visible) {
    update_info->setText( info_string );
    visit_sonic_pi_net->setText( visit );
    visit_sonic_pi_net->setVisible(sonic_pi_net_visible);
    check_updates_now->setVisible(check_now_visible);
}

void SettingsWidget::updateSettings() {
    std::cout << "Update Settings" << std::endl;
    settings.mixer_invert_stereo = mixer_invert_stereo->isChecked();
    settings.mixer_force_mono = mixer_force_mono->isChecked();
    settings.check_args = check_args->isChecked();
    settings.synth_trigger_timing_guarantees = synth_trigger_timing_guarantees_cb->isChecked();
    settings.enable_external_synths = enable_external_synths_cb->isChecked();
    settings.main_volume = system_vol_slider->value();

    settings.osc_server_enabled = osc_server_enabled_check->isChecked();
    settings.osc_public = osc_public_check->isChecked();
    settings.midi_default_channel = midi_default_channel_combo->currentIndex();
    settings.midi_default_channel_str = midi_default_channel_combo->currentText(); // TODO find a more elegant solution
    settings.midi_enabled = midi_enable_check->isChecked();

    settings.auto_indent_on_run = auto_indent_on_run->isChecked();
    settings.show_line_numbers = show_line_numbers->isChecked();
    settings.show_log = show_log->isChecked();
    settings.show_incoming_osc_log = show_incoming_osc_log->isChecked();
    settings.show_buttons = show_buttons->isChecked();
    settings.show_tabs = show_tabs->isChecked();
    settings.full_screen = full_screen->isChecked();
    settings.print_output = print_output->isChecked();
    settings.clear_output_on_run = clear_output_on_run->isChecked();
    settings.log_cues = log_cues->isChecked();
    settings.log_auto_scroll = log_auto_scroll->isChecked();
    if (lightModeCheck->isChecked())        { settings.theme = SonicPiSettings::LightMode; }
    if (darkModeCheck->isChecked())         { settings.theme = SonicPiSettings::DarkMode; }
    if (lightProModeCheck->isChecked())     { settings.theme = SonicPiSettings::LightProMode; }
    if (darkProModeCheck->isChecked())      { settings.theme = SonicPiSettings::DarkProMode; }
    if (highContrastModeCheck->isChecked()) { settings.theme = SonicPiSettings::HighContrastMode; }

    settings.check_updates = check_updates->isChecked();
}
