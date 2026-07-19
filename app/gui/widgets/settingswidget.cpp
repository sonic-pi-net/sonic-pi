#include "settingswidget.h"
#include "devicelistwidget.h"
#include "mainwindow.h"
#include "utils/reducedmotion.h"
#include "utils/sonicpi_i18n.h"
#include "dpi.h"
#include <api/audio/audio_driver_select.hpp>
#include <QTreeWidget>
#include <QHeaderView>
#include <QRadioButton>
#include <QFileDialog>
#include <QColorDialog>
#include <QListWidget>
#include <QDial>
#include "arcdial.h"
#include "theme_card.h"
#include <QDialog>
#include <QStyledItemDelegate>
#include <QLineEdit>
#include <QKeyEvent>
#include <QMouseEvent>
#include <QFocusEvent>
#include <QStyle>
#include <QScopedValueRollback>
#include <memory>
#if defined(Q_OS_DARWIN)
#include "platform/macos.h"
#endif
#ifdef Q_OS_WIN
#define WIN32_LEAN_AND_MEAN
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>
#endif

#include <QSettings>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QGridLayout>
#include <QGroupBox>
#include <QButtonGroup>
#include <QToolButton>
#include <QNetworkInterface>
#include <QDesktopServices>
#include <QCheckBox>
#include <QComboBox>
#include <QRadioButton>
#include <QDial>
#include <QTimer>
#include <QPainter>
#include <QPixmap>
#include <QIcon>
#include <QPen>
#include <QFontMetrics>
#include <QUrl>
#include <iostream>
#include <QLabel>
#include <QPushButton>
#include <QSignalMapper>
#include <QVBoxLayout>
#include <QMessageBox>
#include <QProcess>
#include <QFileInfo>
#include <QCoreApplication>
#include <QSize>
#include <QSvgRenderer>
#include <QApplication>

#include "arcdial.h"

namespace {

// Recording-selector glyphs (Tabler icons, MIT). %1 = the render colour.
// Audio Only = a waveform; Audio + Video = a camcorder.
const char* kWaveformSvg =
    "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' "
    "stroke='%1' stroke-width='2.75' stroke-linecap='round' stroke-linejoin='round'>"
    "<path d='M3 9v6'/>"
    "<path d='M7 5v14'/>"
    "<path d='M11 3v18'/>"
    "<path d='M15 6v12'/>"
    "<path d='M19 9v6'/>"
    "</svg>";

const char* kVideoSvg =
    "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' "
    "stroke='%1' stroke-width='2.75' stroke-linecap='round' stroke-linejoin='round'>"
    "<path d='M15 10l4.553 -2.276a1 1 0 0 1 1.447 .894v6.764a1 1 0 0 1 -1.447 .894l-4.553 -2.276v-4z'/>"
    "<path d='M3 8a2 2 0 0 1 2 -2h8a2 2 0 0 1 2 2v8a2 2 0 0 1 -2 2h-8a2 2 0 0 1 -2 -2z'/>"
    "</svg>";

// Render a tinted SVG glyph to a crisp (2x) pixmap of the given logical size.
QPixmap makeSvgPixmap(const char* svg, const QColor& color, int px)
{
    QPixmap pm(QSize(px, px) * 2);
    pm.setDevicePixelRatio(2);
    pm.fill(Qt::transparent);
    QPainter p(&pm);
    p.setRenderHint(QPainter::Antialiasing);
    const QByteArray bytes =
        QString::fromLatin1(svg).arg(color.name(QColor::HexRgb)).toUtf8();
    QSvgRenderer(bytes).render(&p, QRectF(0, 0, px, px));
    return pm;
}

// Two-state icon: `off` tint at rest, `on` tint while checked — Qt picks
// the QIcon::On pixmap automatically for checked buttons, so the glyph
// reads on both the grey segment and the highlight-filled one.
QIcon makeSvgToggleIcon(const char* svg, const QColor& off, const QColor& on, int px)
{
    QIcon icon;
    icon.addPixmap(makeSvgPixmap(svg, off, px), QIcon::Normal, QIcon::Off);
    icon.addPixmap(makeSvgPixmap(svg, on, px), QIcon::Normal, QIcon::On);
    icon.addPixmap(makeSvgPixmap(svg, off, px), QIcon::Active, QIcon::Off);
    icon.addPixmap(makeSvgPixmap(svg, on, px), QIcon::Active, QIcon::On);
    return icon;
}

} // namespace

/**
 * Default Constructor
 */
SettingsWidget::SettingsWidget(int tau_osc_cues_port, bool i18n, SonicPiSettings *piSettings, SonicPii18n *sonicPii18n, const QString& shortcutConfigPath, QWidget *parent) {
    this->piSettings = piSettings;
    this->i18n = i18n;
    this->sonicPii18n = sonicPii18n;
    this->shortcutConfigPath = shortcutConfigPath;
    this->available_languages = sonicPii18n->getAvailableLanguages();
    this->tau_osc_cues_port = tau_osc_cues_port;

    // Safety timeout: if device switch takes longer than 15 seconds,
    // re-enable controls so the user isn't stuck forever.
    m_switchTimeoutTimer = new QTimer(this);
    m_switchTimeoutTimer->setSingleShot(true);
    connect(m_switchTimeoutTimer, &QTimer::timeout, this, [this]() {
        supersonic_version_label->setText(tr("Device switch timed out"));
        audio_output_combo->setEnabled(true);
        audio_input_combo->setEnabled(true);
        audio_sample_rate_combo->setEnabled(true);
        audio_buffer_size_combo->setEnabled(true);
        audio_driver_combo->setEnabled(true);
    });
    QSizePolicy prefsSizePolicy(QSizePolicy::Preferred, QSizePolicy::MinimumExpanding);

    setSizePolicy(prefsSizePolicy) ;
    prefTabs = new QTabWidget();

    QGridLayout *grid = new QGridLayout;
    grid->addWidget(prefTabs, 0, 0);

    QGroupBox *audio_prefs_box = createAudioPrefsTab();
    prefTabs->setTabToolTip(prefTabs->addTab(audio_prefs_box, tr("Audio")),
                            tr("Volume, audio inputs and outputs, safety checks and recording."));

    QGroupBox *ioTab = createIoPrefsTab();
    prefTabs->setTabToolTip(prefTabs->addTab(ioTab, tr("IO")),
                            tr("OSC networking, MIDI devices and game controllers."));

    QGroupBox *editorTab = createEditorPrefsTab();
    prefTabs->setTabToolTip(prefTabs->addTab(editorTab, tr("Editor")),
                            tr("Editor display, code completion, accessibility and pane visibility."));

    QGroupBox *visualizationTab = createVisualizationPrefsTab();
    prefTabs->setTabToolTip(prefTabs->addTab(visualizationTab, tr("Visuals")),
                            tr("Audio oscilloscopes and options useful when performing."));

    QGroupBox *shortcuts_prefs_box = createKeyboardShortcutsTab();
    prefTabs->setTabToolTip(prefTabs->addTab(shortcuts_prefs_box, tr("Shortcuts")),
                            tr("View and customise the keyboard shortcuts."));

    QGroupBox *language_prefs_box = createLanguagePrefsTab();
    prefTabs->setTabToolTip(prefTabs->addTab(language_prefs_box, tr("Language")),
                            tr("Change the language of the interface and tutorial."));

    // Updates last — it's the least-visited tab.
    QGroupBox *update_prefs_box = createUpdatePrefsTab();
    prefTabs->setTabToolTip(prefTabs->addTab(update_prefs_box, tr("Updates")),
                            tr("Version information and update checking."));


    settingsChanged();
    connectAll();
    setLayout(grid);
}

/**
 * Destructor
 */
SettingsWidget::~SettingsWidget() {
}

// True when the GUI is running inside a remote desktop session (RDP).
// Local audio hardware is typically unavailable there — WASAPI endpoints
// are redirected to "Remote Audio", while installed ASIO drivers still
// enumerate from the registry regardless of session and then fail to
// start. Checked per call rather than cached: RDP attach/detach changes
// the session state mid-run, and each attach also changes the audio
// device list, which triggers the device report that re-reads this.
static bool isRemoteDesktopSession()
{
#ifdef Q_OS_WIN
    return GetSystemMetrics(SM_REMOTESESSION) != 0;
#else
    return false;
#endif
}

/**
 * Create Audio Preferences Tab of Settings Widget
 */
QGroupBox* SettingsWidget::createAudioPrefsTab() {

    // --- Main Volume + Audio settings ---
    QGroupBox *volBox = new QGroupBox(tr("Main Volume"));
    volBox->setToolTip(tr("Use this dial to change the system volume."));
    system_vol_slider = new ArcDial(this);
    system_vol_slider->setWrapping(false);
    system_vol_slider->setValueFontPt(20);
    system_vol_slider->setMinimumSize(100, 100);
    system_vol_slider->setAccessibleName(tr("Main Volume"));
    system_vol_slider->setProperty("tipTitle", tr("Main Volume"));
    system_vol_slider->setToolTip(tr("Drag or scroll to change Sonic Pi's overall volume."));

    enable_scsynth_inputs = new QCheckBox(tr("Enable Audio Inputs"));
    enable_scsynth_inputs->setToolTip(tr("Toggle to enable or disable audio inputs."));
    asio_input_note = new QLabel(
        tr("ASIO uses one device for both input and output."));
    asio_input_note->setWordWrap(true);
    asio_input_note->setObjectName("asioInputNote");   // styled by app.qss (muted note)
    asio_input_note->setVisible(false);
    mixer_invert_stereo = new QCheckBox(tr("Invert stereo"));
    mixer_invert_stereo->setToolTip(tr("If enabled, audio sent to the left speaker will be routed to the right speaker and vice versa."));
    mixer_force_mono = new QCheckBox(tr("Force mono"));
    mixer_force_mono->setToolTip(tr("If enabled, right and left audio is mixed and the same signal is sent to both speakers. Useful when working with external systems that can only handle mono."));

    check_args = new QCheckBox(tr("Safe mode"));
    check_args->setToolTip(tr("Checks synth arguments before triggering. If disabled, certain synth opt values may create unexpectedly loud or uncomfortable sounds."));

    synth_trigger_timing_guarantees_cb = new QCheckBox(tr("Enforce timing guarantees"));
    synth_trigger_timing_guarantees_cb->setToolTip(tr("When enabled, Sonic Pi will refuse to trigger synths and FX if it is too late to do so.\n\nWhen disabled, Sonic Pi will always attempt to trigger synths and FX even when a little late."));

    enable_external_synths_cb = new QCheckBox(tr("Enable external synths/FX"));
    enable_external_synths_cb->setToolTip(tr("When enabled, Sonic Pi will allow synths and FX loaded via load_synthdefs to be triggered.\n\nWhen disabled, Sonic Pi will complain when you attempt to use a synth or FX which isn't recognised."));

    QGroupBox *audioGroup = new QGroupBox(tr("Audio"));
    QVBoxLayout *audioGroupLayout = new QVBoxLayout;
    audioGroupLayout->addWidget(enable_scsynth_inputs);
    audioGroupLayout->addWidget(asio_input_note);
    audioGroupLayout->addWidget(mixer_invert_stereo);
    audioGroupLayout->addWidget(mixer_force_mono);
    audioGroup->setLayout(audioGroupLayout);

    QGroupBox *synthsGroup = new QGroupBox(tr("Synths and FX"));
    QVBoxLayout *synthsGroupLayout = new QVBoxLayout;
    synthsGroupLayout->addWidget(check_args);
    synthsGroupLayout->addWidget(synth_trigger_timing_guarantees_cb);
    synthsGroupLayout->addWidget(enable_external_synths_cb);
    synthsGroup->setLayout(synthsGroupLayout);

    QVBoxLayout *vol_box = new QVBoxLayout;
    vol_box->addWidget(system_vol_slider, 1, Qt::AlignHCenter);
    vol_box->addWidget(audioGroup);
    vol_box->addWidget(synthsGroup);
    volBox->setLayout(vol_box);

    // --- Audio Device (driver, device, sample rate, buffer size) ---
    QGroupBox *audioDeviceBox = new QGroupBox(tr("Audio Device"));
    audioDeviceBox->setToolTip(tr("Configure audio driver, device, sample rate and buffer size."));
    QGridLayout *audio_device_layout = new QGridLayout;
    // Gap between rows so the combos read as separate fields, not one block.
    audio_device_layout->setVerticalSpacing(ScaleHeightForDPI(8));

    QLabel *driverLabel = new QLabel(tr("Driver"));
    audio_driver_combo = new QComboBox();
    audio_driver_combo->setMinimumContentsLength(12);
    audio_driver_combo->setSizeAdjustPolicy(QComboBox::AdjustToMinimumContentsLengthWithIcon);
    driverLabel->setBuddy(audio_driver_combo);
    audio_device_layout->addWidget(driverLabel, 0, 0);
    audio_device_layout->addWidget(audio_driver_combo, 0, 1);

    QLabel *outputLabel = new QLabel(tr("Output"));
    audio_output_combo = new QComboBox();
    audio_output_combo->setMinimumContentsLength(20);
    audio_output_combo->setSizeAdjustPolicy(QComboBox::AdjustToMinimumContentsLengthWithIcon);
    outputLabel->setBuddy(audio_output_combo);
    audio_device_layout->addWidget(outputLabel, 1, 0);
    audio_device_layout->addWidget(audio_output_combo, 1, 1);

    QLabel *inputLabel = new QLabel(tr("Input"));
    audio_input_combo = new QComboBox();
    audio_input_combo->setMinimumContentsLength(20);
    audio_input_combo->setSizeAdjustPolicy(QComboBox::AdjustToMinimumContentsLengthWithIcon);
    inputLabel->setBuddy(audio_input_combo);
    audio_device_layout->addWidget(inputLabel, 2, 0);
    audio_device_layout->addWidget(audio_input_combo, 2, 1);

    QLabel *srLabel = new QLabel(tr("Sample Rate"));
    audio_sample_rate_combo = new QComboBox();
    audio_sample_rate_combo->setMinimumContentsLength(8);
    audio_sample_rate_combo->setSizeAdjustPolicy(QComboBox::AdjustToMinimumContentsLengthWithIcon);
    srLabel->setBuddy(audio_sample_rate_combo);
    audio_device_layout->addWidget(srLabel, 3, 0);
    audio_device_layout->addWidget(audio_sample_rate_combo, 3, 1);

    QLabel *bsLabel = new QLabel(tr("Buffer Size"));
    audio_buffer_size_combo = new QComboBox();
    audio_buffer_size_combo->setMinimumContentsLength(8);
    audio_buffer_size_combo->setSizeAdjustPolicy(QComboBox::AdjustToMinimumContentsLengthWithIcon);
    bsLabel->setBuddy(audio_buffer_size_combo);
    audio_device_layout->addWidget(bsLabel, 4, 0);
    audio_device_layout->addWidget(audio_buffer_size_combo, 4, 1);

    remote_session_note = new QLabel(
        tr("Remote desktop session: local audio hardware is usually "
           "unavailable, and ASIO devices may fail to start."));
    remote_session_note->setWordWrap(true);
    remote_session_note->setObjectName("remoteSessionNote");   // styled by app.qss (muted note)
    remote_session_note->setVisible(isRemoteDesktopSession());
    audio_device_layout->addWidget(remote_session_note, 5, 0, 1, 2);

    // Fixed, uniform height so each combo's grey fill exactly matches its
    // focus/hover highlight (otherwise the widget floats taller than the
    // painted background) and every row is the same height (even spacing).
    const int comboHeight = ScaleHeightForDPI(28);
    for (QComboBox* c : { audio_driver_combo, audio_output_combo, audio_input_combo,
                          audio_sample_rate_combo, audio_buffer_size_combo }) {
        c->setFixedHeight(comboHeight);
    }

    audioDeviceBox->setLayout(audio_device_layout);

    // activated(int) — user-interaction only. currentIndexChanged fires
    // on programmatic setCurrentIndex() too, which would emit spurious
    // switches during updateAudioDevices() populate
    connect(audio_driver_combo, SIGNAL(activated(int)), this, SLOT(audioDriverChanged(int)));
    connect(audio_output_combo, SIGNAL(activated(int)), this, SLOT(audioDeviceChanged(int)));
    connect(audio_input_combo, SIGNAL(activated(int)), this, SLOT(audioInputDeviceChanged(int)));
    connect(audio_sample_rate_combo, SIGNAL(activated(int)), this, SLOT(audioSampleRateChanged(int)));
    connect(audio_buffer_size_combo, SIGNAL(activated(int)), this, SLOT(audioBufferSizeChanged(int)));

#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    // --- Recording mode — same setting is reachable from the IO menubar
    // submenu and the rec-button right-click menu. Segmented pill toggle
    // in the same style as the Shortcuts tab's mode control. ---
    QGroupBox *recordingGroup = new QGroupBox(tr("Recording"));
    recordingGroup->setToolTip(tr("Choose what the rec button captures."));

    // Icons tinted with the themed foreground so they read on both the resting
    // grey segment and the highlighted (checked) segment. QToolButton with
    // TextUnderIcon stacks a large glyph above the label — a QPushButton can
    // only put a small icon beside the text.
    const QColor segIconColor = QApplication::palette().color(QPalette::WindowText);
    const QColor segIconOnColor = QApplication::palette().color(QPalette::HighlightedText);
    const int segIconPx = ScaleHeightForDPI(32);

    recording_type_audio_radio = new QToolButton();
    recording_type_audio_radio->setText(tr("Record Audio Only"));
    recording_type_audio_radio->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    recording_type_audio_radio->setIcon(makeSvgToggleIcon(kWaveformSvg, segIconColor, segIconOnColor, segIconPx));
    recording_type_audio_radio->setIconSize(QSize(segIconPx, segIconPx));
    recording_type_audio_radio->setToolTip(tr(
        "SuperSonic writes a .wav of the master mix"));

    recording_type_av_radio = new QToolButton();
    recording_type_av_radio->setText(tr("Record Audio + Video"));
    recording_type_av_radio->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    recording_type_av_radio->setIcon(makeSvgToggleIcon(kVideoSvg, segIconColor, segIconOnColor, segIconPx));
    recording_type_av_radio->setIconSize(QSize(segIconPx, segIconPx));
#if defined(Q_OS_MAC)
    recording_type_av_radio->setToolTip(tr(
        "Captures the Sonic Pi window plus master mix into a .mov\n"
        "using GPU-accelerated screen capture"));
#else
    recording_type_av_radio->setToolTip(tr(
        "Captures the Sonic Pi window plus master mix into an .mp4\n"
        "using GPU-accelerated screen capture"));
#endif

    QWidget* recSegControl = new QWidget();
    recSegControl->setObjectName("recSegControl");
    // Styled by the shared "segmented control" rule in app.qss (house metrics).
    recSegControl->setProperty("segmented", true);
    QHBoxLayout* recSegLayout = new QHBoxLayout(recSegControl);
    recSegLayout->setContentsMargins(3, 3, 3, 3);
    recSegLayout->setSpacing(3);

    // Button IDs are the enum values so the idClicked(int) signal
    // delivers the chosen mode directly. idClicked only fires on user
    // clicks, so programmatic setChecked from settingsChanged() doesn't
    // echo back.
    recording_type_group = new QButtonGroup(this);
    recording_type_group->setExclusive(true);
    for (QToolButton* b : { recording_type_audio_radio, recording_type_av_radio }) {
        b->setCheckable(true);
        b->setCursor(Qt::PointingHandCursor);
        recSegLayout->addWidget(b);
    }
    recording_type_group->addButton(recording_type_audio_radio,
        static_cast<int>(SonicPiSettings::Audio));
    recording_type_group->addButton(recording_type_av_radio,
        static_cast<int>(SonicPiSettings::AudioAndVideo));

    QHBoxLayout *recordingGroupLayout = new QHBoxLayout;
    recordingGroupLayout->addStretch(1);
    recordingGroupLayout->addWidget(recSegControl);
    recordingGroupLayout->addStretch(1);
    recordingGroup->setLayout(recordingGroupLayout);

    connect(recording_type_group, SIGNAL(idClicked(int)),
            this, SLOT(recordingTypeChanged(int)));
#endif

    // --- SuperSonic info panel (ASCII art + version, tooltip = detailed info) ---
    supersonicBox = new QGroupBox();
    QLabel *powered_by_label = new QLabel(tr("Powered by"));
    powered_by_label->setAlignment(Qt::AlignCenter);
    powered_by_label->setObjectName("poweredByLabel");   // styled by app.qss (muted note)
    supersonic_ascii_label = new QLabel(
        QString::fromUtf8(
            "\u2591\u2588\u2580\u2580\u2591\u2588\u2591\u2588\u2591\u2588\u2580\u2588\u2591\u2588\u2580\u2580\u2591\u2588\u2580\u2584\u2591\u2588\u2580\u2580\u2591\u2588\u2580\u2588\u2591\u2588\u2580\u2588\u2591\u2580\u2588\u2580\u2591\u2588\u2580\u2580\n"
            "\u2591\u2580\u2580\u2588\u2591\u2588\u2591\u2588\u2591\u2588\u2580\u2580\u2591\u2588\u2580\u2580\u2591\u2588\u2580\u2584\u2591\u2580\u2580\u2588\u2591\u2588\u2591\u2588\u2591\u2588\u2591\u2588\u2591\u2591\u2588\u2591\u2591\u2588\u2591\u2591\n"
            "\u2591\u2580\u2580\u2580\u2591\u2580\u2580\u2580\u2591\u2580\u2591\u2591\u2591\u2580\u2580\u2580\u2591\u2580\u2591\u2580\u2591\u2580\u2580\u2580\u2591\u2580\u2580\u2580\u2591\u2580\u2591\u2580\u2591\u2580\u2580\u2580\u2591\u2580\u2580\u2580"
        )
    );
    supersonic_ascii_label->setFont(QFont("Hack", 7));
    supersonic_ascii_label->setAlignment(Qt::AlignCenter);

    supersonic_version_label = new QLabel(tr("Waiting for SuperSonic..."));
    supersonic_version_label->setAlignment(Qt::AlignCenter);

    // Mic permission status line (macOS only — hidden on other platforms).
    // Polled from a QTimer so the user sees it flip to authorized as soon
    // as they grant access in System Settings, without needing to restart.
    mic_permission_label = new QLabel(tr(""));
    mic_permission_label->setAlignment(Qt::AlignCenter);
    mic_permission_label->setWordWrap(true);
    mic_permission_label->setVisible(false);
    mic_permission_settings_button = new QPushButton(tr("Open System Settings"));
    mic_permission_settings_button->setVisible(false);
    connect(mic_permission_settings_button, &QPushButton::clicked, this, []() {
#if defined(Q_OS_DARWIN)
        SonicPi::openSystemMicrophonePane();
#endif
    });

    QVBoxLayout *supersonic_layout = new QVBoxLayout;
    supersonic_layout->addStretch();
    supersonic_layout->addWidget(powered_by_label);
    supersonic_layout->addSpacing(ScaleHeightForDPI(8));
    supersonic_layout->addWidget(supersonic_ascii_label);
    supersonic_layout->addWidget(supersonic_version_label);
    supersonic_layout->addWidget(mic_permission_label);
    supersonic_layout->addWidget(mic_permission_settings_button,
                                 0, Qt::AlignCenter);
    supersonic_layout->addStretch();
    supersonicBox->setLayout(supersonic_layout);

#if defined(Q_OS_DARWIN)
    // Poll mic permission every 2 s. Cheap — a single AVFoundation call.
    m_micPermissionTimer = new QTimer(this);
    m_micPermissionTimer->setInterval(2000);
    connect(m_micPermissionTimer, &QTimer::timeout, this,
            &SettingsWidget::updateMicPermissionStatus);
    m_micPermissionTimer->start();
    // Also poll immediately so the initial state is correct.
    QTimer::singleShot(100, this, &SettingsWidget::updateMicPermissionStatus);
#endif

    // --- Assemble grid layout ---
    // Col 0: Volume knob + all checkboxes (spans all rows)
    // Col 1: Audio Device, Recording (mac/win), SuperSonic panel
    QGroupBox *audio_prefs_box = new QGroupBox();
    QGridLayout *audio_prefs_box_layout = new QGridLayout;

    // Audio Device and Recording keep their natural height; the SuperSonic
    // panel (centred art with top/bottom stretches) absorbs any slack so it
    // doesn't squeeze the device combos.
    audioDeviceBox->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    recordingGroup->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
#endif
    supersonicBox->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Expanding);

    audio_prefs_box_layout->addWidget(volBox, 0, 0, 3, 1);
    audio_prefs_box_layout->addWidget(audioDeviceBox, 0, 1);
#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    audio_prefs_box_layout->addWidget(recordingGroup, 1, 1);
#endif
    audio_prefs_box_layout->addWidget(supersonicBox, 2, 1);
    audio_prefs_box_layout->setRowStretch(0, 0);
    audio_prefs_box_layout->setRowStretch(1, 0);
    audio_prefs_box_layout->setRowStretch(2, 1);
    audio_prefs_box->setLayout(audio_prefs_box_layout);
    return audio_prefs_box;
}

/**
 * create io tab of settings widget
 */
QGroupBox* SettingsWidget::createIoPrefsTab() {
    QGroupBox *ioTab = new QGroupBox();

    QGroupBox *network_box = new QGroupBox(tr("Networked OSC"));
    network_box->setToolTip(tr("Sonic Pi can send and receive Open Sound Control messages to and from other programs or computers via the currently connected network."));

    QLabel *network_ip_label = new QLabel();
    QString ip_address_trans = tr("Local IP address");
    QString port_num_trans = tr("Incoming OSC port");
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

    network_ip_label->setText(ip_address_trans + ": " + ip_address + "\n" + port_num_trans + + ": " + QString::number(tau_osc_cues_port));
    network_ip_label->setToolTip(all_ip_addresses);

    osc_public_check = new QCheckBox(tr("Allow OSC from other computers"));
    osc_public_check->setToolTip(tr("When checked, Sonic Pi will let you send and receive OSC messages to and from remote machines. When unchecked, only sending and receiving from the local machine will be enabled."));

    osc_server_enabled_check = new QCheckBox(tr("Allow incoming OSC"));
    osc_server_enabled_check->setToolTip(tr("When checked, Sonic Pi will listen for OSC messages. When unchecked, no OSC messages will be received."));

    QVBoxLayout *network_box_layout = new QVBoxLayout;
    network_box_layout->addWidget(osc_server_enabled_check);
    network_box_layout->addWidget(osc_public_check);
    network_box_layout->addWidget(network_ip_label);
    network_box->setLayout(network_box_layout);

    QGroupBox *midi_config_box = new QGroupBox(tr("MIDI Configuration"));
    midi_config_box->setToolTip(tr("Configure MIDI behaviour"));

    QGroupBox *midi_ports_box = new QGroupBox(tr("MIDI Ports"));
    midi_ports_box->setToolTip(tr("List all connected MIDI Ports"));

    midi_enable_check = new QCheckBox(tr("Enable incoming MIDI cues"));
    midi_enable_check->setToolTip(tr("Enable or disable automatic conversion of incoming MIDI messages to cue events"));

    midi_default_channel_combo = new QComboBox();
    midi_default_channel_combo->addItem("* (" + tr("all") + ")");
    for (int ch = 1; ch <= 16; ++ch) {
        midi_default_channel_combo->addItem(QString::number(ch));
    }
    midi_default_channel_combo->setMaxVisibleItems(17);
    midi_default_channel_combo->setMinimumContentsLength(2);
    midi_default_channel_combo->setSizeAdjustPolicy(QComboBox::AdjustToMinimumContentsLengthWithIcon) ;

    QLabel *midi_default_channel_label = new QLabel;
    midi_default_channel_label->setText(tr("Default MIDI out channel"));
    midi_default_channel_label->setToolTip(tr("Default MIDI Channel to send messages to (* means all)"));

    QGridLayout *midi_default_channel_layout = new QGridLayout();

    midi_default_channel_combo->setToolTip(tr("Default MIDI Channel to send messages to  (* means all)"));

    midi_default_channel_layout->addWidget(midi_default_channel_combo, 0, 0);
    midi_default_channel_layout->addWidget(midi_default_channel_label, 0, 1);

    midi_in_ports_list = new DeviceListWidget(tr("No connected input devices"));
    midi_out_ports_list = new DeviceListWidget(tr("No connected output devices"));
    midi_in_ports_list->setObjectName("midi-in-ports-list");
    midi_in_ports_list->setAccessibleName(tr("MIDI input ports"));
    midi_out_ports_list->setObjectName("midi-out-ports-list");
    midi_out_ports_list->setAccessibleName(tr("MIDI output ports"));
    midi_in_ports_list->setToolTip(tr("MIDI input devices send MIDI messages directly to Sonic Pi and are received as cue events (similar to incoming OSC messages and internal cues)."));
    midi_out_ports_list->setToolTip(tr("MIDI output devices receive MIDI messages directly from Sonic Pi which can be sent via the midi_* fns."));

    QLabel *midi_in_header = new QLabel(tr("Inputs"));
    QLabel *midi_out_header = new QLabel(tr("Outputs"));
    midi_in_header->setStyleSheet("font-weight: bold;");
    midi_out_header->setStyleSheet("font-weight: bold;");

    connect(midi_in_ports_list, &DeviceListWidget::deviceToggled, this,
            [this](const QString& name, bool enabled) { emit midiPortEnabledChanged("in", name, enabled); });
    connect(midi_out_ports_list, &DeviceListWidget::deviceToggled, this,
            [this](const QString& name, bool enabled) { emit midiPortEnabledChanged("out", name, enabled); });

    QVBoxLayout *midi_ports_box_layout = new QVBoxLayout;
    QVBoxLayout *midi_config_box_layout = new QVBoxLayout;
    midi_config_box_layout->addWidget(midi_enable_check);
    midi_config_box_layout->addLayout(midi_default_channel_layout);

    midi_ports_box_layout->addWidget(midi_in_header);
    midi_ports_box_layout->addWidget(midi_in_ports_list);
    midi_ports_box_layout->addSpacing(8);
    midi_ports_box_layout->addWidget(midi_out_header);
    midi_ports_box_layout->addWidget(midi_out_ports_list);
    midi_ports_box_layout->addStretch(1);

    midi_ports_box->setLayout(midi_ports_box_layout);
    midi_config_box->setLayout(midi_config_box_layout);

    QGroupBox *gamepad_box = new QGroupBox(tr("Game Controllers"));
    gamepad_box->setToolTip(tr("Configure game controller behaviour"));

    gamepad_enable_check = new QCheckBox(tr("Enable incoming gamepad cues"));
    gamepad_enable_check->setToolTip(tr("Enable or disable automatic conversion of game controller button and axis events to cue events."));

    gamepad_devices_list = new DeviceListWidget(tr("No connected game controllers"));
    gamepad_devices_list->setObjectName("gamepad-devices-list");
    gamepad_devices_list->setAccessibleName(tr("Game controllers"));
    gamepad_devices_list->setToolTip(tr("Connected game controllers send button and axis events to Sonic Pi which are received as cue events."));

    connect(gamepad_devices_list, &DeviceListWidget::deviceToggled, this,
            [this](const QString& name, bool enabled) { emit gamepadDeviceEnabledChanged(name, enabled); });

    QVBoxLayout *gamepad_box_layout = new QVBoxLayout;
    gamepad_box_layout->addWidget(gamepad_enable_check);
    gamepad_box_layout->addWidget(gamepad_devices_list);
    gamepad_box->setLayout(gamepad_box_layout);

    QGridLayout *io_tab_layout = new QGridLayout();
    io_tab_layout->addWidget(midi_ports_box, 0, 0, 3, 1);
    io_tab_layout->addWidget(midi_config_box, 0, 1);
    io_tab_layout->addWidget(gamepad_box, 1, 1);
    io_tab_layout->addWidget(network_box, 2, 1);

    ioTab->setLayout(io_tab_layout);
    return ioTab;
}

/**
 * create Editor Tab of Preferences Widget
 */
QGroupBox* SettingsWidget::createEditorPrefsTab() {
    QGroupBox *editor_box = new QGroupBox();
    QGroupBox *editor_show_panels_box = new QGroupBox(tr("Show Panels"));
    editor_show_panels_box->setToolTip(tr("Show and hide information panes such as the scope and log."));
    QGroupBox *editor_display_box = new QGroupBox(tr("Show and Hide"));
    editor_display_box->setToolTip(tr("Configure editor display options."));
    QGroupBox *editor_look_feel_box = new QGroupBox(tr("Theme"));
    editor_look_feel_box->setToolTip(tr("Configure editor look and feel."));
    QGroupBox *automation_box = new QGroupBox(tr("Automation / Misc"));
    automation_box->setToolTip(tr("Configure automation and other features."));

    auto_indent_on_run = new QCheckBox(tr("Auto-align"));
    auto_indent_on_run->setToolTip(tr("Automatically align code on Enter or Run "));

    show_line_numbers = new QCheckBox(tr("Show line numbers"));
    show_line_numbers->setToolTip(tr("Toggle line number visibility."));

    show_autocompletion = new QCheckBox(tr("Show code completion"));
    show_autocompletion->setToolTip(tr("When enabled, Sonic Pi's editor will attempt to autocomplete your code with suggestions. When disabled, these suggestions will not be visible."));

    show_completion_help = new QCheckBox(tr("Show code completion help"));
    show_completion_help->setToolTip(tr("When enabled, the code completion popup includes helper panes - documentation, a note keyboard and value sliders. When disabled, it shows just the list of suggestions."));

    show_context = new QCheckBox(tr("Show code context"));
    show_context->setToolTip(tr("When enabled, Sonic Pi's editor will show a pane which will display context-specific information for the code such as the current line and position of the cursor."));

    speak_transport = new QCheckBox(tr("Speak run and stop"));
    speak_transport->setToolTip(tr("When enabled, a screen reader announces \"Run started\" and \"Stopped\". Disable this if you'd rather hear the very start of your audio without it being ducked by the announcement."));

    reduce_motion = new QCheckBox(tr("Reduce animations"));
    reduce_motion->setToolTip(tr("When enabled, Sonic Pi keeps its interface still: panes and popups appear in place instead of sliding or gliding. Also switched on automatically while your operating system's reduce-animations accessibility setting is active."));

    show_log = new QCheckBox(tr("Show log"));
    show_log->setToolTip(tr("Toggle visibility of the log."));
    show_log->setProperty("tipShortcut", shortcutStrShiftMeta('L'));
    show_log->setChecked(true);

    show_cues = new QCheckBox(tr("Show cue log"));
    show_cues->setToolTip(tr("Toggle visibility of cue log which displays internal cues & incoming OSC/MIDI messages."));
    show_cues->setProperty("tipShortcut", shortcutStrShiftMeta('C'));
    show_cues->setChecked(true);

    show_metro = new QCheckBox(tr("Show Link metronome controls"));
    show_metro->setToolTip(tr("Toggle visibility of the Link metronome controls."));
    show_cues->setChecked(true);

    show_buttons = new QCheckBox(tr("Show buttons"));
    show_buttons->setToolTip(tr("Toggle visibility of the control buttons."));
    show_buttons->setProperty("tipShortcut", shortcutStrShiftMeta('B'));
    show_buttons->setChecked(true);

    show_editor_toolbar = new QCheckBox(tr("Show editor toolbar"));
    show_editor_toolbar->setToolTip(tr("Toggle visibility of the editor's floating toolbar (undo/redo, cut/copy/paste, find)."));
    show_editor_toolbar->setChecked(true);
    show_tabs = new QCheckBox(tr("Show tabs"));
    show_tabs->setChecked(true);
    show_tabs->setToolTip(tr("Toggle visibility of the buffer selection tabs."));
    full_screen = new QCheckBox(tr("Full screen"));
    full_screen->setToolTip(tr("Toggle full screen mode."));
    full_screen->setProperty("tipShortcut", shortcutStrShiftMeta('F'));

    show_titles = new QCheckBox(tr("Show titles"));
    show_titles->setToolTip(tr("Toggle the title visibility for the scope, log, cue and other information panes"));
    show_titles->setChecked(true);

    hide_menubar_in_fullscreen = new QCheckBox(tr("Hide menu bar in full screen mode"));
    hide_menubar_in_fullscreen->setToolTip(tr("Automatically hide the menubar when the app is in full screen mode. Note that the menubar is always visible when not in full screen mode."));
    hide_menubar_in_fullscreen->setChecked(false);

    // One checkable button per colour scheme, made mutually exclusive by the
    // button group. Icons are a separate choice (proIconsCheck) so any scheme
    // can pair with either icon set.
    colourModeButtonGroup = new QButtonGroup(this);
    lightModeCheck = new ThemeCard(tr("Light"));
    darkModeCheck = new ThemeCard(tr("Dark"));
    highContrastModeCheck = new ThemeCard(tr("High Contrast"));
    mildModeCheck = new ThemeCard(tr("Mild Dark"));
    phosphorModeCheck = new ThemeCard(tr("Phosphor"));
    signalModeCheck = new ThemeCard(tr("Signal"));
    colourModeButtonGroup->addButton(lightModeCheck, 0);
    colourModeButtonGroup->addButton(darkModeCheck, 1);
    colourModeButtonGroup->addButton(highContrastModeCheck, 2);
    colourModeButtonGroup->addButton(mildModeCheck, 3);
    colourModeButtonGroup->addButton(phosphorModeCheck, 4);
    colourModeButtonGroup->addButton(signalModeCheck, 5);

    lightModeCheck->setToolTip(tr("Light colour scheme."));
    darkModeCheck->setToolTip(tr("Dark colour scheme."));
    highContrastModeCheck->setToolTip(tr("High-contrast colour scheme for maximum legibility."));
    mildModeCheck->setToolTip(tr("Mild Dark: a softer, low-contrast dark colour scheme."));
    phosphorModeCheck->setToolTip(tr("Phosphor: a green-on-black CRT colour scheme."));
    signalModeCheck->setToolTip(tr("Signal: high-contrast blue-and-gold colour scheme."));

    // Orthogonal to the colour scheme: swap the classic toolbar icons for the
    // compact Pro set. High Contrast keeps its own icons, so this has no effect
    // there (disabled while High Contrast is selected).
    proIconsCheck = new QCheckBox(tr("Pro icons"));
    proIconsCheck->setToolTip(tr("Use the compact Pro toolbar icon set instead of the classic icons."));

    // Global greyscale toggle: renders every interface colour as luma-matched
    // grey. Independent of the hue rotation above.
    monochromeCheck = new QCheckBox(tr("Monochrome"));
    monochromeCheck->setToolTip(tr("Show the whole interface in greyscale."));

    // Global colour inversion (photo-negative) over the whole interface.
    invertCheck = new QCheckBox(tr("Invert colours"));
    invertCheck->setToolTip(tr("Invert every interface colour (photo-negative)."));

    // Theme picker: a grid of checkable cards, one per theme, each painted in its
    // theme's colours with a toolbar-icon preview above the theme name.
    QWidget* themeGrid = new QWidget();
    themeGrid->setObjectName("themeGrid");
    QGridLayout* themeGridLayout = new QGridLayout(themeGrid);
    themeGridLayout->setContentsMargins(0, 0, 0, 0);
    themeGridLayout->setHorizontalSpacing(6);
    themeGridLayout->setVerticalSpacing(6);

    // Card icon montage (the three Greek glyphs, tinted per card) is built by
    // makeThemeCardGlyphs() so it can be regenerated when the global colour
    // filters change; iconSize here drives the card sizing below.
    const QSize iconSize(ScaleWidthForDPI(80), ScaleHeightForDPI(26));

    // bg/fg: card background and text. border: resting border colour.
    // bg/fg: card background + name text. accent: the scheme's signature colour,
    // used to tint the glyphs so each card reads as its own theme. border: resting.
    struct ThemeSwatch { QPushButton* btn; const char* bg; const char* fg; const char* accent; const char* border; int row; int col; };
    const char* kGrey = "#5a7f7f7f";   // resting border colour (AARRGGBB)
    const ThemeSwatch swatches[] = {
        { lightModeCheck,        "#ffffff", "#3c3c3c", "#ff1493", kGrey,     0, 0 },
        { darkModeCheck,         "#1a1a1a", "#ededed", "#ff1493", kGrey,     0, 1 },
        { highContrastModeCheck, "#ffffff", "#000000", "#99004a", "#000000", 0, 2 },
        { mildModeCheck,         "#1e1e1e", "#d4d4d4", "#ce9178", kGrey,     1, 0 },
        { phosphorModeCheck,     "#0a0e0a", "#8bd450", "#39ff14", kGrey,     1, 1 },
        { signalModeCheck,       "#000000", "#ffffff", "#1e90ff", "#ffd700", 1, 2 },   // black/white, blue + gold
    };
    // Card width is driven by the widest label (the icon sits above the name).
    QFont measureFont = lightModeCheck->font();
    measureFont.setPixelSize(ScaleHeightForDPI(19));   // matches the drawn "medium" size
    QFontMetrics fm(measureFont);
    int maxTextW = 0;
    for (const ThemeSwatch& s : swatches)
        maxTextW = qMax(maxTextW, fm.horizontalAdvance(s.btn->text()));
    // Wide enough for the label and the four-glyph icon row above it.
    const int btnMinW = qMax(maxTextW + ScaleWidthForDPI(28), iconSize.width() + ScaleWidthForDPI(18));
    // A QPushButton doesn't size to its child layout, so drive the height here.
    const int cardH = iconSize.height() + ScaleHeightForDPI(38);
    for (const ThemeSwatch& s : swatches) {
        s.btn->setCheckable(true);
        s.btn->setCursor(Qt::PointingHandCursor);
        s.btn->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
        s.btn->setMinimumWidth(btnMinW);
        // Lock the height on the widget (not via QSS min-height/height): re-setting
        // a QSS height on a button with a child layout makes it creep taller on
        // every repolish, which refreshThemeCards() would trigger on each rotate.
        s.btn->setFixedHeight(cardH);
        // Card fill + border are custom-painted (antialiased) by ThemeCard.
        static_cast<ThemeCard*>(s.btn)->setCardColors(
            QColor(QString::fromLatin1(s.bg)), QColor(QString::fromLatin1(s.border)));
        s.btn->setStyleSheet("QPushButton { padding:0; } QLabel { background:transparent; }");

        // Icon above name, as child labels (a QPushButton lays its own icon+text
        // horizontally). Labels are click-through so the button receives the click.
        QVBoxLayout* card = new QVBoxLayout(s.btn);
        card->setContentsMargins(ScaleWidthForDPI(10), ScaleHeightForDPI(4),
                                 ScaleWidthForDPI(10), ScaleHeightForDPI(8));
        card->setSpacing(ScaleHeightForDPI(3));
        QLabel* iconLbl = new QLabel;
        iconLbl->setPixmap(makeThemeCardGlyphs(QColor(QString::fromLatin1(s.accent))));
        iconLbl->setAlignment(Qt::AlignCenter);
        iconLbl->setAttribute(Qt::WA_TransparentForMouseEvents);
        const QString name = s.btn->text();
        QLabel* nameLbl = new QLabel(name);
        nameLbl->setAlignment(Qt::AlignCenter);
        nameLbl->setAttribute(Qt::WA_TransparentForMouseEvents);
        nameLbl->setStyleSheet(QString("background:transparent; color:%1;")
                                   .arg(QColor(QString::fromLatin1(s.fg)).name()));
        // Remember each card's widgets + base colours so refreshThemeCards() can
        // re-preview them through the active filters (hue / mono / invert).
        m_themeCards.append({ s.btn, iconLbl, nameLbl,
            QColor(QString::fromLatin1(s.bg)), QColor(QString::fromLatin1(s.fg)),
            QColor(QString::fromLatin1(s.accent)), QColor(QString::fromLatin1(s.border)) });
        // Stretches above and below centre the icon+name group vertically.
        card->addStretch(1);
        card->addWidget(iconLbl);
        card->addWidget(nameLbl);
        card->addStretch(1);
        // Name is shown by nameLbl; set it as the accessible name before clearing
        // the button's own text.
        s.btn->setAccessibleName(name);
        s.btn->setText("");

        themeGridLayout->addWidget(s.btn, s.row, s.col);
    }
    // Equal minimum width on the three columns; a trailing stretch column takes
    // any remaining width.
    for (int c = 0; c < 3; ++c)
        themeGridLayout->setColumnMinimumWidth(c, btnMinW);
    themeGridLayout->setColumnStretch(3, 1);

    QVBoxLayout *editor_display_box_layout = new QVBoxLayout;
    QVBoxLayout *editor_show_panels_box_layout = new QVBoxLayout;
    QVBoxLayout *editor_box_look_feel_layout = new QVBoxLayout;
    QVBoxLayout *automation_box_layout = new QVBoxLayout;

    editor_show_panels_box_layout->addWidget(show_log);
    editor_show_panels_box_layout->addWidget(show_cues);
    editor_show_panels_box_layout->addWidget(show_context);
    editor_show_panels_box_layout->addWidget(show_metro);

    editor_display_box_layout->addWidget(show_line_numbers);
    editor_display_box_layout->addWidget(show_autocompletion);
    editor_display_box_layout->addWidget(show_completion_help);
    editor_display_box_layout->addWidget(show_buttons);
    editor_display_box_layout->addWidget(show_editor_toolbar);
    editor_display_box_layout->addWidget(show_tabs);
    editor_display_box_layout->addWidget(show_titles);
#ifndef Q_OS_MAC
    // Don't enable this on Mac as macOS autohides the menubar on
    // fullscreen anyway
    editor_display_box_layout->addWidget(hide_menubar_in_fullscreen);
#endif

    editor_box_look_feel_layout->addWidget(themeGrid);
    // Breathing room between the theme cards and the rotate-colour dial below.
    editor_box_look_feel_layout->addSpacing(ScaleHeightForDPI(26));

    // Colour hue-rotation dial (amp-style ArcDial). Drag vertically to set the
    // rotation; it stops hard at 0 and 359 (no wrap) and shows no value.
    m_hueDial = new ArcDial(this);
    m_hueDial->setRange(0, 359);
    m_hueDial->setWrapping(false);
    m_hueDial->setShowValue(false);
    m_hueDial->setFixedSize(ScaleWidthForDPI(108), ScaleHeightForDPI(108));
    m_hueTimer = new QTimer(this);
    m_hueTimer->setSingleShot(true);
    connect(m_hueDial, &QDial::valueChanged, this, &SettingsWidget::hueRotationChanged);
    connect(m_hueDial, &QAbstractSlider::sliderReleased, this, [this]() { emit themeChanged(); });

    // Left: the dial with its "Rotate Colour" label beneath it (a dial value of
    // 0 is simply no rotation). Right: Pro icons, Monochrome, Invert.
    QVBoxLayout* hueCol = new QVBoxLayout;
    QLabel* rcTitle = new QLabel(tr("Rotate Hue"));
    rcTitle->setAlignment(Qt::AlignHCenter);
    rcTitle->setToolTip(tr("Drag the dial to rotate the hue of every colour in the interface."));
    // Centre the dial + label in its half, both vertically and horizontally.
    hueCol->addStretch(1);
    hueCol->addWidget(m_hueDial, 0, Qt::AlignHCenter);
    hueCol->addWidget(rcTitle, 0, Qt::AlignHCenter);
    hueCol->addStretch(1);

    // Right column: the three toggles stacked tightly and centred vertically so
    // they sit level with the dial beside them instead of floating at the top.
    QVBoxLayout* toggleCol = new QVBoxLayout;
    toggleCol->setSpacing(ScaleHeightForDPI(4));
    toggleCol->addStretch(1);
    toggleCol->addWidget(proIconsCheck);
    toggleCol->addWidget(monochromeCheck);
    toggleCol->addWidget(invertCheck);
    toggleCol->addStretch(1);

    // Two equal halves: toggles on the left, dial (+ label) on the right.
    QHBoxLayout* tweaks = new QHBoxLayout;
    tweaks->addLayout(toggleCol, 1);
    tweaks->addLayout(hueCol, 1);
    editor_box_look_feel_layout->addLayout(tweaks);

    editor_show_panels_box->setLayout(editor_show_panels_box_layout);
    editor_display_box->setLayout(editor_display_box_layout);
    editor_look_feel_box->setLayout(editor_box_look_feel_layout);

    QGroupBox *accessibility_box = new QGroupBox(tr("Accessibility"));
    accessibility_box->setToolTip(tr("Settings that support screen readers and other assistive tools."));
    QVBoxLayout *accessibility_box_layout = new QVBoxLayout;
    accessibility_box_layout->addWidget(speak_transport);
    accessibility_box_layout->addWidget(reduce_motion);
    accessibility_box->setLayout(accessibility_box_layout);


    automation_box_layout->addWidget(auto_indent_on_run);
    automation_box_layout->addWidget(full_screen);

    automation_box->setLayout(automation_box_layout);

    QGroupBox *debug_box = new QGroupBox(tr("Logging"));
    debug_box->setToolTip(tr("Configure debug behaviour"));

    log_synths = new QCheckBox(tr("Log synths"));
    log_synths->setToolTip(tr("If disabled, activity such as synth and sample triggering will not be printed to the log by default."));

    clear_output_on_run = new QCheckBox(tr("Clear log on run"));
    clear_output_on_run->setToolTip(tr("If enabled, the log is cleared each time the run button is pressed."));

    log_cues = new QCheckBox(tr("Log cues"));
    log_cues->setToolTip(tr("If disabled, cues will still trigger. However, they will not be visible in the logs."));

    log_auto_scroll = new QCheckBox(tr("Auto-scroll log"));
    log_auto_scroll->setToolTip(tr("If enabled, the log is scrolled to the bottom after every new message is displayed."));

    QVBoxLayout *debug_box_layout = new QVBoxLayout;
    debug_box_layout->addWidget(log_synths);
    debug_box_layout->addWidget(log_cues);
    debug_box_layout->addWidget(log_auto_scroll);
    debug_box_layout->addWidget(clear_output_on_run);
    debug_box->setLayout(debug_box_layout);

    // Two independent columns rather than a shared grid: grid rows take the
    // taller of the two sides, which stretched Show and Hide to match the
    // right-hand boxes and squeezed the theme cards. Independent columns let
    // each side pack to its own content, with any spare height left at the
    // bottom of each column.
    QVBoxLayout *leftEditorPrefs = new QVBoxLayout;
    leftEditorPrefs->addWidget(editor_look_feel_box);
    leftEditorPrefs->addWidget(editor_display_box);
    leftEditorPrefs->addStretch(1);

    QVBoxLayout *rightEditorPrefs = new QVBoxLayout;
    rightEditorPrefs->addWidget(debug_box);
    rightEditorPrefs->addWidget(editor_show_panels_box);
    rightEditorPrefs->addWidget(automation_box);
    rightEditorPrefs->addWidget(accessibility_box);
    rightEditorPrefs->addStretch(1);

    QHBoxLayout *editorPrefsColumns = new QHBoxLayout;
    editorPrefsColumns->addLayout(leftEditorPrefs, 1);
    editorPrefsColumns->addLayout(rightEditorPrefs, 1);

    editor_box->setLayout(editorPrefsColumns);
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
    show_scopes = new QCheckBox(tr("Show scopes"));
    show_scopes->setToolTip(tr("Toggle the visibility of the audio oscilloscopes."));
    show_scope_labels = new QCheckBox(tr("Show scope labels"));
    show_scope_labels->setToolTip(tr("Toggle the visibility of the labels for the audio oscilloscopes"));
    show_scope_labels->setChecked(true);

    scope_box_kinds->setLayout(scope_box_kinds_layout);
    scope_box_kinds->setToolTip(tr("The audio oscilloscope comes in several flavours which may be viewed independently or all together:\n\nLissajous - illustrates the phase relationship between the left and right channels\nMirror Stereo - simple left/right composite wave, with left on top, right on bottom\nMono - shows a combined view of the left and right channels (using RMS)\nSpectrum - shows the sound frequencies as a spectrum, from low to high frequencies\nStereo - shows two independent scopes for left and right channels"));
    scope_box_layout->addWidget(show_scopes);
    scope_box_layout->addWidget(show_scope_labels);
    scope_box->setLayout(scope_box_layout);
    viz_tab_layout->addWidget(scope_box, 0, 0);
    viz_tab_layout->addWidget(scope_box_kinds, 1, 0);

    // In-editor visuals driven by the running audio: trigger flashes and the
    // per-live_loop mini scopes.
    QGroupBox *editor_visuals_box = new QGroupBox(tr("Editor Visuals"));
    QVBoxLayout *editor_visuals_box_layout = new QVBoxLayout;

    flash_code = new QCheckBox(tr("Flash code on sound trigger"));
    flash_code->setToolTip(tr("When enabled, the editor briefly washes the code responsible for each sound as it is triggered."));

    flash_gutter = new QCheckBox(tr("Flash gutter on sound trigger"));
    flash_gutter->setToolTip(tr("When enabled, the editor briefly shows a dot in the gutter next to the line responsible for each sound as it is triggered."));

    show_loop_scopes = new QCheckBox(tr("Show live loop scopes"));
    show_loop_scopes->setToolTip(tr("When enabled, each running live loop shows a small oscilloscope and spectrum of its own audio next to its line in the editor."));
    loop_scope_scroll = new QCheckBox(tr("Scrolling live loop scopes"));
    loop_scope_scroll->setToolTip(tr("When enabled, live loop scopes scroll their recent audio like a strip chart. When disabled, they hold a steady waveform like the main scope."));

    // Brightness as an amp-style ArcDial (same feel as the volume + hue
    // dials), with the percentage shown in the hub.
    flash_brightness_slider = new ArcDial(this);
    flash_brightness_slider->setWrapping(false);
    flash_brightness_slider->setRange(5, 100);
    flash_brightness_slider->setValueSuffix("%");
    flash_brightness_slider->setValueFontPt(20);
    flash_brightness_slider->setFixedSize(ScaleWidthForDPI(108), ScaleHeightForDPI(108));
    flash_brightness_slider->setAccessibleName(tr("Flash brightness"));
    flash_brightness_slider->setProperty("tipTitle", tr("Flash Brightness"));
    flash_brightness_slider->setToolTip(tr("Drag or scroll to change how strongly the code flash washes the line."));

    QLabel *flash_brightness_label = new QLabel(tr("Flash Brightness"));
    flash_brightness_label->setAlignment(Qt::AlignHCenter);

    // Checkboxes on the left, dial to their right — same arrangement as the
    // hue dial in the look & feel section. The box hugs its content (the tab
    // grid no longer stretches rows), so both columns centre naturally.
    QVBoxLayout *flash_checks_col = new QVBoxLayout;
    flash_checks_col->addStretch(1);
    flash_checks_col->addWidget(flash_code);
    flash_checks_col->addWidget(flash_gutter);
    flash_checks_col->addWidget(show_loop_scopes);
    flash_checks_col->addWidget(loop_scope_scroll);
    flash_checks_col->addStretch(1);
    QVBoxLayout *flash_dial_col = new QVBoxLayout;
    flash_dial_col->addStretch(1);
    flash_dial_col->addWidget(flash_brightness_slider, 0, Qt::AlignHCenter);
    flash_dial_col->addWidget(flash_brightness_label, 0, Qt::AlignHCenter);
    flash_dial_col->addStretch(1);
    QHBoxLayout *editor_visuals_row = new QHBoxLayout;
    editor_visuals_row->addLayout(flash_checks_col);
    editor_visuals_row->addSpacing(ScaleWidthForDPI(40));
    editor_visuals_row->addLayout(flash_dial_col);
    editor_visuals_row->addStretch(1);
    editor_visuals_box_layout->addLayout(editor_visuals_row);
    editor_visuals_box->setLayout(editor_visuals_box_layout);
    viz_tab_layout->addWidget(editor_visuals_box, 2, 0);

    QGroupBox *transparency_box = new QGroupBox(tr("Transparency"));
    QGridLayout *transparency_box_layout = new QGridLayout;
    gui_transparency_slider = new QSlider(this);
    gui_transparency_slider->setAccessibleName(tr("Transparency"));
    QLabel *transparency_value_label = new QLabel();
    transparency_value_label->setAlignment(Qt::AlignHCenter);
    connect(gui_transparency_slider, &QSlider::valueChanged, transparency_value_label,
        [transparency_value_label](int v) {
            transparency_value_label->setText(QString("%1%").arg(v));
        });
    transparency_value_label->setText(QString("%1%").arg(gui_transparency_slider->value()));
    transparency_box_layout->addWidget(gui_transparency_slider, 0, 0, Qt::AlignHCenter);
    transparency_box_layout->addWidget(transparency_value_label, 1, 0);
    transparency_box->setLayout(transparency_box_layout);

//#if defined(Q_OS_LINUX)
//    // do nothing
//#else
    // Framed like the other groups and spanning the left stack's three rows,
    // so its frame bottom lines up with the Editor Visuals box.
    viz_tab_layout->addWidget(transparency_box, 0, 1, 3, 1);
//#endif

    // Groups hug their content and stack from the top; leftover tab height
    // goes to an empty stretch row, and leftover width to the left column —
    // no more group boxes ballooning to fill the tab.
    viz_tab_layout->setRowStretch(3, 1);
    viz_tab_layout->setColumnStretch(0, 1);
    viz_tab_layout->setHorizontalSpacing(ScaleWidthForDPI(24));
    viz_tab_layout->setVerticalSpacing(ScaleHeightForDPI(18));
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
    check_updates->setToolTip(tr("This check involves sending anonymous information about your platform and version."));
    check_updates_now = new QPushButton(tr("Check now"));
    check_updates_now->setFlat(true);
    check_updates_now->setSizePolicy(QSizePolicy(QSizePolicy::Minimum, QSizePolicy::Fixed));
    check_updates_now->setToolTip(tr("Force a check for updates now. This check involves sending anonymous information about your platform and version."));
    visit_sonic_pi_net = new QPushButton(tr("Get update"));
    visit_sonic_pi_net->setSizePolicy(QSizePolicy(QSizePolicy::Minimum, QSizePolicy::Fixed));
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

    if (piSettings->language == "system_language") {
      if (!sonicPii18n->isSystemLanguageAvailable()) {
          QGroupBox *translation_box = new QGroupBox("Translation");
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
          language_box_layout->addWidget(go_translate);
      }
    }


    language_box->setLayout(language_box_layout);

    QGroupBox *language_prefs_box = new QGroupBox();
    QGridLayout *language_prefs_box_layout = new QGridLayout;
    language_prefs_box_layout->addWidget(language_box, 0, 0, 0, 0);
    language_prefs_box->setLayout(language_prefs_box_layout);
    return language_prefs_box;
}

// Default key string for a command under a given base preset.
static QString baseKeyFor(const ShortcutDef& d, const QString& base) {
    if (base == "win") return QString(d.win);
    if (base == "emacs") return QString(d.emacs);
    return QString(d.mac);
}

// Read a shortcut .ini: its "base" preset (left at the caller's default if the
// file omits it) plus the per-command overrides it stores.
static void readShortcutIni(const QString& path, QString& base, QMap<QString, QString>& overrides) {
    QSettings cfg(path, QSettings::IniFormat);
    base = cfg.value("base", base).toString();
    overrides.clear();
    for (const ShortcutDef& d : MainWindow::shortcutDefs()) {
        if (cfg.contains(d.id)) overrides.insert(d.id, cfg.value(d.id).toString());
    }
}

// Normalise a shortcut string to a comparable form so equivalent chords written
// differently (e.g. "MetaShift+." vs "ShiftMeta+.") compare equal. Returns
// "<sorted-modifiers>|<key>"; empty for an unset binding.
static QString canonicalChord(const QString& raw) {
    QString s = raw.trimmed().toLower();
    if (s.isEmpty()) return QString();
    QStringList parts = s.split('+', Qt::SkipEmptyParts);
    if (parts.isEmpty()) return QString();
    QString key = parts.takeLast();
    QString modBlob = parts.join("");
    QStringList mods;
    for (const QString& m : { QStringLiteral("ctrl"), QStringLiteral("shift"),
                              QStringLiteral("alt"), QStringLiteral("meta") }) {
        if (modBlob.contains(m)) mods << m;
    }
    mods.sort();
    return mods.join("+") + "|" + key;
}

// Delegate that restricts editing to the Shortcut column of a command row.
// Base key text (no modifiers), e.g. "R", "Left", "F1", ",".
static QString shortcutKeyName(int key) {
    if (key == 0 || key == Qt::Key_unknown) return QString();
    return QKeySequence(key).toString(QKeySequence::PortableText);
}

// Convert a captured key chord into Sonic Pi shortcut notation that
// resolveShortcut() maps back to the identical QKeySequence. Each leading
// prefix (Meta/Ctrl/ShiftMeta/CtrlMeta/CtrlShift) consumes its modifier; the
// remainder is parsed by QKeySequence, so it must only carry Qt-native tokens.
static QString chordToSonicPiNotation(int key, Qt::KeyboardModifiers mods) {
    QString k = shortcutKeyName(key);
    if (k.isEmpty()) return QString();

#ifdef Q_OS_MAC
    const bool cmd   = mods & Qt::ControlModifier; // Cmd  -> "Meta"
    const bool ctrl  = mods & Qt::MetaModifier;    // ctrl -> "Ctrl"
    const bool alt   = mods & Qt::AltModifier;     // Option
    const bool shift = mods & Qt::ShiftModifier;

    QString prefix;
    bool shiftConsumed = false;
    if (cmd && ctrl)        prefix = "CtrlMeta";
    else if (cmd && shift)  { prefix = "ShiftMeta"; shiftConsumed = true; }
    else if (cmd)           prefix = "Meta";
    else if (ctrl && shift) { prefix = "CtrlShift"; shiftConsumed = true; }
    else if (ctrl)          prefix = "Ctrl";

    QStringList rest;
    if (alt) rest << "Alt";
    if (shift && !shiftConsumed) rest << "Shift";
    rest << k;
    QString tail = rest.join("+");
    return prefix.isEmpty() ? tail : prefix + "+" + tail;
#else
    // Non-mac: "Meta" maps to Alt, "Ctrl" to Ctrl (see metaKey()/ctrlKey()).
    const bool ctrl    = mods & Qt::ControlModifier;
    const bool metaTok = mods & Qt::AltModifier; // Alt -> "Meta"
    const bool shift   = mods & Qt::ShiftModifier;

    QString prefix;
    bool shiftConsumed = false;
    if (ctrl && metaTok)        prefix = "CtrlMeta";
    else if (metaTok && shift)  { prefix = "ShiftMeta"; shiftConsumed = true; }
    else if (metaTok)           prefix = "Meta";
    else if (ctrl && shift)     { prefix = "CtrlShift"; shiftConsumed = true; }
    else if (ctrl)              prefix = "Ctrl";

    QStringList rest;
    if (shift && !shiftConsumed) rest << "Shift";
    rest << k;
    QString tail = rest.join("+");
    return prefix.isEmpty() ? tail : prefix + "+" + tail;
#endif
}

// In-place editor that captures a pressed key chord rather than typed text.
class ShortcutRecorder : public QLineEdit {
public:
    explicit ShortcutRecorder(QWidget* parent = nullptr) : QLineEdit(parent) {
        setReadOnly(true);
        setAlignment(Qt::AlignCenter);
        setPlaceholderText(QObject::tr("Type shortcut…"));
        // Prominent "listening" look so it's obvious the cell is recording.
        setStyleSheet(
            "QLineEdit { background: palette(highlight); color: palette(highlighted-text);"
            " font-weight: bold; border: 2px solid palette(highlight); }");
    }
    bool captured() const { return m_captured; }
protected:
    // While recording, claim every chord so application/editor QAction
    // shortcuts don't swallow it before keyPressEvent() runs.
    bool event(QEvent* e) override {
        if (e->type() == QEvent::ShortcutOverride) {
            e->accept();
            return true;
        }
        return QLineEdit::event(e);
    }
    void keyPressEvent(QKeyEvent* e) override {
        switch (e->key()) {
        case Qt::Key_Shift: case Qt::Key_Control: case Qt::Key_Alt:
        case Qt::Key_Meta: case Qt::Key_AltGr: case Qt::Key_CapsLock:
        case Qt::Key_unknown:
            e->ignore();
            return;
        case Qt::Key_Escape:
            QLineEdit::keyPressEvent(e); // let the view cancel the edit
            return;
        case Qt::Key_Backspace: case Qt::Key_Delete:
            m_captured = true;
            setText(QString()); // clear the binding
            emit editingFinished();
            e->accept();
            return;
        default:
            break;
        }
        QString notation = chordToSonicPiNotation(e->key(), e->modifiers());
        if (!notation.isEmpty()) {
            m_captured = true;
            setText(notation);
            emit editingFinished();
        }
        e->accept();
    }
private:
    bool m_captured = false;
};

// Delegate: only the Shortcut column of a command row is editable, via the
// key recorder.
class ShortcutKeyDelegate : public QStyledItemDelegate {
public:
    using QStyledItemDelegate::QStyledItemDelegate;
    QWidget* createEditor(QWidget* parent, const QStyleOptionViewItem&,
                          const QModelIndex& index) const override {
        if (index.column() != 1 || !index.parent().isValid()) return nullptr;
        ShortcutRecorder* rec = new ShortcutRecorder(parent);
        ShortcutKeyDelegate* self = const_cast<ShortcutKeyDelegate*>(this);
        // Commit exactly once, and only if a chord was actually recorded.
        // editingFinished can re-fire on focus-out (e.g. when the conflict
        // dialog opens); committing twice on the same live editor crashes.
        auto done = std::make_shared<bool>(false);
        QObject::connect(rec, &QLineEdit::editingFinished, self, [self, rec, done]() {
            if (*done) return;
            *done = true;
            if (rec->captured()) emit self->commitData(rec);
            emit self->closeEditor(rec);
        });
        return rec;
    }
    void setEditorData(QWidget* editor, const QModelIndex&) const override {
        // Start empty so the "Type shortcut…" prompt shows it is listening.
        static_cast<ShortcutRecorder*>(editor)->setText(QString());
    }
    void setModelData(QWidget* editor, QAbstractItemModel* model,
                      const QModelIndex& index) const override {
        model->setData(index, static_cast<ShortcutRecorder*>(editor)->text(), Qt::EditRole);
    }
    // Render the stored "Meta+R" notation as the platform-native chord (⌘R),
    // matching the menu bar. The underlying value stays in Sonic Pi notation.
    QString displayText(const QVariant& value, const QLocale&) const override {
        const QString s = value.toString().trimmed();
        if (s.isEmpty()) return s;
        const QString native = MainWindow::resolveShortcut(s).toString(QKeySequence::NativeText);
        return native.isEmpty() ? s : native;
    }
};

QGroupBox* SettingsWidget::createKeyboardShortcutsTab() {
    QGroupBox *shortcuts_box = new QGroupBox();

    // Segmented "pill" toggle: a subtle track with the active mode filled in
    // the accent colour. More visible than a dropdown, same row height.
    auto makeSeg = [](const QString& text) {
        QPushButton* b = new QPushButton(text);
        b->setCheckable(true);
        b->setCursor(Qt::PointingHandCursor);
        return b;
    };
    QPushButton* macBtn = makeSeg(tr("Mac"));
    QPushButton* winBtn = makeSeg(tr("Windows | Linux"));
    QPushButton* emacsBtn = makeSeg(tr("Emacs Live"));
    QPushButton* customBtn = makeSeg(tr("Custom"));
    shortcutSchemeGroup = new QButtonGroup(this);
    shortcutSchemeGroup->addButton(macBtn, 3);
    shortcutSchemeGroup->addButton(winBtn, 2);
    shortcutSchemeGroup->addButton(emacsBtn, 1);
    shortcutSchemeGroup->addButton(customBtn, 4);

    QWidget* segControl = new QWidget();
    segControl->setObjectName("segControl");
    // Styled by the shared "segmented control" rule in app.qss (house metrics).
    segControl->setProperty("segmented", true);
    QHBoxLayout* segLayout = new QHBoxLayout(segControl);
    segLayout->setContentsMargins(3, 3, 3, 3);
    segLayout->setSpacing(3);
    segLayout->addWidget(macBtn);
    segLayout->addWidget(winBtn);
    segLayout->addWidget(emacsBtn);
    segLayout->addWidget(customBtn);

    QHBoxLayout *schemeRow = new QHBoxLayout;
    schemeRow->addStretch();
    schemeRow->addWidget(new QLabel(tr("Mode:")));
    schemeRow->addWidget(segControl);
    schemeRow->addStretch();

    // Custom-only controls: base preset, manage buttons, and the edit hint.
    shortcutCustomControls = new QWidget();
    shortcutBaseCombo = new QComboBox();
    shortcutBaseCombo->addItem(tr("Mac"), "mac");
    shortcutBaseCombo->addItem(tr("Windows | Linux"), "win");
    shortcutBaseCombo->addItem(tr("Emacs Live"), "emacs");
    shortcutEditRowButton = new QPushButton(tr("Edit Shortcut"));
    shortcutEditRowButton->setEnabled(false);
    QPushButton *resetButton = new QPushButton(tr("Reset"));
    resetButton->setStyleSheet(
        "QPushButton { background: palette(highlight); color: palette(highlighted-text); }");
    QPushButton *importButton = new QPushButton(tr("Import…"));
    QPushButton *exportButton = new QPushButton(tr("Export…"));

    shortcutModifiedLabel = new QLabel();
    shortcutModifiedLabel->setStyleSheet("QLabel { font-style: italic; }");

    QHBoxLayout *ccTop = new QHBoxLayout;
    ccTop->setContentsMargins(0, 0, 0, 0);
    ccTop->addWidget(new QLabel(tr("Base:")));
    ccTop->addWidget(shortcutBaseCombo);
    ccTop->addWidget(shortcutEditRowButton);
    ccTop->addStretch();
    ccTop->addWidget(shortcutModifiedLabel);
    ccTop->addWidget(importButton);
    ccTop->addWidget(exportButton);
    ccTop->addWidget(resetButton);

    shortcutCustomControls->setLayout(ccTop);

    shortcutTree = new QTreeWidget();
    shortcutTree->setColumnCount(2);
    shortcutTree->setHeaderHidden(true);
    shortcutTree->setSelectionMode(QAbstractItemView::SingleSelection);
    shortcutTree->setAlternatingRowColors(true);
    shortcutTree->setStyleSheet(
        "QTreeView { alternate-background-color: rgba(127,127,127,26); }"
        "QTreeView::item { padding-top: 4px; padding-bottom: 4px; }");
    // Delegate only on the Shortcut column: it both records new chords and
    // renders the binding natively (⌘R) instead of the raw "Meta+R" notation.
    shortcutTree->setItemDelegateForColumn(1, new ShortcutKeyDelegate(shortcutTree));
    shortcutTree->setEditTriggers(QAbstractItemView::DoubleClicked
        | QAbstractItemView::SelectedClicked | QAbstractItemView::EditKeyPressed);
    shortcutTree->header()->setSectionResizeMode(0, QHeaderView::Stretch);
    shortcutTree->header()->setSectionResizeMode(1, QHeaderView::ResizeToContents);

    QVBoxLayout *layout = new QVBoxLayout;
    layout->addLayout(schemeRow);
    layout->addWidget(shortcutTree, 1);
    layout->addWidget(shortcutCustomControls);
    shortcuts_box->setLayout(layout);

    int mode = piSettings->shortcut_mode;
    if (QAbstractButton* active = shortcutSchemeGroup->button(mode)) active->setChecked(true);
    shortcutCustomControls->setVisible(mode == 4);
    reloadShortcutTree();

    connect(shortcutSchemeGroup, &QButtonGroup::idToggled, this,
        [this](int, bool checked) { if (checked) onShortcutSchemeToggled(); });
    connect(shortcutBaseCombo, QOverload<int>::of(&QComboBox::currentIndexChanged), this,
        [this](int) {
            QString newBase = shortcutBaseCombo->currentData().toString();
            // Keep the user's edits (diffs from the old base) as a layer over the new base.
            QMap<QString, QString> overrides = collectDiffsAgainst(shortcutEditBase);
            fillShortcutTree(shortcutTree, newBase, overrides, true);
            shortcutEditBase = newBase;
            applyShortcuts();
        });
    connect(shortcutTree, &QTreeWidget::itemChanged, this, &SettingsWidget::onShortcutItemChanged);
    // Enable Edit only when a command row (not a group header) is selected.
    connect(shortcutTree, &QTreeWidget::itemSelectionChanged, this, [this]() {
        QTreeWidgetItem* it = shortcutTree->currentItem();
        shortcutEditRowButton->setEnabled(it && it->parent() != nullptr
            && (it->flags() & Qt::ItemIsEditable));
    });
    // Edit / double-click both start recording the selected row's shortcut.
    connect(shortcutEditRowButton, &QPushButton::clicked, this, [this]() {
        QTreeWidgetItem* it = shortcutTree->currentItem();
        if (it && it->parent()) shortcutTree->editItem(it, 1);
    });
    connect(resetButton, &QPushButton::clicked, this, [this]() { resetShortcutsToBase(); });
    connect(importButton, &QPushButton::clicked, this, [this]() { importShortcuts(); });
    connect(exportButton, &QPushButton::clicked, this, [this]() { exportShortcuts(); });

    return shortcuts_box;
}

// Resolve the base preset + user overrides for the currently-active scheme.
void SettingsWidget::currentBaseAndOverrides(QString& base, QMap<QString, QString>& overrides) const {
    int mode = piSettings->shortcut_mode;
    base = (mode == 2) ? "win" : (mode == 1) ? "emacs" : "mac";
    overrides.clear();
    if (mode == 4 && !shortcutConfigPath.isEmpty() && QFile::exists(shortcutConfigPath)) {
        readShortcutIni(shortcutConfigPath, base, overrides);
    }
}

// Fill a tree grouped by menu category. editable=true marks command rows
// editable (the delegate confines editing to the Shortcut column).
void SettingsWidget::fillShortcutTree(QTreeWidget* tree, const QString& base,
                                      const QMap<QString, QString>& overrides, bool editable) {
    QSignalBlocker blocker(tree);
    tree->clear();
    QMap<QString, QTreeWidgetItem*> groups;
    for (const ShortcutDef& d : MainWindow::shortcutDefs()) {
        QString grp(d.group);
        QTreeWidgetItem* parent = groups.value(grp, nullptr);
        if (!parent) {
            parent = new QTreeWidgetItem(tree, QStringList{ grp });
            parent->setFlags(Qt::ItemIsEnabled);
            parent->setFirstColumnSpanned(true);
            QFont gf = parent->font(0);
            gf.setBold(true);
            gf.setPointSizeF(gf.pointSizeF() * 1.1);
            parent->setFont(0, gf);
            parent->setBackground(0, QColor(127, 127, 127, 120));
            parent->setExpanded(true);
            groups.insert(grp, parent);
        }
        QString baseKey = baseKeyFor(d, base);
        QString key = overrides.value(d.id, baseKey);
        QTreeWidgetItem* item = new QTreeWidgetItem(parent);
        item->setText(0, QCoreApplication::translate("MainWindow", d.desc));
        item->setText(1, key);
        item->setData(0, Qt::UserRole, QString(d.id));
        item->setData(1, Qt::UserRole, key); // last-applied value, for revert-on-cancel
        if (editable) item->setFlags(item->flags() | Qt::ItemIsEditable);
    }
    restyleShortcutTree(tree, base);
}

// Colour + weight every command row: red for a binding shared by 2+ commands
// (a conflict), accent + bold for one changed from the base preset, default
// otherwise. Tooltips explain each.
void SettingsWidget::restyleShortcutTree(QTreeWidget* tree, const QString& base) {
    QMap<QString, QString> baseOf;
    for (const ShortcutDef& d : MainWindow::shortcutDefs()) baseOf.insert(d.id, baseKeyFor(d, base));

    QMap<QString, QList<QTreeWidgetItem*>> byVal;
    for (int g = 0; g < tree->topLevelItemCount(); ++g) {
        QTreeWidgetItem* p = tree->topLevelItem(g);
        for (int i = 0; i < p->childCount(); ++i) {
            QString c = canonicalChord(p->child(i)->text(1));
            if (!c.isEmpty()) byVal[c].append(p->child(i));
        }
    }

    int modifiedCount = 0;
    QSignalBlocker b(tree);
    for (int g = 0; g < tree->topLevelItemCount(); ++g) {
        QTreeWidgetItem* p = tree->topLevelItem(g);
        for (int i = 0; i < p->childCount(); ++i) {
            QTreeWidgetItem* it = p->child(i);
            QString id = it->data(0, Qt::UserRole).toString();
            QString v = it->text(1).trimmed();
            const QList<QTreeWidgetItem*>& sharing = byVal.value(canonicalChord(v));
            bool conflict = !v.isEmpty() && sharing.size() > 1;
            bool modified = baseOf.contains(id) && v != baseOf.value(id);
            if (modified) ++modifiedCount;

            QFont f = it->font(1);
            f.setBold(false);
            f.setItalic(modified);
            it->setFont(1, f);

            if (conflict) {
                QStringList others;
                for (QTreeWidgetItem* o : sharing) if (o != it) others << o->text(0);
                it->setForeground(1, QColor(0xE0, 0x52, 0x52));
                it->setToolTip(1, tr("Also assigned to: %1").arg(others.join(", ")));
            } else if (modified) {
                QString def = baseOf.value(id);
                it->setForeground(1, tree->palette().color(QPalette::Highlight));
                it->setToolTip(1, tr("Changed from default (%1)").arg(def.isEmpty() ? tr("unset") : def));
            } else {
                it->setData(1, Qt::ForegroundRole, QVariant());
                it->setToolTip(1, QString());
            }
        }
    }

    if (shortcutModifiedLabel) {
        shortcutModifiedLabel->setText(modifiedCount == 0
            ? tr("(no changes)")
            : tr("(%1 changed)").arg(modifiedCount));
    }
}

// Rebuild the single tree for the active scheme; editable only for Custom.
void SettingsWidget::reloadShortcutTree() {
    if (!shortcutTree) return;
    int mode = piSettings->shortcut_mode;
    bool editable = (mode == 4);
    // Presets are read-only: no selection / focus, so nothing looks interactive.
    shortcutTree->setSelectionMode(editable ? QAbstractItemView::SingleSelection
                                            : QAbstractItemView::NoSelection);
    shortcutTree->setFocusPolicy(editable ? Qt::StrongFocus : Qt::NoFocus);
    if (shortcutEditRowButton) shortcutEditRowButton->setEnabled(false);

    QString base;
    QMap<QString, QString> overrides;
    currentBaseAndOverrides(base, overrides);
    {
        QSignalBlocker b(shortcutBaseCombo);
        int i = shortcutBaseCombo->findData(base);
        if (i >= 0) shortcutBaseCombo->setCurrentIndex(i);
    }
    shortcutEditBase = base;
    fillShortcutTree(shortcutTree, base, overrides, editable);
}

// Gather the command rows whose current binding differs from the given base.
QMap<QString, QString> SettingsWidget::collectDiffsAgainst(const QString& base) const {
    QMap<QString, QString> current;
    for (int g = 0; g < shortcutTree->topLevelItemCount(); ++g) {
        QTreeWidgetItem* parent = shortcutTree->topLevelItem(g);
        for (int i = 0; i < parent->childCount(); ++i) {
            QTreeWidgetItem* it = parent->child(i);
            current.insert(it->data(0, Qt::UserRole).toString(), it->text(1).trimmed());
        }
    }
    QMap<QString, QString> diffs;
    for (const ShortcutDef& d : MainWindow::shortcutDefs()) {
        QString id(d.id);
        if (current.contains(id) && current.value(id) != baseKeyFor(d, base)) {
            diffs.insert(id, current.value(id));
        }
    }
    return diffs;
}

// Gather only the command rows that differ from the chosen base preset.
QMap<QString, QString> SettingsWidget::collectShortcutDiffs(QString* outBase) const {
    QString base = shortcutBaseCombo->currentData().toString();
    if (outBase) *outBase = base;
    return collectDiffsAgainst(base);
}

void SettingsWidget::onShortcutSchemeToggled() {
    int mode = shortcutSchemeGroup->checkedId();
    shortcutCustomControls->setVisible(mode == 4);
    emit shortcutSchemeChanged(mode); // MainWindow sets the mode + reapplies
    reloadShortcutTree();             // reflect the new mode (editable iff Custom)
}

void SettingsWidget::applyShortcuts() {
    QString base;
    QMap<QString, QString> diffs = collectShortcutDiffs(&base);
    emit shortcutsApplyRequested(base, diffs);
}

void SettingsWidget::onShortcutItemChanged(QTreeWidgetItem* item, int column) {
    if (column != 1 || !item->parent()) return;
    if (m_inShortcutChange) return; // editor focus-out can re-fire; ignore re-entry
    QScopedValueRollback<bool> guard(m_inShortcutChange, true);

    QString base = shortcutBaseCombo->currentData().toString();
    QString newVal = item->text(1).trimmed();
    QString prevVal = item->data(1, Qt::UserRole).toString();

    if (!newVal.isEmpty()) {
        QString canon = canonicalChord(newVal);
        QList<QTreeWidgetItem*> clashes;
        for (int g = 0; g < shortcutTree->topLevelItemCount(); ++g) {
            QTreeWidgetItem* p = shortcutTree->topLevelItem(g);
            for (int i = 0; i < p->childCount(); ++i) {
                QTreeWidgetItem* o = p->child(i);
                if (o != item && canonicalChord(o->text(1)) == canon) clashes.append(o);
            }
        }
        if (!clashes.isEmpty()) {
            QStringList names;
            for (QTreeWidgetItem* o : clashes) names << o->text(0);
            QMessageBox box(this);
            box.setIcon(QMessageBox::Warning);
            box.setWindowTitle(tr("Shortcut already in use"));
            box.setText(tr("\"%1\" is already assigned to: %2.").arg(newVal, names.join(", ")));
            box.setInformativeText(tr("What would you like to do?"));
            QPushButton* reassign = box.addButton(tr("Reassign to this"), QMessageBox::AcceptRole);
            box.addButton(tr("Keep both"), QMessageBox::ActionRole);
            QPushButton* cancel = box.addButton(QMessageBox::Cancel);
            box.exec();
            if (box.clickedButton() == cancel) {
                {
                    QSignalBlocker b(shortcutTree);
                    item->setText(1, prevVal);
                }
                restyleShortcutTree(shortcutTree, base);
                return; // nothing applied
            }
            if (box.clickedButton() == reassign) {
                QSignalBlocker b(shortcutTree);
                for (QTreeWidgetItem* o : clashes) {
                    o->setText(1, QString());
                    o->setData(1, Qt::UserRole, QString());
                }
            }
            // "Keep both" leaves the clash; restyle will flag it red.
        }
    }

    item->setData(1, Qt::UserRole, newVal);
    restyleShortcutTree(shortcutTree, base);
    applyShortcuts();
}

void SettingsWidget::resetShortcutsToBase() {
    if (QMessageBox::warning(this, tr("Reset shortcuts?"),
            tr("This discards all your custom changes and restores the base preset. Continue?"),
            QMessageBox::Yes | QMessageBox::No, QMessageBox::No) != QMessageBox::Yes) {
        return;
    }
    fillShortcutTree(shortcutTree, shortcutBaseCombo->currentData().toString(), {}, true);
    applyShortcuts();
}

void SettingsWidget::exportShortcuts() {
    QString path = QFileDialog::getSaveFileName(this, tr("Export Shortcuts"),
        "sonic-pi-shortcuts.ini", tr("Shortcut files (*.ini)"));
    if (path.isEmpty()) return;
    QString base;
    QMap<QString, QString> diffs = collectShortcutDiffs(&base);
    QSettings cfg(path, QSettings::IniFormat);
    cfg.clear();
    cfg.setValue("base", base);
    for (auto it = diffs.constBegin(); it != diffs.constEnd(); ++it) {
        cfg.setValue(it.key(), it.value());
    }
    cfg.sync();
}

void SettingsWidget::importShortcuts() {
    QString path = QFileDialog::getOpenFileName(this, tr("Import Shortcuts"),
        QString(), tr("Shortcut files (*.ini)"));
    if (path.isEmpty()) return;
    QString base = "mac";
    QMap<QString, QString> overrides;
    readShortcutIni(path, base, overrides);
    {
        QSignalBlocker b(shortcutSchemeGroup);
        if (QAbstractButton* customBtn = shortcutSchemeGroup->button(4)) customBtn->setChecked(true);
    }
    shortcutCustomControls->setVisible(true);
    {
        QSignalBlocker b(shortcutBaseCombo);
        int baseIdx = shortcutBaseCombo->findData(base);
        if (baseIdx >= 0) shortcutBaseCombo->setCurrentIndex(baseIdx);
    }
    fillShortcutTree(shortcutTree, base, overrides, true);
    shortcutEditBase = base;
    emit shortcutSchemeChanged(4);
    applyShortcuts();
}


// Display string for the tooltip popup's key-cap chip (set as the
// "tipShortcut" widget property — see sonicpitooltip.h).
QString SettingsWidget::shortcutStrShiftMeta(char key) {
#ifdef Q_OS_MAC
    return QString("⇧⌘%1").arg(key);
#else
    return QString("Shift+Alt+%1").arg(key);
#endif
}

void SettingsWidget::updateScopeNames( std::vector<QString> names ) {
    piSettings->scope_names = names;
    // Per-kind descriptions for the tooltip popup; keep in sync with the
    // scope kinds published by the scope window.
    QMap<QString, QString> scopeDescriptions;
    scopeDescriptions["Lissajous"]     = tr("Illustrates the phase relationship between the left and right channels.");
    scopeDescriptions["Mirror Stereo"] = tr("A simple left/right composite wave, with left on top, right on bottom.");
    scopeDescriptions["Mono"]          = tr("A combined view of the left and right channels (using RMS).");
    scopeDescriptions["Spectrum"]      = tr("The sound frequencies as a spectrum, from low to high.");
    scopeDescriptions["Stereo"]        = tr("Two independent scopes for the left and right channels.");
    for( auto name : names ) {
        QCheckBox* cb = new QCheckBox( name );
        cb->setChecked( piSettings->isScopeActive(name));
        cb->setToolTip(scopeDescriptions.value(name,
            tr("Toggle the visibility of the %1 oscilloscope.").arg(name)));
        scope_box_kinds_layout->addWidget(cb);
        connect(cb, &QCheckBox::clicked, this, [=]() {
          toggleScope(cb);
        });

    }
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

void SettingsWidget::toggleScope( QObject* qo ) {
  auto qw = (QWidget*) qo;
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

        // Show confirmation box
        QMessageBox msgBox(this);
        msgBox.setText(QString(tr("You've selected a new language: %1")).arg(new_lang));
        QString info_text = (
          tr("Do you want to apply this language?")
          + "\n"
          + tr("The new language will be applied when you next start Sonic Pi.")
        );

        if (lang == "system_language") {
            // Determine the actual language to load
            QString actual_lang = sonicPii18n->determineUILanguage(lang);
            info_text = tr("System language found: %1").arg(sonicPii18n->getNativeLanguageName(actual_lang)) + "\n" + info_text;
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

void SettingsWidget::updateEnableScsynthInputs() {
    bool inputsEnabled = enable_scsynth_inputs->isChecked();
    if (!inputsEnabled) {
        // Update the UI immediately — SuperSonic's broadcast can lag
        QSignalBlocker blocker(audio_input_combo);
        audio_input_combo->clear();
        audio_input_combo->addItem(tr("-- DISABLED --"), QString("__disabled__"));
        audio_input_combo->setCurrentIndex(0);
        audio_input_combo->setEnabled(false);
    } else {
        // Next /supersonic/input-devices broadcast will populate
        audio_input_combo->setEnabled(true);
    }
    emit enableScsynthInputsChanged();
    updateMicPermissionStatus(); // show/hide the mic notice to match the toggle
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

void SettingsWidget::toggleGamepad() {
    emit gamepadSettingsChanged();
}

void SettingsWidget::updateMidiInPorts( QString in ) {
    midi_in_ports_list->setDevices( in );
}

void SettingsWidget::updateMidiOutPorts( QString out ) {
    midi_out_ports_list->setDevices( out );
}

void SettingsWidget::updateGamepadDevices( QString devices ) {
    gamepad_devices_list->setDevices( devices );
}

void SettingsWidget::updateScsynthInfo( QString scsynthInfo ) {
  supersonicBox->setToolTip(scsynthInfo);
  // Show "Switching audio device..." and disable controls during device changes
  if (scsynthInfo.contains("Switching audio")) {
    supersonic_version_label->setText(tr("Switching audio device..."));
    audio_output_combo->setEnabled(false);
    audio_input_combo->setEnabled(false);
    audio_sample_rate_combo->setEnabled(false);
    audio_buffer_size_combo->setEnabled(false);
    audio_driver_combo->setEnabled(false);
    m_switchTimeoutTimer->start(15000);
  }
}

void SettingsWidget::updateAudioDevices(const SonicPi::AudioDevicesInfo& devicesInfo) {
    // Skip rebuild if nothing changed — /supersonic/devices fires several
    // times per boot and rebuilding invalidates the dropdown cache
    if (devicesInfo.devices == m_lastAudioDevicesInfo.devices &&
        devicesInfo.currentDevice == m_lastAudioDevicesInfo.currentDevice &&
        devicesInfo.mode == m_lastAudioDevicesInfo.mode &&
        devicesInfo.sampleRate == m_lastAudioDevicesInfo.sampleRate) {
        return;
    }
    m_lastAudioDevicesInfo = devicesInfo;

    std::cout << "[gui-audio] updateAudioDevices: mode='" << devicesInfo.mode
              << "' currentDevice='" << devicesInfo.currentDevice
              << "' numDevices=" << devicesInfo.devices.size() << std::endl;
    QSignalBlocker blocker(audio_output_combo);

    audio_output_combo->clear();

    // System Default with resolved device as suffix — "__system__" sentinel
    // in itemData is what gets sent to SuperSonic
    // ASIO has no OS-level "default device" concept \u2014 each ASIO driver IS
    // its single device. Substitute "-- None --" (sentinel `__none__`,
    // NO OSC fired when picked) so the user explicitly chooses an ASIO
    // device. Other drivers keep the System Default behaviour.
    QString selectedDriver = audio_driver_combo->currentText();
    bool isAsio = (selectedDriver == "ASIO");
    bool inSystemMode = (devicesInfo.mode.empty() || devicesInfo.mode == "system");
    if (isAsio) {
        audio_output_combo->addItem(tr("-- None --"), QString("__none__"));
    } else {
        QString systemDefaultLabel = tr("System Default");
        if (inSystemMode && !devicesInfo.currentDevice.empty()) {
            systemDefaultLabel = tr("System Default (\u2192 %1)")
                .arg(QString::fromStdString(devicesInfo.currentDevice));
        }
        audio_output_combo->addItem(systemDefaultLabel, QString("__system__"));
    }

    // Filter by selected driver. ASIO selection lists only ASIO
    // devices; non-ASIO selections hide ASIO devices (the engine has
    // already deduped Windows-Audio / DirectSound entries by name).
    bool haveTypes = devicesInfo.deviceTypes.size() == devicesInfo.devices.size();
    for (size_t i = 0; i < devicesInfo.devices.size(); ++i) {
        const auto& dev = devicesInfo.devices[i];
        if (haveTypes && !selectedDriver.isEmpty()) {
            QString qt = QString::fromStdString(devicesInfo.deviceTypes[i]);
            if (isAsio) {
                if (qt != "ASIO") continue;
            } else {
                if (qt == "ASIO") continue;
            }
        }
        audio_output_combo->addItem(QString::fromStdString(dev));
    }

    // Selection priority:
    //   non-ASIO + mode==system → System Default sentinel
    //   non-ASIO                → concrete device name
    //   ASIO, engine on ASIO    → concrete device name
    //   ASIO, engine not on ASIO yet → leave on "-- None --"
    //     (currentDevice reflects whichever non-ASIO driver JUCE is
    //      still on, displaying it as the ASIO output would be wrong)
    bool selectedBySystem = false;
    bool engineIsOnAsio   = (m_engineActualDriver == "ASIO");
    if (!isAsio && inSystemMode) {
        int idx = audio_output_combo->findData(QString("__system__"));
        if (idx >= 0) {
            audio_output_combo->setCurrentIndex(idx);
            selectedBySystem = true;
        }
    }
    bool selectByCurrent = !selectedBySystem
                        && !devicesInfo.currentDevice.empty()
                        && (!isAsio || engineIsOnAsio);
    if (selectByCurrent) {
        int idx = audio_output_combo->findText(QString::fromStdString(devicesInfo.currentDevice));
        if (idx >= 0) {
            audio_output_combo->setCurrentIndex(idx);
        }
    }
}

void SettingsWidget::updateAudioInputDevices(const SonicPi::AudioInputDevicesInfo& devicesInfo) {
    // No change-detection guard — the enable checkbox needs repopulation
    // even when the device list is unchanged
    m_lastAudioInputDevicesInfo = devicesInfo;
    QSignalBlocker blocker(audio_input_combo);

    QString previousSelection = audio_input_combo->currentText();
    bool inputsEnabled = enable_scsynth_inputs->isChecked();

    audio_input_combo->clear();

    if (!inputsEnabled) {
        audio_input_combo->addItem(tr("-- DISABLED --"), QString("__disabled__"));
        audio_input_combo->setCurrentIndex(0);
        audio_input_combo->setEnabled(false);
        return;
    }

    // "-- None --" = no specific input (keep SuperSonic's current); not the
    // same as DISABLED which the checkbox owns
    audio_input_combo->setEnabled(true);
    audio_input_combo->addItem(tr("-- None --"));
    QString selDriver = audio_driver_combo->currentText();
    bool isAsioDr = (selDriver == "ASIO");
    bool haveTypesIn = devicesInfo.deviceTypes.size() == devicesInfo.devices.size();
    // The engine resolves a swap's input name strictly within the active
    // driver, so an input typed under a different driver would be refused
    // ("unknown input device") — don't offer it. The Windows Audio mode
    // variants expose the same endpoint names, so they count as one family.
    // ASIO stays a coarse yes/no bucket: an ASIO device is its own driver
    // and we can't probe its inputs from here.
    auto driverFamily = [](const QString& t) {
        return t.startsWith("Windows Audio") ? QString("Windows Audio") : t;
    };
    for (size_t i = 0; i < devicesInfo.devices.size(); ++i) {
        const auto& dev = devicesInfo.devices[i];
        if (haveTypesIn && !selDriver.isEmpty()) {
            QString qt = QString::fromStdString(devicesInfo.deviceTypes[i]);
            if (isAsioDr) {
                if (qt != "ASIO") continue;
            } else if (qt == "ASIO") {
                continue;
            } else if (driverFamily(qt) != driverFamily(selDriver)) {
                continue;
            }
        }
        audio_input_combo->addItem(QString::fromStdString(dev));
    }

    if (!devicesInfo.currentDevice.empty()) {
        int idx = audio_input_combo->findText(QString::fromStdString(devicesInfo.currentDevice));
        audio_input_combo->setCurrentIndex(idx >= 0 ? idx : 0);
    } else {
        // No active input — show None rather than a stale selection
        audio_input_combo->setCurrentIndex(0);
    }
    (void)previousSelection;
}

void SettingsWidget::updateAudioDeviceConfig(const SonicPi::AudioDeviceConfigInfo& configInfo) {
    QSignalBlocker srBlocker(audio_sample_rate_combo);
    QSignalBlocker bsBlocker(audio_buffer_size_combo);
    QSignalBlocker drBlocker(audio_driver_combo);

    audio_sample_rate_combo->clear();
    for (int rate : configInfo.availableSampleRates) {
        audio_sample_rate_combo->addItem(QString::number(rate), rate);
    }
    if (configInfo.sampleRate > 0) {
        int idx = audio_sample_rate_combo->findData(configInfo.sampleRate);
        if (idx >= 0) {
            audio_sample_rate_combo->setCurrentIndex(idx);
        }
    }

    audio_buffer_size_combo->clear();
    for (int bs : configInfo.availableBufferSizes) {
        audio_buffer_size_combo->addItem(QString::number(bs), bs);
    }
    if (configInfo.bufferSize > 0) {
        int idx = audio_buffer_size_combo->findData(configInfo.bufferSize);
        if (idx >= 0) {
            audio_buffer_size_combo->setCurrentIndex(idx);
        }
    }

    // Driver dropdown doubles as pending user intent (Driver=ASIO isn't
    // committed until an Output device is also picked) and a mirror of the
    // engine's live driver. The report's intendedDriver settles the conflict
    // authoritatively: it carries the engine's own pending-pick record, which
    // survives failed swaps and recovery reopens — and when it's explicitly
    // empty, a differing local selection is stale and follows the engine.
    // Reports without the field (older engine) fall back to inference:
    // m_engineActualDriver still holds the driver from the previous report
    // here (refreshed below), which lets choose_driver_selection tell an
    // engine-synced value from a deliberate override.
    QString userSelection = audio_driver_combo->currentText();
    bool wasEmpty = audio_driver_combo->count() == 0;
    audio_driver_combo->clear();
    for (const auto& driver : configInfo.availableDrivers) {
        audio_driver_combo->addItem(QString::fromStdString(driver));
    }
    std::string driverToSelect = sonic_pi::audio::choose_driver_selection(
        wasEmpty,
        userSelection.toStdString(),
        m_engineActualDriver.toStdString(),
        configInfo.availableDrivers,
        configInfo.currentDriver,
        configInfo.hasIntendedDriver,
        configInfo.intendedDriver);
    if (!driverToSelect.empty()) {
        int idx = audio_driver_combo->findText(QString::fromStdString(driverToSelect));
        if (idx >= 0) {
            audio_driver_combo->setCurrentIndex(idx);
        }
    }

    // Cache the engine's actual driver. updateAudioDevices uses it to
    // distinguish the user-picked-ASIO-engine-on-ASIO case (select the
    // concrete device) from user-picked-ASIO-engine-still-elsewhere
    // (default Output to "-- None --" instead of the stale driver's
    // device).
    m_engineActualDriver = QString::fromStdString(configInfo.currentDriver);
    m_engineCurrentSampleRate = configInfo.sampleRate;
    m_engineCurrentBufferSize = configInfo.bufferSize;

    // Update SuperSonic summary with live config (matches SuperSonic's own format)
    QString versionText = QString("%1 Hz | buffer %2 | out %3 | in %4")
        .arg(configInfo.sampleRate)
        .arg(configInfo.bufferSize)
        .arg(configInfo.outputChannels)
        .arg(configInfo.inputChannels);
    if (!configInfo.currentDriver.empty()) {
        versionText += QString(" | %1").arg(QString::fromStdString(configInfo.currentDriver));
    }
    const bool remoteSession = isRemoteDesktopSession();
    if (remoteSession) {
        versionText += QString(" | %1").arg(tr("remote session"));
    }
    remote_session_note->setVisible(remoteSession);
    supersonic_version_label->setText(versionText);

    // Re-enable controls after device switch completes
    m_switchTimeoutTimer->stop();
    audio_output_combo->setEnabled(true);
    audio_input_combo->setEnabled(true);
    audio_sample_rate_combo->setEnabled(true);
    audio_buffer_size_combo->setEnabled(true);
    audio_driver_combo->setEnabled(true);

    // ASIO-driver constraints may have changed (e.g. driver swapped).
    // Re-apply so the input checkbox + dropdown reflect the new driver.
    applyAsioInputConstraints();

    // Force a re-render of the device dropdowns. activated(int) only
    // fires on user-interaction — programmatic setCurrentIndex above
    // doesn't trigger audioDriverChanged, so without a manual re-run
    // any /supersonic/devices message that arrived before the driver
    // combo was populated would have been processed with an empty
    // selectedDriver and bypassed the driver filter.
    {
        SonicPi::AudioDevicesInfo cached = m_lastAudioDevicesInfo;
        m_lastAudioDevicesInfo = SonicPi::AudioDevicesInfo{};
        if (!cached.devices.empty()) updateAudioDevices(cached);
        if (!m_lastAudioInputDevicesInfo.devices.empty()
            || !m_lastAudioInputDevicesInfo.currentDevice.empty()) {
            SonicPi::AudioInputDevicesInfo cachedIn = m_lastAudioInputDevicesInfo;
            m_lastAudioInputDevicesInfo = SonicPi::AudioInputDevicesInfo{};
            updateAudioInputDevices(cachedIn);
        }
    }
}

void SettingsWidget::audioDriverChanged(int index) {
    if (index < 0) return;
    // Re-render the output and input dropdowns with the new driver's
    // filter. Both populate functions read audio_driver_combo->currentText()
    // when filtering, so just calling them with the cached info applies
    // the new filter. updateAudioDevices has a change-detection guard
    // that short-circuits if the snapshot is identical — clear it first
    // so the re-render actually runs.
    SonicPi::AudioDevicesInfo cached = m_lastAudioDevicesInfo;
    m_lastAudioDevicesInfo = SonicPi::AudioDevicesInfo{};
    if (!cached.devices.empty()) updateAudioDevices(cached);
    if (!m_lastAudioInputDevicesInfo.devices.empty()
        || !m_lastAudioInputDevicesInfo.currentDevice.empty()) {
        updateAudioInputDevices(m_lastAudioInputDevicesInfo);
    }
    applyAsioInputConstraints();
    emit driverChanged(audio_driver_combo->currentText());
}

void SettingsWidget::applyAsioInputConstraints() {
    QString driver = audio_driver_combo->currentText();
    bool isAsio = (driver == "ASIO");

    if (isAsio && !asio_constraint_applied) {
        // Entering ASIO: remember the user's previous checkbox state so
        // we can restore it on exit, then force-tick + grey out the
        // checkbox. ASIO is full-duplex by spec; users who want truly
        // input-free output need to switch to Windows Audio / DirectSound.
        asio_saved_input_checked = enable_scsynth_inputs->isChecked();
        asio_saved_input_tooltip = enable_scsynth_inputs->toolTip();
        asio_constraint_applied  = true;
    }

    if (isAsio) {
        QSignalBlocker b(enable_scsynth_inputs);
        enable_scsynth_inputs->setChecked(true);
        enable_scsynth_inputs->setEnabled(false);
        // Style label AND indicator. Sonic Pi's theme keeps the
        // indicator vivid when setEnabled(false), so the label
        // greying alone doesn't read as non-interactive.
        enable_scsynth_inputs->setStyleSheet(
            "QCheckBox { color: gray; font-style: italic; }"
            "QCheckBox::indicator {"
            " background-color: rgba(128, 128, 128, 80);"
            " border: 1px solid rgba(128, 128, 128, 140);"
            "}");
        enable_scsynth_inputs->setToolTip(
            tr("ASIO devices have linked input/output."));
        asio_input_note->setVisible(true);

        // Input dropdown on ASIO mirrors the Output selection (ASIO is a
        // single device on both directions). When the user hasn't yet
        // picked a valid ASIO output device, the Output dropdown is on
        // its "-- None --" entry — mirror that into the Input dropdown
        // instead of leaving stale Windows Audio inputs visible.
        QString outName     = audio_output_combo->currentText();
        QString outData     = audio_output_combo->currentData().toString();
        bool noOutputPicked = outName.isEmpty()
                              || outData == "__none__"
                              || outData == "__system__"
                              || outName.startsWith(tr("System Default"));
        QSignalBlocker ib(audio_input_combo);
        if (noOutputPicked) {
            int idx = audio_input_combo->findText(tr("-- None --"));
            if (idx >= 0) audio_input_combo->setCurrentIndex(idx);
        } else {
            int idx = audio_input_combo->findText(outName);
            if (idx >= 0) audio_input_combo->setCurrentIndex(idx);
        }
        audio_input_combo->setEnabled(false);
        audio_input_combo->setToolTip(tr("Mirrors Output (ASIO is full-duplex)."));
    } else if (asio_constraint_applied) {
        // Leaving ASIO: restore prior state.
        QSignalBlocker b(enable_scsynth_inputs);
        enable_scsynth_inputs->setChecked(asio_saved_input_checked);
        enable_scsynth_inputs->setEnabled(true);
        enable_scsynth_inputs->setStyleSheet(QString());
        enable_scsynth_inputs->setToolTip(asio_saved_input_tooltip.isEmpty()
            ? tr("Toggle to enable or disable audio inputs.")
            : asio_saved_input_tooltip);
        asio_input_note->setVisible(false);
        audio_input_combo->setEnabled(true);
        audio_input_combo->setToolTip(QString());
        asio_constraint_applied = false;
    }
}

void SettingsWidget::updateMicPermissionStatus() {
#if defined(Q_OS_DARWIN)
    // The microphone only feeds live_audio / :sound_in, so this notice is only
    // relevant when audio inputs are enabled. With inputs off, stay hidden.
    if (!enable_scsynth_inputs->isChecked()) {
        mic_permission_label->setVisible(false);
        mic_permission_settings_button->setVisible(false);
        m_lastMicPermissionStatus.clear(); // re-evaluate when inputs are re-enabled
        return;
    }

    std::string status = SonicPi::microphonePermissionStatus();
    if (status == m_lastMicPermissionStatus) return;  // no-op change
    m_lastMicPermissionStatus = status;
    std::cout << "[gui-mic] status now: " << status << std::endl;

    if (status == "authorized") {
        mic_permission_label->setVisible(false);
        mic_permission_settings_button->setVisible(false);
    } else {
        QString msg;
        if (status == "denied")
            msg = tr("Sonic Pi doesn't have microphone access yet, so live_audio and :sound_in will be silent. Click below to grant access in System Settings.");
        else if (status == "restricted")
            msg = tr("Microphone access is restricted by system policy, so live_audio and :sound_in will be silent.");
        else  // notDetermined
            msg = tr("Microphone access not yet granted — click below to open System Settings.");
        mic_permission_label->setText(msg);
        mic_permission_label->setStyleSheet(
            "QLabel { color: palette(highlight); font-weight: bold; }");
        mic_permission_label->setVisible(true);
        mic_permission_settings_button->setVisible(true);
    }
#endif
}

void SettingsWidget::audioDeviceChanged(int index) {
    if (index < 0) return;
    // If the selected item carries a non-empty itemData string (e.g. the
    // "System Default" sentinel stores "__system__"), emit that instead of
    // the user-visible text. Regular device entries have no itemData so
    // they fall through to currentText() as before.
    QString data = audio_output_combo->currentData().toString();
    // "-- None --" entry (ASIO mode) carries `__none__` data and means
    // "do nothing": the user is on ASIO but hasn't picked an ASIO
    // device yet. Don't fire any OSC — keep the engine on whatever
    // device it was already on.
    if (data == "__none__") {
        std::cout << "[gui-audio] output dropdown picked -- None -- (ASIO no-op)" << std::endl;
        applyAsioInputConstraints();
        return;
    }
    QString emitted = data.isEmpty() ? audio_output_combo->currentText() : data;
    std::cout << "[gui-audio] output dropdown changed: index=" << index
              << " text='" << audio_output_combo->currentText().toUtf8().constData()
              << "' data='" << data.toUtf8().constData()
              << "' emitting='" << emitted.toUtf8().constData() << "'" << std::endl;
    emit audioOutputDeviceChanged(emitted);
    // On ASIO, the input combo mirrors the output. Re-apply so the input
    // dropdown stays in sync with the just-picked output device.
    applyAsioInputConstraints();
}

void SettingsWidget::audioInputDeviceChanged(int index) {
    if (index < 0) return;
    QString data = audio_input_combo->currentData().toString();
    QString emitted = data.isEmpty() ? audio_input_combo->currentText() : data;
    std::cout << "[gui-audio] input dropdown changed: index=" << index
              << " text='" << audio_input_combo->currentText().toUtf8().constData()
              << "' data='" << data.toUtf8().constData()
              << "' emitting='" << emitted.toUtf8().constData() << "'" << std::endl;
    emit audioInputDeviceChangedSignal(emitted);
}

void SettingsWidget::audioSampleRateChanged(int index) {
    if (index < 0) return;
    int rate = audio_sample_rate_combo->currentData().toInt();
    std::cout << "[gui-audio] sample-rate dropdown changed: index=" << index
              << " rate=" << rate << std::endl;
    emit sampleRateChanged(rate);
}

void SettingsWidget::audioBufferSizeChanged(int index) {
    if (index < 0) return;
    int bs = audio_buffer_size_combo->currentData().toInt();
    std::cout << "[gui-audio] buffer-size dropdown changed: index=" << index
              << " bs=" << bs << std::endl;
    emit bufferSizeChanged(bs);
}

#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
void SettingsWidget::recordingTypeChanged(int mode) {
    if (mode < 0) return;  // no button checked
    // mode is the enum value (button IDs were set to it directly).
    // Persistence + sync is owned by MainWindow::setRecordingMode.
    emit recordingModeChangedFromPrefs(mode);
}
#endif

void SettingsWidget::changeMainVolume(int vol) {
    emit volumeChanged(vol);
}

void SettingsWidget::toggleLineNumbers() {
    emit showLineNumbersChanged();
}

void SettingsWidget::showAutoCompletion() {
  emit showAutoCompletionChanged();
}

void SettingsWidget::showCompletionHelp() {
  emit showCompletionHelpChanged();
}

void SettingsWidget::showContext() {
  emit showContextChanged();
}

void SettingsWidget::flashOnPlay() {
  emit flashSettingsChanged();
}

void SettingsWidget::speakTransport() {
  emit speakTransportChanged();
}

void SettingsWidget::reduceMotion() {
  emit reduceMotionChanged();
}

void SettingsWidget::toggleLog() {
    emit showLogChanged();
}

void SettingsWidget::toggleCuesLog() {
    emit showCuesChanged();
}

void SettingsWidget::toggleMetro() {
    emit showMetroChanged();
}

void SettingsWidget::toggleButtons() {
    emit showButtonsChanged();
}

void SettingsWidget::toggleEditorToolbar() {
    emit showEditorToolbarChanged();
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

// Tint the dial to the theme's highlight/accent colour rotated by `degrees`, so
// dragging previews exactly what the accent will become. Repolishing this one
// small widget is cheap; the whole-page re-theme is deferred to release.
void SettingsWidget::updateHueDialTint(int degrees) {
    if (!m_hueDial || !m_huePreviewBase.isValid()) return;
    // Run the accent through the theme's transform pipeline with the dial's
    // in-flight rotation, so the dial previews the accent exactly as the rest
    // of the interface will render it under the current toggles.
    m_hueDial->setArcColor(SonicPiTheme::applyColourTransforms(
        m_huePreviewBase,
        piSettings && piSettings->invert_colours,
        piSettings && piSettings->monochrome,
        degrees));
}

void SettingsWidget::setHuePreviewBase(const QColor& baseAccent) {
    m_huePreviewBase = baseAccent;
    updateHueDialTint(piSettings ? piSettings->hue_rotation : 0);
}

void SettingsWidget::hueRotationChanged(int degrees) {
    piSettings->hue_rotation = degrees;
    updateHueDialTint(degrees);   // live feedback on the dial only (arc + centre value)
    if (!m_hueDial || !m_hueDial->isSliderDown())
        emit themeChanged();      // keyboard/wheel step (not a drag): apply immediately
}

// The three iconic Sonic Pi Greek glyphs (lambda, delta, pi), drawn in a centred
// row and recoloured to `tint` (masked by each glyph's own alpha) so they read on
// any card background. A member (not a build-time lambda) so refreshThemeCards()
// can regenerate it when the global colour filters change.
QPixmap SettingsWidget::makeThemeCardGlyphs(const QColor& tint) const {
    const QSize iconSize(ScaleWidthForDPI(80), ScaleHeightForDPI(26));
    const qreal dpr = devicePixelRatioF();
    static const char* const paths[] = {
        ":/images/toolbar/pro/info.png",    // lambda
        ":/images/toolbar/pro/help.png",    // delta
        ":/images/toolbar/pro/prefs.png",   // pi
    };
    QPixmap pm(iconSize * dpr);
    pm.setDevicePixelRatio(dpr);
    pm.fill(Qt::transparent);
    QPainter p(&pm);
    p.setRenderHint(QPainter::Antialiasing, true);
    p.setRenderHint(QPainter::SmoothPixmapTransform, true);
    const qreal w = iconSize.width(), h = iconSize.height();
    const qreal ih = h * 0.82;   // glyph height; width follows aspect ratio
    auto tinted = [&](const QString& path) -> QPixmap {
        QPixmap src(path);
        if (src.isNull()) return src;
        QPixmap t(src.size());
        t.setDevicePixelRatio(src.devicePixelRatio());
        t.fill(Qt::transparent);
        QPainter tp(&t);
        tp.drawPixmap(0, 0, src);
        tp.setCompositionMode(QPainter::CompositionMode_SourceIn);
        tp.fillRect(t.rect(), tint);
        tp.end();
        return t;
    };
    QList<QPixmap> glyphs;
    qreal totalW = 0;
    const qreal gap = ScaleWidthForDPI(7);
    for (const char* path : paths) {
        const QPixmap t = tinted(QString::fromLatin1(path));
        glyphs.append(t);
        if (!t.isNull())
            totalW += ih * (qreal(t.width()) / qMax(1, t.height()));
    }
    if (!glyphs.isEmpty())
        totalW += gap * (glyphs.size() - 1);
    qreal x = (w - totalW) / 2.0;
    for (const QPixmap& t : glyphs) {
        if (t.isNull()) continue;
        const qreal iw = ih * (qreal(t.width()) / qMax(1, t.height()));
        p.drawPixmap(QRectF(x, (h - ih) / 2.0, iw, ih), t, QRectF(t.rect()));
        x += iw + gap;
    }
    p.end();
    return pm;
}

void SettingsWidget::refreshThemeCards(SonicPiTheme* theme) {
    m_cardTheme = theme;
    if (!theme) return;
    // Preview each card's palette as the interface would render it under the
    // current global filters, so the cards track hue rotation / monochrome /
    // invert along with everything else.
    for (const ThemeCardInfo& c : m_themeCards) {
        const QColor bg     = theme->applyGlobalTransforms(c.bg);
        const QColor fg     = theme->applyGlobalTransforms(c.fg);
        const QColor accent = theme->applyGlobalTransforms(c.accent);
        const QColor border = theme->applyGlobalTransforms(c.border);
        static_cast<ThemeCard*>(c.card)->setCardColors(bg, border);
        // The selected/hover ring uses the current theme's accent (not a fixed pink).
        static_cast<ThemeCard*>(c.card)->setHighlight(theme->color("HighlightedBackground"));
        // Recolour the name label directly — re-setting the button's own
        // stylesheet here would re-polish its child layout and make the card
        // creep taller on every rotate.
        if (c.name) c.name->setStyleSheet(QString("background:transparent; color:%1;").arg(fg.name()));
        if (c.icon) c.icon->setPixmap(makeThemeCardGlyphs(accent));
    }

    // Recording-mode segmented icons bake their off/on colours in, so regenerate
    // them on theme change: resting = window foreground, selected = auto-contrast
    // against the accent fill (black on neon green, white on dark, …).
#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    if (recording_type_audio_radio && recording_type_av_radio) {
        const int px = ScaleHeightForDPI(32);
        const QColor off = theme->color("WindowForeground");
        const QColor on = theme->contrastingText(theme->color("HighlightedBackground"));
        recording_type_audio_radio->setIcon(makeSvgToggleIcon(kWaveformSvg, off, on, px));
        recording_type_av_radio->setIcon(makeSvgToggleIcon(kVideoSvg, off, on, px));
    }
#endif
}


void SettingsWidget::toggleScope() {
    emit scopeChanged();
}

void SettingsWidget::toggleScopeLabels() {
    emit scopeLabelsChanged();
}

void SettingsWidget::toggleTitles() {
    emit titlesChanged();
}

void SettingsWidget::toggleHideMenuBarInFullscreen() {
    emit hideMenuBarInFullscreenChanged();
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
    piSettings->enable_scsynth_inputs = enable_scsynth_inputs->isChecked();
    piSettings->mixer_force_mono = mixer_force_mono->isChecked();
    piSettings->check_args = check_args->isChecked();
    piSettings->synth_trigger_timing_guarantees = synth_trigger_timing_guarantees_cb->isChecked();
    piSettings->enable_external_synths = enable_external_synths_cb->isChecked();
    piSettings->main_volume = system_vol_slider->value();

    piSettings->osc_server_enabled = osc_server_enabled_check->isChecked();
    piSettings->osc_public = osc_server_enabled_check->isChecked() && osc_public_check->isChecked();
    if(piSettings->osc_server_enabled){
      osc_public_check->show();
    } else {
      osc_public_check->hide();
    }
    if(!osc_server_enabled_check->isChecked()) {
      osc_public_check->setChecked(false);
    }

    QString channel_pat_str = midi_default_channel_combo->currentText();
    if(channel_pat_str.startsWith("*")) {
      channel_pat_str = QString("*");
    }

    piSettings->midi_default_channel = midi_default_channel_combo->currentIndex();
    piSettings->midi_default_channel_str = channel_pat_str;
    piSettings->midi_enabled = midi_enable_check->isChecked();
    piSettings->gamepad_enabled = gamepad_enable_check->isChecked();

    piSettings->auto_indent_on_run = auto_indent_on_run->isChecked();
    piSettings->show_line_numbers = show_line_numbers->isChecked();
    piSettings->show_autocompletion = show_autocompletion->isChecked();
    piSettings->show_completion_help = show_completion_help->isChecked();
    piSettings->show_context = show_context->isChecked();
    piSettings->flash_code = flash_code->isChecked();
    piSettings->flash_brightness = flash_brightness_slider->value();
    piSettings->flash_gutter = flash_gutter->isChecked();
    piSettings->show_loop_scopes = show_loop_scopes->isChecked();
    piSettings->loop_scope_scroll = loop_scope_scroll->isChecked();
    piSettings->speak_transport = speak_transport->isChecked();
    piSettings->reduce_motion = reduce_motion->isChecked();
    // Widgets consult prefersReducedMotion() directly (no settings pointer
    // there), so push the preference into the shared flag as it changes.
    SonicPi::setReduceMotionPreference(piSettings->reduce_motion);
    piSettings->show_log = show_log->isChecked();
    piSettings->show_cues = show_cues->isChecked();
    piSettings->show_metro = show_metro->isChecked();
    piSettings->show_buttons = show_buttons->isChecked();
    piSettings->show_editor_toolbar = show_editor_toolbar->isChecked();
    piSettings->show_tabs = show_tabs->isChecked();
    piSettings->full_screen = full_screen->isChecked();
    piSettings->log_synths = log_synths->isChecked();
    piSettings->clear_output_on_run = clear_output_on_run->isChecked();
    piSettings->log_cues = log_cues->isChecked();
    piSettings->log_auto_scroll = log_auto_scroll->isChecked();
    piSettings->gui_transparency = gui_transparency_slider->value();
    SonicPiTheme::ColourScheme scheme = SonicPiTheme::LightScheme;
    if (darkModeCheck->isChecked())         { scheme = SonicPiTheme::DarkScheme; }
    if (highContrastModeCheck->isChecked()) { scheme = SonicPiTheme::HighContrastScheme; }
    if (mildModeCheck->isChecked())         { scheme = SonicPiTheme::MildDarkScheme; }
    if (phosphorModeCheck->isChecked())     { scheme = SonicPiTheme::PhosphorScheme; }
    if (signalModeCheck->isChecked())       { scheme = SonicPiTheme::SignalScheme; }
    piSettings->colourScheme = scheme;
    piSettings->proIcons = proIconsCheck->isChecked();
    if (m_hueDial) piSettings->hue_rotation = m_hueDial->value();
    if (monochromeCheck) piSettings->monochrome = monochromeCheck->isChecked();
    if (invertCheck) piSettings->invert_colours = invertCheck->isChecked();

    piSettings->show_scopes = show_scopes->isChecked();
    piSettings->show_scope_labels = show_scope_labels->isChecked();
    piSettings->show_titles = show_titles->isChecked();
    piSettings->hide_menubar_in_fullscreen = hide_menubar_in_fullscreen->isChecked();

    piSettings->check_updates = check_updates->isChecked();
}

void SettingsWidget::settingsChanged() {
    language_combo->setCurrentIndex(available_languages.indexOf(piSettings->language));
    QString language_detail_text = "";
    if (!i18n) {
      language_detail_text += "<b>Failed to load language translation. Using English (UK).</b>";
    }
    if (piSettings->language == "system_language") {
      language_detail_text += tr("System language: %1\n").arg(sonicPii18n->getNativeLanguageName(sonicPii18n->currentlyLoadedLanguage()));
    }
    language_details_label->setText(language_detail_text);

    mixer_invert_stereo->setChecked(piSettings->mixer_invert_stereo);
    mixer_force_mono->setChecked(piSettings->mixer_force_mono);
    enable_scsynth_inputs->setChecked(piSettings->enable_scsynth_inputs);
    check_args->setChecked(piSettings->check_args);
    synth_trigger_timing_guarantees_cb->setChecked( piSettings->synth_trigger_timing_guarantees);
    enable_external_synths_cb->setChecked(piSettings->enable_external_synths);
    system_vol_slider->setValue(piSettings->main_volume);

    osc_server_enabled_check->setChecked(piSettings->osc_server_enabled);
    if(piSettings->osc_server_enabled){
      osc_public_check->show();
    } else {
      osc_public_check->hide();
    }
    osc_public_check->setChecked(piSettings->osc_server_enabled && piSettings->osc_public);
    midi_default_channel_combo->setCurrentIndex(piSettings->midi_default_channel);
    piSettings->midi_default_channel_str = midi_default_channel_combo->currentText(); // TODO find a more elegant solution
    midi_enable_check->setChecked(piSettings->midi_enabled);
    gamepad_enable_check->setChecked(piSettings->gamepad_enabled);

    auto_indent_on_run->setChecked(piSettings->auto_indent_on_run);

    show_line_numbers->setChecked(piSettings->show_line_numbers);
    show_log->setChecked(piSettings->show_log);
    show_cues->setChecked(piSettings->show_cues);
    show_metro->setChecked(piSettings->show_metro);
    show_buttons->setChecked(piSettings->show_buttons);
    show_editor_toolbar->setChecked(piSettings->show_editor_toolbar);
    show_tabs->setChecked(piSettings->show_tabs);
    full_screen->setChecked(piSettings->full_screen);
    log_synths->setChecked(piSettings->log_synths);
    clear_output_on_run->setChecked(piSettings->clear_output_on_run);
    log_cues->setChecked(piSettings->log_cues);
    log_auto_scroll->setChecked(piSettings->log_auto_scroll);
    gui_transparency_slider->setValue(piSettings->gui_transparency);
    const SonicPiTheme::ColourScheme scheme = piSettings->colourScheme;
    lightModeCheck->setChecked( scheme == SonicPiTheme::LightScheme );
    darkModeCheck->setChecked( scheme == SonicPiTheme::DarkScheme );
    highContrastModeCheck->setChecked( scheme == SonicPiTheme::HighContrastScheme );
    mildModeCheck->setChecked( scheme == SonicPiTheme::MildDarkScheme );
    phosphorModeCheck->setChecked( scheme == SonicPiTheme::PhosphorScheme );
    signalModeCheck->setChecked( scheme == SonicPiTheme::SignalScheme );
    proIconsCheck->setChecked( piSettings->proIcons );
    if (m_hueDial) { QSignalBlocker hb(m_hueDial); m_hueDial->setValue(piSettings->hue_rotation); }
    if (monochromeCheck) monochromeCheck->setChecked(piSettings->monochrome);
    if (invertCheck) invertCheck->setChecked(piSettings->invert_colours);

    show_scopes->setChecked(piSettings->show_scopes);
    show_scope_labels->setChecked(piSettings->show_scope_labels);
    show_titles->setChecked(piSettings->show_titles);
    hide_menubar_in_fullscreen->setChecked(piSettings->hide_menubar_in_fullscreen);

    check_updates->setChecked(piSettings->check_updates);
    show_autocompletion->setChecked(piSettings->show_autocompletion);
    show_completion_help->setChecked(piSettings->show_completion_help);
    show_context->setChecked(piSettings->show_context);
    flash_code->setChecked(piSettings->flash_code);
    { QSignalBlocker fb(flash_brightness_slider); flash_brightness_slider->setValue(piSettings->flash_brightness); }
    flash_gutter->setChecked(piSettings->flash_gutter);
    show_loop_scopes->setChecked(piSettings->show_loop_scopes);
    loop_scope_scroll->setChecked(piSettings->loop_scope_scroll);
    speak_transport->setChecked(piSettings->speak_transport);
    reduce_motion->setChecked(piSettings->reduce_motion);
#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    // setChecked emits toggled, not idClicked, so this doesn't echo
    // back to recordingTypeChanged.
    if (piSettings->recording_type == SonicPiSettings::AudioAndVideo) {
        recording_type_av_radio->setChecked(true);
    } else {
        recording_type_audio_radio->setChecked(true);
    }
#endif
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
    connect(enable_scsynth_inputs, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(enable_scsynth_inputs, SIGNAL(clicked()), this, SLOT(updateEnableScsynthInputs()));

    connect(midi_default_channel_combo, SIGNAL(currentIndexChanged(int)), this, SLOT(updateSettings()));
    connect(midi_enable_check, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(osc_server_enabled_check, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(osc_public_check, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(midi_enable_check, SIGNAL(clicked()), this, SLOT(toggleMidi()));
    connect(gamepad_enable_check, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(gamepad_enable_check, SIGNAL(clicked()), this, SLOT(toggleGamepad()));
    connect(osc_server_enabled_check, SIGNAL(clicked()), this, SLOT(toggleOscServer()));
    connect(osc_public_check, SIGNAL(clicked()), this, SLOT(toggleOscServer()));

    connect(auto_indent_on_run, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_line_numbers, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_log, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_cues, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_metro, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_buttons, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_editor_toolbar, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_tabs, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(full_screen, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(log_synths, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(clear_output_on_run, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(log_cues, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(log_auto_scroll, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(lightModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(darkModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(highContrastModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(mildModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(phosphorModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(signalModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(proIconsCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(monochromeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(invertCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(gui_transparency_slider, SIGNAL(valueChanged(int)), this, SLOT(updateSettings()));

    connect(show_autocompletion, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_completion_help, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_context, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(flash_code, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(flash_code, SIGNAL(clicked()), this, SLOT(flashOnPlay()));
    connect(flash_gutter, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(flash_gutter, SIGNAL(clicked()), this, SLOT(flashOnPlay()));
    connect(show_loop_scopes, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_loop_scopes, SIGNAL(clicked()), this, SLOT(flashOnPlay()));
    connect(loop_scope_scroll, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(loop_scope_scroll, SIGNAL(clicked()), this, SLOT(flashOnPlay()));
    connect(flash_brightness_slider, SIGNAL(valueChanged(int)), this, SLOT(updateSettings()));
    connect(flash_brightness_slider, SIGNAL(valueChanged(int)), this, SLOT(flashOnPlay()));
    connect(speak_transport, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(speak_transport, SIGNAL(clicked()), this, SLOT(speakTransport()));
    connect(reduce_motion, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(reduce_motion, SIGNAL(clicked()), this, SLOT(reduceMotion()));

    connect(show_line_numbers, SIGNAL(clicked()), this, SLOT(toggleLineNumbers()));
    connect(show_log, SIGNAL(clicked()), this, SLOT(toggleLog()));
    connect(show_cues, SIGNAL(clicked()), this, SLOT(toggleCuesLog()));
    connect(show_metro, SIGNAL(clicked()), this, SLOT(toggleMetro()));
    connect(show_buttons, SIGNAL(clicked()), this, SLOT(toggleButtons()));
    connect(show_editor_toolbar, SIGNAL(clicked()), this, SLOT(toggleEditorToolbar()));
    connect(full_screen, SIGNAL(clicked()), this, SLOT(toggleFullScreen()));
    connect(show_tabs, SIGNAL(clicked()), this, SLOT(toggleTabs()));
    connect(log_auto_scroll, SIGNAL(clicked()), this, SLOT(toggleLogAutoScroll()));
    connect(lightModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(darkModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(highContrastModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(mildModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(phosphorModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(signalModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(proIconsCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(monochromeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(invertCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(gui_transparency_slider, SIGNAL(valueChanged(int)), this, SLOT(updateTransparency(int)));

    connect(show_scope_labels, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_scopes, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_scope_labels, SIGNAL(clicked()), this, SLOT(toggleScopeLabels()));
    connect(show_titles, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_titles, SIGNAL(clicked()), this, SLOT(toggleTitles()));
    connect(hide_menubar_in_fullscreen, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(hide_menubar_in_fullscreen, SIGNAL(clicked()), this, SLOT(toggleHideMenuBarInFullscreen()));
    connect(show_scopes, SIGNAL(clicked()), this, SLOT(toggleScope()));

    connect(check_updates, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(check_updates, SIGNAL(clicked()), this, SLOT(toggleCheckUpdates()));
    connect(visit_sonic_pi_net, SIGNAL(clicked()), this, SLOT(openSonicPiNet()));
    connect(check_updates_now, SIGNAL(clicked()), this, SLOT(checkForUpdatesNow()));

    connect(show_autocompletion, SIGNAL(clicked()), this, SLOT(showAutoCompletion()));
    connect(show_completion_help, SIGNAL(clicked()), this, SLOT(showCompletionHelp()));
    connect(show_context, SIGNAL(clicked()), this, SLOT(showContext()));
    connect(check_args, SIGNAL(clicked()), this, SLOT(checkArgs()));
    connect(synth_trigger_timing_guarantees_cb, SIGNAL(clicked()), this, SLOT(synthTriggerTimingGuarantees()));
    connect(enable_external_synths_cb, SIGNAL(clicked()), this, SLOT(enableExternalSynths()));
    connect(midi_default_channel_combo, SIGNAL(currentIndexChanged(int)), this, SLOT(midiDefaultChannel()));
    connect(log_cues, SIGNAL(clicked()), this, SLOT(logCues()));
    connect(log_synths, SIGNAL(clicked()), this, SLOT(logSynths()));
    connect(clear_output_on_run, SIGNAL(clicked()), this, SLOT(clearOutputOnRun()));
    connect(auto_indent_on_run, SIGNAL(clicked()), this, SLOT(autoIndentOnRun()));

    // Prefs checkboxes: focus only via keyboard (Tab), not a mouse click. A click
    // still toggles the box, but no longer grabs focus and draws the blue focus
    // ring around the whole row — which read as a pointless "selection". Keyboard
    // users keep the focus ring (and Space to toggle) for accessibility.
    for (QCheckBox* cb : findChildren<QCheckBox*>()) {
        cb->setFocusPolicy(Qt::TabFocus);
    }
    // Watch focus application-wide so that checkboxes created after startup
    // (scope kinds, MIDI/OSC device rows) get the keyboard focus ring too
    // (see eventFilter). Installed once: connectAll() runs only from the
    // constructor, and SettingsWidget is constructed once.
    qApp->installEventFilter(this);
}

bool SettingsWidget::eventFilter(QObject* obj, QEvent* event)
{
    // The checkbox focus ring (QCheckBox[kbFocus="true"] in app.qss) reads as a
    // pointless "selection box" when a mouse click draws it. Show it ONLY for
    // keyboard (Tab) focus: flip the kbFocus property by focus reason and repolish.
    // The toggle-on-click is unaffected. This filter is installed on qApp so it
    // covers every checkbox, including ones created after startup.
    QCheckBox* cb = qobject_cast<QCheckBox*>(obj);
    if (cb && (event->type() == QEvent::FocusIn || event->type() == QEvent::FocusOut)) {
        const bool kb = event->type() == QEvent::FocusIn
            && [event]() { const Qt::FocusReason r = static_cast<QFocusEvent*>(event)->reason();
                           return r == Qt::TabFocusReason || r == Qt::BacktabFocusReason; }();
        if (cb->property("kbFocus").toBool() != kb) {
            cb->setProperty("kbFocus", kb);
            cb->style()->unpolish(cb);
            cb->style()->polish(cb);
            cb->update();
        }
    }
    return QWidget::eventFilter(obj, event);
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
