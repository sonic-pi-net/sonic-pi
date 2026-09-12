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

#include "trackspanel.h"

#include <QButtonGroup>
#include <QComboBox>
#include <QDialog>
#include <QDialogButtonBox>
#include <QDir>
#include <QEvent>
#include <QFileDialog>
#include <QFileInfo>
#include <QFont>
#include <QFontMetrics>
#include <QFontMetricsF>
#include <QtMath>
#include <QFrame>
#include <QGraphicsOpacityEffect>
#include <QGridLayout>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QPointer>
#include <QPushButton>
#include <QRegularExpression>
#include <QScrollArea>
#include <QSettings>
#include <QSignalBlocker>
#include <QStyle>
#include <QTimer>
#include <QVBoxLayout>

#include "api/osc/osc_pkt.hh"
#include "api/sonicpi_api.h"
#include "dpi.h"
#include "model/sonicpitheme.h"
#include "utils/fontroles.h"
#include "utils/tablericons.h"
#include "widgets/cardscope.h"      // the quickstart cards' ring scope
#include "widgets/thinsplitter.h"
#include "utils/tutorialdocs.h"        // highlightCode — the editor's colours
#include "widgets/tutorialwidgets.h"   // TutDial — the synth docs' own rotary

#ifdef Q_OS_WIN
extern "C" __declspec(dllimport) int __stdcall AllowSetForegroundWindow(unsigned long processId);
static void allowForegroundToAnyProcess() { AllowSetForegroundWindow(0xFFFFFFFFul /* ASFW_ANY */); }
#endif

namespace
{
// A device's face. Wide enough for four dials across, which is the width at
// which a name fits under an arc without wrapping to three lines, and short
// enough that several devices are visible at once — the entire point of a lane.
constexpr int kDialsPerRow = 4;
constexpr int kDeviceWidth = 300;

// A track's name is typed in code as a symbol, so it is a symbol: a letter or
// underscore, then letters, digits and underscores. Anything else would need
// quoting and `live_track :"my track"` is not a thing anyone should write.
const QRegularExpression& symbolShape()
{
    static const QRegularExpression re(QStringLiteral("^[A-Za-z_][A-Za-z0-9_]*$"));
    return re;
}

// Where a curated set is remembered. Keyed on the plugin's path and index: a
// handle is a runtime name and does not survive a restart, and the plugin's
// own id is not in the list the engine sends.
QString curationKey(const SonicPi::TrackNodeInfo& node)
{
    QString k = QString::fromStdString(node.path);
    k.replace('/', '_');
    k.replace('\\', '_');
    return QStringLiteral("tracks/curated/%1#%2").arg(k).arg(node.index);
}

QSettings panelSettings()
{
    return QSettings(QSettings::IniFormat, QSettings::UserScope,
                     QStringLiteral("sonic-pi.net"), QStringLiteral("tracks-panel"));
}

// A path the engine will open. Both processes are on this machine, so the
// native form is the one to hand over: it is what a user pasted or picked.
QString enginePath(const QString& path)
{
    return QDir::toNativeSeparators(QDir::cleanPath(path));
}

} // namespace

// ── The panel ────────────────────────────────────────────────────────────────

TracksPanel::TracksPanel(std::shared_ptr<SonicPi::SonicPiAPI> spAPI, QWidget* parent)
    : QWidget(parent)
    , m_spAPI(spAPI)
{
    // A plain QWidget does not paint its stylesheet background unless told
    // to; the TracksPanel rule in app.qss is what gives the pane its ground.
    setAttribute(Qt::WA_StyledBackground, true);
    buildUi();
    // The window builds this panel before the engine is up, so anything sent
    // from here would land on nothing: the window calls onEngineReady() once
    // there is an engine to ask. Until then the browser shows what the last
    // scan found — plugins are installed far less often than Sonic Pi is
    // opened, and a browser that is empty for the first five seconds of
    // every session teaches people not to look at it.
    recallPlugins();
}

void TracksPanel::onEngineReady()
{
    // The engine answers `list` with a broadcast, which is how this panel
    // learns what already exists — it may have been running for an hour
    // before this tab was opened, or have just come back from a device
    // switch with no tracks at all (onTracks restores the rig in that case).
    // The folders come the same way, and the next scan searches them too.
    if (!m_spAPI) return;
    m_engineUp = true;
    // A new engine gets one restore attempt; see onTracks.
    m_restoreTried = false;
    m_spAPI->SupersonicSendOSC(oscpkt::Message("/clockwork/track/folders"));
    m_spAPI->SupersonicSendOSC(oscpkt::Message("/clockwork/track/list"));
    // The folders arriving is what starts the first scan (onTrackFolders);
    // a Rescan pressed before now happens as soon as it can.
    if (m_scanWanted || m_foundIsRecalled) { m_scanWanted = false; onRescan(); }
}

TracksPanel::~TracksPanel()
{
    saveCurrentRig();
    for (Device* d : m_devices) delete d;
    m_devices.clear();
}

// THE HOUSE PILL, for every button here that carries a word. app.qss draws
// #qsDeckPill as the outlined pill the docs pane's section tabs and the Cards
// deck bar are made of, filled when its "current" property is true; a
// bare QPushButton is the black dialog button and reads as a different
// application. Built here rather than styled here — the look stays in the
// stylesheet, where the themes can reach it.
static QPushButton* housePill(const QString& text, QWidget* parent, double fontScale = 1.0)
{
    auto* b = new QPushButton(text, parent);
    b->setObjectName(QStringLiteral("qsDeckPill"));
    b->setProperty("current", false);
    b->setCursor(Qt::PointingHandCursor);
    b->setAccessibleName(text);
    // The pane's text zoom, the way the docs and cards pills carry theirs.
    b->setStyleSheet(QStringLiteral("font-size: %1px;").arg(FontRolePx(FontRole::Base, fontScale)));
    return b;
}

void TracksPanel::buildUi()
{
    auto* outer = new QHBoxLayout(this);
    outer->setContentsMargins(0, 0, 0, 0);
    outer->setSpacing(0);

    // THE DOCS PANE'S DIVIDER between the columns: the same ThinSplitter, a
    // thin centre line at rest that fills on hover and drags. Three panes
    // rather than the docs' two, because the tracks are a column of their
    // own; the chain takes whatever the columns leave.
    m_split = new ThinSplitter(Qt::Horizontal, this);
    m_split->setHandleWidth(7);
    m_split->setChildrenCollapsible(false);
    m_split->addWidget(buildBrowserColumn());
    m_split->addWidget(buildTracksColumn());
    m_split->addWidget(buildChainArea());
    m_split->setStretchFactor(0, 0);
    m_split->setStretchFactor(1, 0);
    m_split->setStretchFactor(2, 1);
    outer->addWidget(m_split);

    const QByteArray state = panelSettings().value(QStringLiteral("ui/split")).toByteArray();
    if (state.isEmpty() || !m_split->restoreState(state))
        m_split->setSizes({ ScaleWidthForDPI(280), ScaleWidthForDPI(210), ScaleWidthForDPI(1000) });
    connect(m_split, &QSplitter::splitterMoved, this, [this](int, int) {
        panelSettings().setValue(QStringLiteral("ui/split"), m_split->saveState());
    });

    applySizing();
}

// ── Text zoom ────────────────────────────────────────────────────────────────

void TracksPanel::setUserZoom(int zoom)
{
    zoom = qBound(kFontZoomMin, zoom, kFontZoomMax);
    if (zoom == m_userZoom) return;
    m_userZoom = zoom;
    applySizing();
}

/*
 * THE COLUMNS FIT THEIR WORDS. Each has a house width, and grows to what its
 * buttons need under the theme's padding and font — "Instruments" beside
 * "Effects", "Save rig…" beside "Load rig…" — rather than clipping a label to
 * "um" and "fec". Measured after the event loop has applied the stylesheet,
 * which is when a button knows its own padding; a translation or a platform
 * font changes the answer, so it is measured rather than guessed. A floor
 * only: the divider lets the columns be dragged wider.
 */
void TracksPanel::fitColumns()
{
    const UiScale ui(m_fontScale);
    const struct { QWidget* host; int base; } cols[] = {
        { m_browserHost, 280 }, { m_tracksHost, 210 },
    };
    for (const auto& c : cols)
    {
        if (!c.host || !c.host->layout()) continue;
        c.host->setMinimumWidth(qMax(ui.x(c.base), c.host->layout()->minimumSize().width()));
    }
}

// Every piece of text under `root`, on its role at the current zoom. The
// size goes on as an inline font-size, the way the docs and cards pills
// carry theirs: app.qss dresses these widgets (#qsCodeLine's Hack,
// #qsCardTitle's weight), and a size in the same cascade sits under those
// rules, where a setFont() would be pushed aside by them or push them aside.
void TracksPanel::applyFontsIn(QWidget* root)
{
    const UiScale ui(m_fontScale);
    auto put = [this](QWidget* w, FontRole role, bool bold = false) {
        w->setStyleSheet(QStringLiteral("font-size: %1px;%2")
                             .arg(FontRolePx(role, m_fontScale))
                             .arg(bold ? QStringLiteral(" font-weight: bold;") : QString()));
    };
    for (QLabel* l : root->findChildren<QLabel*>())
    {
        const QString n = l->objectName();
        if (n == QLatin1String("tracksTitle"))
            put(l, FontRole::Large, true);
        else if (n == QLatin1String("tutSection") || n == QLatin1String("tracksBrowserCount")
                 || n == QLatin1String("tracksDetail") || n == QLatin1String("tracksStatus")
                 || n == QLatin1String("qsCardBlurb"))
            put(l, FontRole::Small);
        else
            put(l, FontRole::Base);
    }
    for (QListWidget* l : root->findChildren<QListWidget*>())
    {
        put(l, FontRole::Base);
        l->setSpacing(ui.y(1));   // re-lays the rows out at the new height
    }
    for (QLineEdit* e : root->findChildren<QLineEdit*>())
        put(e, FontRole::Base);
    for (QComboBox* c : root->findChildren<QComboBox*>())
        put(c, FontRole::Base);
    for (QPushButton* b : root->findChildren<QPushButton*>(QStringLiteral("qsDeckPill")))
        put(b, FontRole::Base);
}

// Every size the panel chooses, at the current zoom.
void TracksPanel::applySizing()
{
    m_fontScale = FontZoomFactor(m_userZoom);
    const UiScale ui(m_fontScale);
    applyFontsIn(this);
    QTimer::singleShot(0, this, [this]() { fitColumns(); });
    m_ampDial->setUiScale(m_fontScale);
    scopeSized();
    for (Device* dev : m_devices)
    {
        fitDeviceTitle(dev);
        for (const ParamRow& r : dev->rows)
            if (r.dial) r.dial->setUiScale(m_fontScale);
    }
    // The vendor captions in the browser carry a font of their own.
    rebuildBrowser();
}

// The ring is the amp dial's knob's size, so the two read as one row of
// round things: what is coming out beside what sets how loud.
void TracksPanel::scopeSized()
{
    if (!m_scope) return;
    const int side = qMax(UiScale(m_fontScale).x(72), ScaleWidthForDPI(72));
    m_scope->setFixedSize(side, side);
}

// A device grows with the text zoom and never shrinks below its house width.
int TracksPanel::deviceWidth() const
{
    return qMax(UiScale(m_fontScale).x(kDeviceWidth), ScaleWidthForDPI(kDeviceWidth));
}

// A device is as wide as its name needs, the way Live sizes a device to its
// title: the house width, or wider when the name and the header's buttons
// want more, up to twice — past that the name ends in an ellipsis and the
// tooltip has it whole. Measured in the header's own font, so a zoom re-fits
// it.
//
// Twice, not half again, because the header's font is the platform's and the
// platforms disagree by a third: bold, at the base size (dpi.h: 18px on macOS,
// 19px elsewhere), "ValhallaSupermassive" is 180px in macOS's San Francisco
// and 235px in DejaVu Sans, Linux's default. With the six header buttons (five
// glyphs at 28px, Configure as wide as its word) that name needs ~440px on
// macOS and ~515px on Linux, and a cap of 450px (half again on the 300px
// house) wrote it out whole on one and elided it on the other.
// trackspanel_look.test.cpp pins the whole name, on every platform CI runs.
void TracksPanel::fitDeviceTitle(Device* dev)
{
    if (!dev || !dev->frame || !dev->title) return;
    const QString name = QString::fromStdString(dev->info.name);
    QFont f = RoleFont(FontRole::Base, m_fontScale);
    f.setBold(true);   // #qsCardTitle is 700
    const QFontMetrics fm(f);
    // The name's width, rounded UP. QFontMetrics::horizontalAdvance rounds to
    // the nearest pixel, but elidedText compares against the exact fractional
    // width — so a name given exactly its rounded-down advance (193.98 fits
    // in 194; 235.4 does not fit in 235) loses its last letter to an ellipsis
    // in the font whose rounding went the other way. Which font that is
    // depends on the platform: it was Linux's DejaVu Sans for
    // "ValhallaSupermassive", and macOS's San Francisco for "Vital".
    const int need = qCeil(QFontMetricsF(f).horizontalAdvance(name));
    int chrome = ScaleWidthForDPI(10) + ScaleWidthForDPI(6);
    for (QPushButton* b : { dev->bypassBtn, dev->editorBtn, dev->configureBtn, dev->leftBtn,
                            dev->rightBtn, dev->removeBtn })
        // A glyph button is fixed; Configure is as wide as its word.
        if (b) chrome += (b->minimumWidth() == b->maximumWidth()) ? b->minimumWidth()
                                                                   : b->sizeHint().width();
    const int house = deviceWidth();
    const int most = house * 2;
    const int width = qBound(house, chrome + need, most);
    dev->frame->setFixedWidth(width);
    dev->title->setText(fm.elidedText(name, Qt::ElideRight, width - chrome));
}

QWidget* TracksPanel::buildBrowserColumn()
{
    /*
     * THE HOUSE NAV COLUMN, not a third list style.
     *
     * The docs pane already has this exact widget — a filter field over a list,
     * on a tinted sidebar — and app.qss describes it under #docsNavPage,
     * #docsFilter and #docsNavList. These are the same object names, so the
     * browser IS the docs nav rather than something that resembles it, and it
     * follows every theme without anyone remembering to check.
     */
    m_filter = new QLineEdit(this);
    m_filter->setObjectName(QStringLiteral("docsFilter"));
    m_filter->setPlaceholderText(tr("Filter plugins..."));
    m_filter->setAccessibleName(tr("Filter plugins"));
    m_filter->setClearButtonEnabled(true);
    connect(m_filter, &QLineEdit::textChanged, this, &TracksPanel::onFilterChanged);

    // WHAT A PLUGIN IS FOR, as a filter rather than a mark on every row. Live
    // and Bitwig keep instruments and effects in separate browser folders;
    // the vendor grouping is worth more here, so the split is a row of the
    // house pills above the list — the same pills the docs pane's Tutorial /
    // Examples / Synths selector is made of — and the rows stay clean.
    auto* kinds = new QWidget(this);
    auto* kindsLayout = new QHBoxLayout(kinds);
    kindsLayout->setContentsMargins(0, 0, 0, 0);
    kindsLayout->setSpacing(ScaleWidthForDPI(4));
    m_kindFilter = new QButtonGroup(this);
    m_kindFilter->setExclusive(true);
    const struct { const char* label; const char* tip; int id; } kindDefs[] = {
        { QT_TR_NOOP("All"),         QT_TR_NOOP("Every plugin found."), 0 },
        { QT_TR_NOOP("Instruments"), QT_TR_NOOP("Plugins that take notes and make sound."), 1 },
        { QT_TR_NOOP("Effects"),     QT_TR_NOOP("Plugins that transform what is sent to them."), 2 },
    };
    for (const auto& k : kindDefs)
    {
        auto* b = housePill(tr(k.label), kinds);
        b->setCheckable(true);
        b->setToolTip(tr(k.tip));
        b->setChecked(k.id == 0);
        b->setProperty("current", k.id == 0);
        kindsLayout->addWidget(b, 1);
        m_kindFilter->addButton(b, k.id);
    }
    // The pill shows "current" through a property the stylesheet reads, so
    // the checked state has to be mirrored into it, and re-polished to land.
    connect(m_kindFilter, &QButtonGroup::idClicked, this, [this](int) {
        for (QAbstractButton* b : m_kindFilter->buttons())
        {
            if (b->property("current").toBool() == b->isChecked()) continue;
            b->setProperty("current", b->isChecked());
            b->style()->unpolish(b);
            b->style()->polish(b);
        }
    });
    connect(m_kindFilter, &QButtonGroup::idClicked, this, &TracksPanel::onFilterChanged);

    m_browser = new QListWidget(this);
    m_browser->setObjectName(QStringLiteral("docsNavList"));
    m_browser->setFrameShape(QFrame::NoFrame);
    m_browser->setAccessibleName(tr("Installed plugins"));
    m_browser->setSpacing(ScaleHeightForDPI(1));
    m_browser->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    m_browser->setToolTip(tr("Double-click a plugin to put it on the selected track."));
    // itemActivated ONLY: it covers a double-click and Return, and adding
    // itemDoubleClicked as well fires both for one gesture.
    connect(m_browser, &QListWidget::itemActivated, this, &TracksPanel::onAddSelectedPlugin);

    // AN EXPLICIT ADD, alongside the double-click. Live, Bitwig and Reaper all
    // take a double-click, none make it the only way in, and a gesture with no
    // visible control is not discoverable.
    m_addButton = housePill(tr("+ Add to track"), this);
    m_addButton->setEnabled(false);
    m_addButton->setToolTip(tr("Put the selected plugin at the end of the selected track's chain. "
                               "Double-clicking a row does the same."));
    connect(m_addButton, &QPushButton::clicked, this, &TracksPanel::onAddSelectedPlugin);
    connect(m_browser, &QListWidget::currentItemChanged, this,
            [this](QListWidgetItem* cur, QListWidgetItem*) {
                m_addButton->setEnabled(cur && cur->data(Qt::UserRole).isValid()
                                        && selectedTrack() != nullptr);
            });

    m_browserCount = new QLabel(tr("looking..."), this);
    m_browserCount->setObjectName(QStringLiteral("tracksBrowserCount"));

    m_rescanButton = housePill(tr("Rescan"), this);
    m_rescanButton->setToolTip(tr("Look through the plugin folders again. "
                                  "This opens every plugin it finds, so it takes a moment."));
    connect(m_rescanButton, &QPushButton::clicked, this, &TracksPanel::onRescan);

    // Discovery is not reliable enough to be the only way in. A plugin in a
    // vendor's own folder, a build tree, or a Windows install that ignored
    // Common Files is invisible to the standard search, and a host with no
    // way to say "look here too" has decided those plugins do not exist.
    m_foldersButton = housePill(tr("Plugin folders…"), this);
    m_foldersButton->setToolTip(tr("Where plugins are looked for, and folders of your own to add."));
    connect(m_foldersButton, &QPushButton::clicked, this, &TracksPanel::onFolders);

    // Captioned like the tracks column beside it, so the two read as a pair.
    auto* caption = new QLabel(tr("PLUGINS"), this);
    caption->setObjectName(QStringLiteral("tutSection"));
    m_browserCaption = caption;

    auto* col = new QVBoxLayout();
    col->setContentsMargins(ScaleWidthForDPI(8), ScaleHeightForDPI(12),
                            ScaleWidthForDPI(4), ScaleHeightForDPI(8));
    col->setSpacing(ScaleHeightForDPI(6));
    col->addWidget(caption);
    col->addWidget(m_filter);
    col->addWidget(kinds);
    col->addWidget(m_browser, 1);
    col->addWidget(m_addButton);
    col->addWidget(m_browserCount);
    auto* tools = new QHBoxLayout();
    tools->setContentsMargins(0, 0, 0, 0);
    tools->setSpacing(ScaleWidthForDPI(6));
    tools->addWidget(m_rescanButton);
    tools->addWidget(m_foldersButton);
    col->addLayout(tools);

    auto* host = new QWidget(this);
    host->setObjectName(QStringLiteral("docsNavPage"));
    // Without this the #docsNavPage tint is never painted: a plain QWidget does
    // not draw a stylesheet background unless it is told to.
    host->setAttribute(Qt::WA_StyledBackground, true);
    host->setLayout(col);
    m_browserHost = host;
    return host;
}

QWidget* TracksPanel::buildTracksColumn()
{
    /*
     * THE TRACKS, as a second nav column. Same tint, same list, same pill
     * rows; a track row renames in place on double-click, the way a track's
     * name field does in Live. This is the list the code's symbols come from.
     */
    m_trackList = new QListWidget(this);
    m_trackList->setObjectName(QStringLiteral("docsNavList"));
    m_trackList->setFrameShape(QFrame::NoFrame);
    m_trackList->setAccessibleName(tr("Tracks"));
    m_trackList->setSpacing(ScaleHeightForDPI(1));
    m_trackList->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    m_trackList->setEditTriggers(QAbstractItemView::DoubleClicked | QAbstractItemView::EditKeyPressed);
    m_trackList->setToolTip(tr("Select a track to see its chain. Double-click a name to change it."));
    connect(m_trackList, &QListWidget::currentItemChanged, this,
            [this](QListWidgetItem*, QListWidgetItem*) { onTrackSelectionChanged(); });
    connect(m_trackList, &QListWidget::itemChanged, this, &TracksPanel::onTrackItemEdited);

    m_newTrackButton = housePill(tr("+ New track"), this);
    m_newTrackButton->setToolTip(tr("Make a track. It gets a name you can change, "
                                    "and code reaches it by that name."));
    connect(m_newTrackButton, &QPushButton::clicked, this, &TracksPanel::onNewTrack);

    // A RIG is every track, its chain, each plugin's state and the extra
    // folders — a studio you can carry between sessions or machines. The
    // current one is kept automatically; these are for keeping more than one.
    m_saveRigButton = housePill(tr("Save rig…"), this);
    m_saveRigButton->setToolTip(tr("Write every track and its plugins to a file."));
    connect(m_saveRigButton, &QPushButton::clicked, this, &TracksPanel::onSaveRig);
    m_loadRigButton = housePill(tr("Load rig…"), this);
    m_loadRigButton->setToolTip(tr("Replace the tracks with ones from a file."));
    connect(m_loadRigButton, &QPushButton::clicked, this, &TracksPanel::onLoadRig);

    auto* caption = new QLabel(tr("TRACKS"), this);
    caption->setObjectName(QStringLiteral("tutSection"));
    m_tracksCaption = caption;

    auto* col = new QVBoxLayout();
    col->setContentsMargins(ScaleWidthForDPI(8), ScaleHeightForDPI(12),
                            ScaleWidthForDPI(4), ScaleHeightForDPI(8));
    col->setSpacing(ScaleHeightForDPI(6));
    col->addWidget(caption);
    col->addWidget(m_trackList, 1);
    col->addWidget(m_newTrackButton);
    auto* rig = new QHBoxLayout();
    rig->setContentsMargins(0, 0, 0, 0);
    rig->setSpacing(ScaleWidthForDPI(6));
    rig->addWidget(m_saveRigButton);
    rig->addWidget(m_loadRigButton);
    col->addLayout(rig);

    auto* host = new QWidget(this);
    host->setObjectName(QStringLiteral("docsNavPage"));
    host->setAttribute(Qt::WA_StyledBackground, true);
    host->setLayout(col);
    m_tracksHost = host;
    return host;
}

QWidget* TracksPanel::buildChainArea()
{
    auto* area = new QWidget(this);
    auto* right = new QVBoxLayout(area);
    right->setContentsMargins(ScaleWidthForDPI(12), ScaleHeightForDPI(10),
                              ScaleWidthForDPI(12), ScaleHeightForDPI(8));
    right->setSpacing(ScaleHeightForDPI(8));

    // ── The track's header: name, level, mute ────────────────────────────────
    m_header = new QWidget(area);
    auto* hh = new QHBoxLayout(m_header);
    hh->setContentsMargins(0, 0, 0, 0);
    hh->setSpacing(ScaleWidthForDPI(12));

    // The track's return, live, drawn the way a quickstart card draws its
    // run: the ring scope, sitting where the card's does — ahead of the name.
    // It taps the scope stream the engine keeps for this track's return, so
    // it shows what the track is putting out and nothing else.
    m_scope = new CardScope(m_header);
    m_scope->setObjectName(QStringLiteral("tracksScope"));
    m_scope->setDrive(4.0, 0.06);   // a small ring on one track's return
    m_scope->setAttribute(Qt::WA_TransparentForMouseEvents, true);
    scopeSized();
    hh->addWidget(m_scope);

    auto* titles = new QVBoxLayout();
    titles->setContentsMargins(0, 0, 0, 0);
    titles->setSpacing(0);
    m_trackTitle = new QLabel(m_header);
    m_trackTitle->setObjectName(QStringLiteral("tracksTitle"));   // font: applySizing
    m_trackDetail = new QLabel(m_header);
    m_trackDetail->setObjectName(QStringLiteral("tracksDetail"));
    titles->addWidget(m_trackTitle);
    titles->addWidget(m_trackDetail);
    hh->addLayout(titles, 1);

    // The track's level, as the synth docs draw amp: the same dial, the same
    // range. It is the gain applied after the chain, before the return lane.
    m_ampDial = new TutDial(QStringLiteral("amp"), 0.0, 2.0, 1.0, [this]() {
        const SonicPi::TrackInfo* t = selectedTrack();
        if (!t || !m_spAPI) return;
        oscpkt::Message m("/clockwork/track/gain");
        m.pushInt32(t->id);
        m.pushFloat(static_cast<float>(m_ampDial->value()));
        m_spAPI->SupersonicSendOSC(m);
    }, m_header);
    m_ampDial->setUiScale(m_fontScale);
    m_ampDial->setDocText(tr("The track's level after its chain. 1 is unity; "
                             "double-click to return there."));
    hh->addWidget(m_ampDial);

    // Mute is the house pill in its "current" state while it is on: the same
    // fill the selected docs tab has, which is what an engaged toggle looks
    // like everywhere else in the window.
    m_muteButton = housePill(tr("Mute"), m_header);
    m_muteButton->setCheckable(true);
    m_muteButton->setToolTip(tr("Silence the track's return without stopping anything on it."));
    connect(m_muteButton, &QPushButton::toggled, this, [this](bool on) {
        if (m_muteButton->property("current").toBool() == on) return;
        m_muteButton->setProperty("current", on);
        m_muteButton->style()->unpolish(m_muteButton);
        m_muteButton->style()->polish(m_muteButton);
    });
    connect(m_muteButton, &QPushButton::clicked, this, [this](bool on) {
        const SonicPi::TrackInfo* t = selectedTrack();
        if (!t || !m_spAPI) return;
        oscpkt::Message m("/clockwork/track/mute");
        m.pushInt32(t->id);
        m.pushInt32(on ? 1 : 0);
        m_spAPI->SupersonicSendOSC(m);
    });
    hh->addWidget(m_muteButton);

    m_removeTrackButton = housePill(tr("Remove track"), m_header);
    m_removeTrackButton->setToolTip(tr("Take this track and everything on it out of the studio. "
                                       "Code that names it will say it is unknown."));
    connect(m_removeTrackButton, &QPushButton::clicked, this, &TracksPanel::onRemoveTrack);
    hh->addWidget(m_removeTrackButton);
    right->addWidget(m_header);

    // ── How code reaches the track ───────────────────────────────────────────
    //
    // The three lines that matter, in the editor's own colours, with copy and
    // insert beside each: the panel teaches the language the way the cards do.
    // In a #tutDialGroup — the synth docs' quiet region — rather than a card,
    // because a card has a title bar and these are not a card's worth.
    auto* codeFrame = new QFrame(area);
    codeFrame->setObjectName(QStringLiteral("tutDialGroup"));
    m_codeBlock = codeFrame;
    auto* codeV = new QVBoxLayout(codeFrame);
    codeV->setContentsMargins(ScaleWidthForDPI(12), ScaleHeightForDPI(8),
                              ScaleWidthForDPI(8), ScaleHeightForDPI(8));
    codeV->setSpacing(ScaleHeightForDPI(2));

    auto codeRow = [this, codeFrame, codeV](QLabel*& label, const QString& what) -> QWidget* {
        auto* row = new QWidget(codeFrame);
        auto* rh = new QHBoxLayout(row);
        rh->setContentsMargins(0, 0, 0, 0);
        rh->setSpacing(ScaleWidthForDPI(2));
        label = new QLabel(row);
        label->setObjectName(QStringLiteral("qsCodeLine"));
        label->setTextFormat(Qt::RichText);
        label->setTextInteractionFlags(Qt::NoTextInteraction);
        rh->addWidget(label, 1);

        const int side = ScaleWidthForDPI(24);
        auto* insert = new QPushButton(row);
        insert->setObjectName(QStringLiteral("qsCardBtn"));
        insert->setCursor(Qt::PointingHandCursor);
        insert->setFixedSize(side, side);
        insert->setIconSize(QSize(ScaleWidthForDPI(16), ScaleWidthForDPI(16)));
        insert->setAccessibleName(tr("Add %1 to the editor at the cursor").arg(what));
        insert->setToolTip(tr("Add this line to your code at the cursor."));
        insert->setProperty("glyph", static_cast<int>(TablerIcons::Glyph::SquareChevronsUp));
        connect(insert, &QPushButton::clicked, this, [this, label, what]() {
            emit insertRequested(what, label->accessibleName() + QStringLiteral("\n"));
        });
        auto* copy = new QPushButton(row);
        copy->setObjectName(QStringLiteral("qsCardBtn"));
        copy->setCursor(Qt::PointingHandCursor);
        copy->setFixedSize(side, side);
        copy->setIconSize(QSize(ScaleWidthForDPI(16), ScaleWidthForDPI(16)));
        copy->setAccessibleName(tr("Copy %1 to the clipboard").arg(what));
        copy->setToolTip(tr("Copy this line to the clipboard."));
        copy->setProperty("glyph", static_cast<int>(TablerIcons::Glyph::Copy));
        connect(copy, &QPushButton::clicked, this, [this, label, what, copy]() {
            emit copyRequested(what, label->accessibleName());
            // The glyph flashes to a tick so the copy visibly registered.
            if (!m_theme) return;
            const QColor ink = m_theme->color("Foreground");
            copy->setIcon(TablerIcons::icon(TablerIcons::Glyph::Check, ink,
                                            ScaleWidthForDPI(16), devicePixelRatioF()));
            QPointer<QPushButton> alive(copy);
            QTimer::singleShot(1400, this, [this, alive, ink]() {
                if (!alive) return;
                alive->setIcon(TablerIcons::icon(TablerIcons::Glyph::Copy, ink,
                                                 ScaleWidthForDPI(16), devicePixelRatioF()));
            });
        });
        rh->addWidget(insert);
        rh->addWidget(copy);
        codeV->addWidget(row);
        return row;
    };

    // In the order they are reached for: a track is heard as soon as it
    // exists, so the lines that PLAY it come first and the routing last.
    m_codeNoteRow = codeRow(m_codeNote, tr("track_midi"));
    m_codeSendRow = codeRow(m_codeSend, tr("with_send"));
    codeRow(m_codeLive, tr("live_track"));
    right->addWidget(codeFrame);

    // ── The chain ────────────────────────────────────────────────────────────
    m_emptyLane = new QLabel(tr("Nothing on this track yet.\n"
                                "Double-click a plugin in the browser to put it here."), area);
    m_emptyLane->setObjectName(QStringLiteral("tracksEmptyLane"));
    m_emptyLane->setAlignment(Qt::AlignCenter);
    right->addWidget(m_emptyLane, 1);

    m_laneScroll = new QScrollArea(area);
    m_laneScroll->setWidgetResizable(true);
    m_laneScroll->setFrameShape(QFrame::NoFrame);
    // Horizontal, because that is the axis the chain runs along. A lane that
    // wrapped would put the third plugin under the first and lose the ordering
    // it exists to show.
    m_laneScroll->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    m_laneScroll->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    auto* laneHost = new QWidget(m_laneScroll);
    m_lane = new QHBoxLayout(laneHost);
    m_lane->setContentsMargins(0, 0, 0, 0);
    m_lane->setSpacing(ScaleWidthForDPI(8));
    m_lane->addStretch(1);      // devices are inserted before this
    m_laneScroll->setWidget(laneHost);
    right->addWidget(m_laneScroll, 1);

    // ── No tracks at all ─────────────────────────────────────────────────────
    m_noTracks = new QLabel(tr("No tracks yet.\n\n"
                               "Make one with + New track, put a plugin on it,\n"
                               "then play it from code with track_midi or with_send."), area);
    m_noTracks->setObjectName(QStringLiteral("tracksEmptyLane"));
    m_noTracks->setAlignment(Qt::AlignCenter);
    right->addWidget(m_noTracks, 1);

    // ── What just happened ───────────────────────────────────────────────────
    m_status = new QLabel(area);
    m_status->setObjectName(QStringLiteral("tracksStatus"));
    m_statusTimer = new QTimer(this);
    m_statusTimer->setSingleShot(true);
    connect(m_statusTimer, &QTimer::timeout, this, [this] { if (m_status) m_status->clear(); });
    m_status->setWordWrap(true);
    right->addWidget(m_status);

    showTrack(0);
    return area;
}

// ── Discovery ────────────────────────────────────────────────────────────────

void TracksPanel::onRescan()
{
    if (m_scanning) return;
    if (!m_engineUp || !m_spAPI)
    {
        m_scanWanted = true;     // done the moment there is an engine
        return;
    }
    // THE ENGINE'S PLUGIN PROCESS SCANS, and this window never opens a
    // plugin. A scan runs each plugin's own initialisation, seconds of
    // foreign code; done here it would freeze the window and, when one of
    // them crashed, take the editor and the unsaved buffers with it. Done
    // there, a crash costs the plugin process, which the engine restarts
    // with its tracks — and the error names the plugin that did it.
    m_scanning = true;
    m_rescanButton->setEnabled(false);
    m_browserCount->setText(tr("looking through the plugin folders…"));
    m_spAPI->SupersonicSendOSC(oscpkt::Message("/clockwork/track/scan"));
    // A scan that never answers — an engine that went away mid-scan without
    // a word — must not leave the button dead for the session.
    QTimer::singleShot(120000, this, [this]() {
        if (!m_scanning) return;
        m_scanning = false;
        m_rescanButton->setEnabled(true);
        m_browserCount->setText(tr("the scan did not finish"));
    });
}

void TracksPanel::onTrackPlugins(unsigned int total, unsigned int offset,
                                 const std::vector<SonicPi::TrackPluginInfo>& plugins)
{
    // Pages arrive in order from one scan; the first starts the list over,
    // so a page from a scan that was cut short cannot leak into the next.
    if (offset == 0) m_arriving.clear();
    for (const SonicPi::TrackPluginInfo& p : plugins)
    {
        Discovered d;
        d.name   = QString::fromStdString(p.name);
        d.vendor = QString::fromStdString(p.vendor);
        d.path   = QString::fromStdString(p.path);
        d.index  = static_cast<uint32_t>(p.index);
        d.isInstrument = p.instrument;
        m_arriving.push_back(d);
    }
    if (static_cast<unsigned int>(m_arriving.size()) < total)
        return;   // more pages to come
    m_found = m_arriving;
    m_arriving.clear();
    m_foundIsRecalled = false;
    m_scanning = false;
    m_rescanButton->setEnabled(true);
    rememberPlugins();
    rebuildBrowser();
}

void TracksPanel::rememberPlugins() const
{
    // The last scan, for the next launch's first seconds. One string a
    // plugin, tab-separated: a QSettings list of maps is possible and is
    // not worth reading back.
    QStringList rows;
    for (const Discovered& d : m_found)
        rows << QStringLiteral("%1\t%2\t%3\t%4\t%5")
                    .arg(d.name, d.vendor, d.path).arg(d.index).arg(d.isInstrument ? 1 : 0);
    QSettings s = panelSettings();
    s.setValue(QStringLiteral("tracks/plugins"), rows);
}

void TracksPanel::recallPlugins()
{
    QSettings s = panelSettings();
    const QStringList rows = s.value(QStringLiteral("tracks/plugins")).toStringList();
    m_found.clear();
    for (const QString& row : rows)
    {
        const QStringList f = row.split(QLatin1Char('\t'));
        if (f.size() != 5) continue;
        Discovered d;
        d.name = f[0]; d.vendor = f[1]; d.path = f[2];
        d.index = f[3].toUInt();
        d.isInstrument = f[4] == QLatin1String("1");
        m_found.push_back(d);
    }
    m_foundIsRecalled = !m_found.isEmpty();
    rebuildBrowser();
}

void TracksPanel::rebuildBrowser()
{
    m_browser->clear();

    const QString needle = m_filter ? m_filter->text().trimmed() : QString();
    QString lastVendor;
    bool anyVendor = false;
    int shown = 0;

    for (int i = 0; i < m_found.size(); ++i)
    {
        const Discovered& d = m_found[i];
        if (!needle.isEmpty()
            && !d.name.contains(needle, Qt::CaseInsensitive)
            && !d.vendor.contains(needle, Qt::CaseInsensitive))
            continue;
        const int kind = m_kindFilter ? m_kindFilter->checkedId() : 0;
        if ((kind == 1 && !d.isInstrument) || (kind == 2 && d.isInstrument))
            continue;

        // Grouped by vendor, which is the only grouping that helps: two plugins
        // routinely share a name, and the path — the other thing that separates
        // them — is not something anyone reads. scan_all already sorted by
        // vendor then name, so this is a break rather than a sort.
        if (!anyVendor || d.vendor != lastVendor)
        {
            auto* header = new QListWidgetItem(d.vendor.isEmpty() ? tr("Unknown") : d.vendor);
            header->setFlags(Qt::NoItemFlags);       // a caption, not a choice
            QFont hf = RoleFont(FontRole::Small, m_fontScale);
            hf.setBold(true);
            header->setFont(hf);
            m_browser->addItem(header);
            lastVendor = d.vendor;
            anyVendor = true;
        }

        auto* item = new QListWidgetItem(QStringLiteral("   ") + d.name);
        // The index into m_found, not the row: rows shift as the filter changes
        // and the row a user double-clicks would then name a different plugin.
        item->setData(Qt::UserRole, i);
        item->setToolTip(d.isInstrument
                             ? tr("%1 — an instrument. On a track it takes notes from "
                                  "track_midi and makes sound.").arg(d.name)
                             : tr("%1 — an effect. On a track it transforms what is "
                                  "sent there.").arg(d.name));
        m_browser->addItem(item);
        ++shown;
    }

    if (m_scanning)
        return;   // "looking…" stays until the answer comes
    if (m_found.isEmpty())
        m_browserCount->setText(m_engineUp ? tr("nothing in the plugin folders")
                                           : tr("waiting for the engine"));
    else if (shown == m_found.size())
        m_browserCount->setText(shown == 1 ? tr("1 plugin") : tr("%1 plugins").arg(shown));
    else
        m_browserCount->setText(tr("%1 of %2").arg(shown).arg(m_found.size()));
    if (m_foundIsRecalled)
        m_browserCount->setText(m_browserCount->text() + tr(", from last time"));
}

void TracksPanel::onFilterChanged()
{
    rebuildBrowser();
}

void TracksPanel::onAddSelectedPlugin()
{
    QListWidgetItem* item = m_browser->currentItem();
    if (!item) return;
    bool ok = false;
    const int i = item->data(Qt::UserRole).toInt(&ok);
    if (!ok || i < 0 || i >= m_found.size()) return;   // a vendor caption
    addPluginToTrack(m_found[i]);
}

void TracksPanel::addPluginToTrack(const Discovered& d)
{
    if (!m_spAPI) return;
    const SonicPi::TrackInfo* t = selectedTrack();
    if (!t)
    {
        setStatus(tr("Make a track first — a plugin lives on a track."), false);
        return;
    }
    // The engine loads it and rebroadcasts the list; the device appears when
    // that arrives. There is nothing to draw in the meantime that would not
    // have to be reconciled away.
    oscpkt::Message m("/clockwork/track/plugin/add");
    m.pushInt32(t->id);
    m.pushStr(enginePath(d.path).toStdString());
    m.pushInt32(static_cast<int32_t>(d.index));
    m_spAPI->SupersonicSendOSC(m);
    setStatus(tr("Loading %1 onto :%2…").arg(d.name, QString::fromStdString(t->name)), true);
    m_statusOnNextList = tr("%1 is on :%2.").arg(d.name, QString::fromStdString(t->name));
}

void TracksPanel::onFolders()
{
    /*
     * The places plugins are looked for. The platform's are shown so a user
     * can see where a plugin SHOULD have gone; their own can be added, which
     * is the only answer to a plugin installed anywhere else.
     */
    QDialog dlg(this);
    dlg.setWindowTitle(tr("Plugin folders"));
    auto* v = new QVBoxLayout(&dlg);

    auto* platformCaption = new QLabel(tr("ALWAYS SEARCHED"), &dlg);
    platformCaption->setObjectName(QStringLiteral("tutSection"));
    v->addWidget(platformCaption);
    for (const QString& p : m_platformFolders)
    {
        auto* l = new QLabel(QDir::toNativeSeparators(p), &dlg);
        l->setTextInteractionFlags(Qt::TextSelectableByMouse);
        v->addWidget(l);
    }
    if (m_platformFolders.isEmpty())
        v->addWidget(new QLabel(tr("(the engine has not said yet)"), &dlg));

    auto* extraCaption = new QLabel(tr("ALSO SEARCHED"), &dlg);
    extraCaption->setObjectName(QStringLiteral("tutSection"));
    v->addWidget(extraCaption);
    auto* list = new QListWidget(&dlg);
    list->setObjectName(QStringLiteral("docsNavList"));
    list->setFrameShape(QFrame::NoFrame);
    for (const QString& p : m_extraFolders) list->addItem(QDir::toNativeSeparators(p));
    v->addWidget(list, 1);

    auto* row = new QHBoxLayout();
    auto* add = housePill(tr("Add folder..."), &dlg, m_fontScale);
    auto* remove = housePill(tr("Remove"), &dlg, m_fontScale);
    remove->setEnabled(false);
    row->addWidget(add);
    row->addWidget(remove);
    row->addStretch(1);
    v->addLayout(row);
    connect(list, &QListWidget::currentItemChanged, &dlg,
            [remove](QListWidgetItem* cur, QListWidgetItem*) { remove->setEnabled(cur != nullptr); });
    connect(add, &QPushButton::clicked, &dlg, [this, &dlg, list]() {
        const QString dir = QFileDialog::getExistingDirectory(&dlg, tr("Add a plugin folder"));
        if (dir.isEmpty() || !m_spAPI) return;
        oscpkt::Message m("/clockwork/track/folders/add");
        m.pushStr(enginePath(dir).toStdString());
        m_spAPI->SupersonicSendOSC(m);
        list->addItem(QDir::toNativeSeparators(dir));
    });
    connect(remove, &QPushButton::clicked, &dlg, [this, list]() {
        QListWidgetItem* cur = list->currentItem();
        if (!cur || !m_spAPI) return;
        oscpkt::Message m("/clockwork/track/folders/remove");
        m.pushStr(enginePath(QDir::fromNativeSeparators(cur->text())).toStdString());
        m_spAPI->SupersonicSendOSC(m);
        delete cur;
    });

    auto* buttons = new QDialogButtonBox(QDialogButtonBox::Close, &dlg);
    connect(buttons, &QDialogButtonBox::rejected, &dlg, &QDialog::reject);
    connect(buttons, &QDialogButtonBox::accepted, &dlg, &QDialog::accept);
    v->addWidget(buttons);
    dlg.resize(ScaleWidthForDPI(520), ScaleHeightForDPI(380));
    dlg.exec();
    // The engine has rebroadcast its folders by now; a rescan picks them up.
    onRescan();
}

void TracksPanel::onTrackFolders(const std::vector<std::string>& extra,
                                 const std::vector<std::string>& platform)
{
    QStringList next;
    for (const std::string& s : extra) next << QString::fromStdString(s);
    QStringList plat;
    for (const std::string& s : platform) plat << QString::fromStdString(s);
    // The first time the engine says what it searches is the first scan of
    // the session (or the first since this engine came up: plugins may have
    // been installed since the last one). A change to the extra folders is
    // another.
    const bool changed = (next != m_extraFolders) || m_platformFolders.isEmpty();
    m_extraFolders = next;
    m_platformFolders = plat;
    if (changed || m_foundIsRecalled) onRescan();
}

// ── Tracks ───────────────────────────────────────────────────────────────────

void TracksPanel::onTracks(int laneBase, const std::vector<SonicPi::TrackInfo>& tracks)
{
    m_laneBase = laneBase;
    m_tracks = tracks;
    m_haveList = true;
    if (m_bridgeDown)
    {
        // The plugin process sends the list as it joins: this is it back.
        m_bridgeDown = false;
        setStatus(tracks.empty() ? tr("The plugin process is back.")
                                 : tr("The plugin process is back, with its tracks."), true);
    }

    // Parameter lists for plugins that are gone go with them: a handle is
    // never reused for a different plugin while the engine runs, but the
    // memory would be.
    QSet<int> live;
    for (const SonicPi::TrackInfo& t : tracks)
        for (const SonicPi::TrackNodeInfo& n : t.nodes) live.insert(n.handle);
    for (auto it = m_params.begin(); it != m_params.end();)
    {
        if (live.contains(it.key())) ++it;
        else it = m_params.erase(it);
    }
    // And lists for plugins that are new are asked for now, not when their
    // track is next shown: the editor completes parameter names for every
    // track. Each list is fetched once per handle.
    for (int handle : live) requestParams(handle);
    publishParamNames();

    /*
     * THE CURRENT RIG. What the engine has is kept at a fixed path after every
     * change and loaded back when an engine with nothing arrives — which is a
     * fresh start, or the engine coming back after a swap. Without this a
     * restart of Sonic Pi is a restart of the studio, and a studio you have to
     * rebuild every morning is one you stop building.
     *
     * An engine reporting NO tracks is only "saved as empty" when the last
     * track went because this panel removed it; otherwise an empty engine is
     * one to restore into.
     */
    // ONCE per engine. `rig/load` answers with a broadcast of the track list,
    // so a restore that brings nothing back arrives here as another empty
    // list — and without this guard that is a loop: restore, empty, restore,
    // at whatever rate the engine can answer. It also eats a track created by
    // hand, because every pass reloads the rig over it. `m_restoreTried` was
    // already set for exactly this and simply never read; `onEngineReady`
    // clears it, so an engine that comes back after a device swap still gets
    // its one attempt.
    if (tracks.empty() && !m_userEmptied && !m_restoreTried)
    {
        if (laneBase > 0 && m_spAPI && QFileInfo::exists(currentRigPath()))
        {
            oscpkt::Message m("/clockwork/track/rig/load");
            m.pushStr(enginePath(currentRigPath()).toStdString());
            m_spAPI->SupersonicSendOSC(m);
            setStatus(tr("Bringing back the tracks from last time…"), true);
            m_statusOnNextList = tr("The tracks from last time are back.");
        }
        m_restoreTried = true;
    }
    else
    {
        if (!tracks.empty()) m_userEmptied = false;
        m_restoreTried = true;
        saveCurrentRig();
        if (!m_statusOnNextList.isEmpty())
        {
            setStatus(m_statusOnNextList, true);
            m_statusOnNextList.clear();
        }
    }

    // Which track to show: the one that was, or the one just made, or the
    // first. Selecting the newest after a create is what every DAW does — you
    // made it so you want to look at it.
    int select = m_selectedId;
    if (m_selectNewest && !tracks.empty())
    {
        int newest = 0;
        for (const SonicPi::TrackInfo& t : tracks) if (t.id > newest) newest = t.id;
        select = newest;
        m_selectNewest = false;
    }
    if (!trackById(select)) select = tracks.empty() ? 0 : tracks.front().id;

    rebuildTrackList();
    showTrack(select);
}

void TracksPanel::onTrackState(int id, float gain, bool mute)
{
    for (SonicPi::TrackInfo& t : m_tracks)
    {
        if (t.id != id) continue;
        t.gain = gain;
        t.mute = mute;
        break;
    }
    if (id == m_selectedId) refreshHeader();
    saveCurrentRig();
}

void TracksPanel::rebuildTrackList()
{
    m_rebuildingList = true;
    m_trackList->clear();
    for (const SonicPi::TrackInfo& t : m_tracks)
    {
        auto* item = new QListWidgetItem(QString::fromStdString(t.name));
        item->setFlags(item->flags() | Qt::ItemIsEditable);
        item->setData(Qt::UserRole, t.id);
        const int n = static_cast<int>(t.nodes.size());
        item->setToolTip(n == 0 ? tr(":%1 — nothing on it yet").arg(QString::fromStdString(t.name))
                       : n == 1 ? tr(":%1 — one plugin").arg(QString::fromStdString(t.name))
                                : tr(":%1 — %2 plugins").arg(QString::fromStdString(t.name)).arg(n));
        if (t.mute && m_theme)
        {
            // A muted track is still there and still selectable; it reads
            // as quiet, the way a muted track's name dims in Live.
            item->setForeground(m_theme->faintForeground());
            item->setToolTip(item->toolTip() + tr(" — muted"));
        }
        m_trackList->addItem(item);
    }
    m_rebuildingList = false;
}

void TracksPanel::showTrack(int id)
{
    const bool changed = (id != m_selectedId);
    if (changed) clearStatus();
    m_selectedId = id;
    const SonicPi::TrackInfo* t = selectedTrack();

    // The list follows the selection rather than the other way round, so a
    // rebuild does not fire a selection change that rebuilds again.
    m_rebuildingList = true;
    for (int i = 0; i < m_trackList->count(); ++i)
    {
        QListWidgetItem* item = m_trackList->item(i);
        if (item->data(Qt::UserRole).toInt() == id) { m_trackList->setCurrentItem(item); break; }
    }
    m_rebuildingList = false;

    const bool any = !m_tracks.empty();
    m_header->setVisible(t != nullptr);
    m_codeBlock->setVisible(t != nullptr);
    m_noTracks->setVisible(!any);
    if (!t)
    {
        if (m_scope) m_scope->stop();
        m_scopeTrack = 0;
        m_scopeSlot = -1;
        m_emptyLane->hide();
        m_laneScroll->hide();
        for (Device* d : m_devices) destroyDevice(d);
        m_devices.clear();
        if (m_addButton) m_addButton->setEnabled(false);
        // Only one reason for no tracks is a message; the other is a lane
        // base of 0, which means this SuperSonic was built without them.
        if (m_haveList && m_laneBase == 0)
            m_noTracks->setText(
                tr("This SuperSonic has no track lanes.\n"
                   "Tracks need a build with the plugin host in it."));
        return;
    }

    refreshHeader();
    if (changed)
    {
        // A different track: its devices are different objects. Configure
        // state and the rest belong to the device, so they go with it.
        for (Device* d : m_devices) destroyDevice(d);
        m_devices.clear();
    }
    rebuildChain();
    if (m_addButton)
        m_addButton->setEnabled(m_browser->currentItem()
                                && m_browser->currentItem()->data(Qt::UserRole).isValid());
}

void TracksPanel::refreshHeader()
{
    const SonicPi::TrackInfo* t = selectedTrack();
    if (!t) return;
    const QString name = QString::fromStdString(t->name);
    m_trackTitle->setText(QStringLiteral(":") + name);

    bool hasInstrument = false;
    int latency = 0;
    for (const SonicPi::TrackNodeInfo& n : t->nodes)
    {
        if (n.instrument) hasInstrument = true;
        if (!n.bypass) latency += n.latency;
    }
    // Channels as code sees them (1-based): the numbers `output:` and
    // `input:` would take if someone wrote the routing by hand.
    QString detail = tr("send %1·%2  →  chain  →  return %3·%4")
                         .arg(t->sendChannel + 1).arg(t->sendChannel + 2)
                         .arg(t->returnChannel + 1).arg(t->returnChannel + 2);
    if (latency > 0) detail += tr("   ·   %1 frames of latency").arg(latency);
    if (t->mute) detail += tr("   ·   muted");
    m_trackDetail->setText(detail);

    m_ampDial->setValue(t->gain, false);
    m_muteButton->setChecked(t->mute);

    // The scope follows the selection. A fresh start on every refresh would
    // blank the rings each time the list lands, so it is retargeted only
    // when the track, or the slot it writes, is not the one on show.
    if (m_scope && m_spAPI
        && (m_scopeTrack != t->id || m_scopeSlot != t->scopeSlot))
    {
        m_scopeTrack = t->id;
        m_scopeSlot = t->scopeSlot;
        if (t->scopeSlot >= 0)
        {
            m_scope->setSlot(static_cast<unsigned int>(t->scopeSlot));
            m_scope->start(m_spAPI.get());
        }
        else
        {
            m_scope->stop();
        }
    }

    // The code that reaches this track. A track with no instrument has no use
    // for track_midi, so that line is not offered — the panel should never
    // suggest code that will do nothing. The note comes as the idiom: the
    // track is set once with use_track, and track_midi plays it.
    setCodeLine(m_codeLive, QStringLiteral("live_track :%1").arg(name));
    setCodeLine(m_codeSend, QStringLiteral("with_send :%1, amp: 0.5 do").arg(name));
    setCodeLine(m_codeNote, QStringLiteral("use_track :%1\ntrack_midi :e3, sustain: 0.5").arg(name));
    m_codeNoteRow->setVisible(hasInstrument);
}

void TracksPanel::setCodeLine(QLabel* label, const QString& code)
{
    if (!label) return;
    // The accessible name is the plain code: it is what copy and insert read,
    // and what a screen reader says instead of the markup.
    label->setAccessibleName(code);
    if (!m_theme)
    {
        label->setText(code.toHtmlEscaped());
        return;
    }
    SonicPi::CodeColours colours;
    colours.keyword = m_theme->color("KeywordForeground").name();
    colours.symbol  = m_theme->color("SymbolForeground").name();
    colours.number  = m_theme->color("NumberForeground").name();
    colours.string  = m_theme->color("DoubleQuotedStringForeground").name();
    colours.comment = m_theme->color("CommentForeground").name();
    label->setText(SonicPi::TutorialDocs::highlightCode(code, colours));
}

void TracksPanel::onTrackSelectionChanged()
{
    if (m_rebuildingList) return;
    QListWidgetItem* cur = m_trackList->currentItem();
    if (!cur) return;
    showTrack(cur->data(Qt::UserRole).toInt());
}

void TracksPanel::onTrackItemEdited(QListWidgetItem* item)
{
    if (m_rebuildingList || !item) return;
    const int id = item->data(Qt::UserRole).toInt();
    const SonicPi::TrackInfo* t = trackById(id);
    if (!t) return;
    const QString was = QString::fromStdString(t->name);
    const QString name = item->text().trimmed();
    if (name == was) return;

    if (!symbolShape().match(name).hasMatch())
    {
        setStatus(tr("A track's name is typed as a symbol: letters, digits and underscores, "
                     "starting with a letter. \"%1\" is not one.").arg(name), false);
        m_rebuildingList = true;
        item->setText(was);
        m_rebuildingList = false;
        return;
    }
    if (!m_spAPI) return;
    oscpkt::Message m("/clockwork/track/rename");
    m.pushInt32(id);
    m.pushStr(name.toStdString());
    m_spAPI->SupersonicSendOSC(m);
    // The list rebuilds from the engine's answer; a refused name (taken, say)
    // comes back as track/error and the row reverts with the rebroadcast.
}

QString TracksPanel::uniqueTrackName() const
{
    QSet<QString> taken;
    for (const SonicPi::TrackInfo& t : m_tracks) taken.insert(QString::fromStdString(t.name));
    for (int n = 1;; ++n)
    {
        const QString candidate = QStringLiteral("track%1").arg(n);
        if (!taken.contains(candidate)) return candidate;
    }
}

void TracksPanel::onNewTrack()
{
    if (!m_spAPI) return;
    if (m_haveList && m_laneBase == 0)
    {
        setStatus(tr("This SuperSonic has no track lanes — tracks need a build with the plugin host in it."), false);
        return;
    }
    oscpkt::Message m("/clockwork/track/create");
    m.pushStr(uniqueTrackName().toStdString());
    m_spAPI->SupersonicSendOSC(m);
    m_selectNewest = true;
    setStatus(tr("Double-click the new track's name to call it something. "
                 "Then use_track :name and track_midi :e3 play its instrument, with_send :name feeds its effects."), true);
}

void TracksPanel::onRemoveTrack()
{
    const SonicPi::TrackInfo* t = selectedTrack();
    if (!t || !m_spAPI) return;
    if (m_tracks.size() == 1) m_userEmptied = true;
    oscpkt::Message m("/clockwork/track/remove");
    m.pushInt32(t->id);
    m_spAPI->SupersonicSendOSC(m);
}

const SonicPi::TrackInfo* TracksPanel::selectedTrack() const
{
    return trackById(m_selectedId);
}

const SonicPi::TrackInfo* TracksPanel::trackById(int id) const
{
    if (id == 0) return nullptr;
    for (const SonicPi::TrackInfo& t : m_tracks) if (t.id == id) return &t;
    return nullptr;
}

// ── Rigs ─────────────────────────────────────────────────────────────────────

QString TracksPanel::rigsDir() const
{
    if (!m_spAPI) return QString();
    const QString base = QString::fromStdString(m_spAPI->GetPath(SonicPi::SonicPiPath::ConfigPath).string());
    const QString dir = QDir(base).filePath(QStringLiteral("rigs"));
    QDir().mkpath(dir);
    return dir;
}

QString TracksPanel::currentRigPath() const
{
    const QString dir = rigsDir();
    return dir.isEmpty() ? QString() : QDir(dir).filePath(QStringLiteral("current.json"));
}

void TracksPanel::saveCurrentRig()
{
    // Nothing to keep until the engine has said what it has, and nothing to
    // keep if it has no lanes: a rig from a build without tracks is a file
    // that says "no tracks" and would overwrite one that said more.
    if (!m_spAPI || !m_haveList || !m_restoreTried || m_laneBase == 0) return;
    const QString path = currentRigPath();
    if (path.isEmpty()) return;
    oscpkt::Message m("/clockwork/track/rig/save");
    m.pushStr(enginePath(path).toStdString());
    m_spAPI->SupersonicSendOSC(m);
}

void TracksPanel::onSaveRig()
{
    if (!m_spAPI) return;
    const QString path = QFileDialog::getSaveFileName(this, tr("Save rig"),
                                                      QDir(rigsDir()).filePath(tr("my-rig.json")),
                                                      tr("Rigs (*.json)"));
    if (path.isEmpty()) return;
    oscpkt::Message m("/clockwork/track/rig/save");
    m.pushStr(enginePath(path).toStdString());
    m_spAPI->SupersonicSendOSC(m);
    setStatus(tr("Rig saved to %1").arg(QDir::toNativeSeparators(path)), true);
}

void TracksPanel::onLoadRig()
{
    if (!m_spAPI) return;
    const QString path = QFileDialog::getOpenFileName(this, tr("Load rig"), rigsDir(),
                                                      tr("Rigs (*.json)"));
    if (path.isEmpty()) return;
    oscpkt::Message m("/clockwork/track/rig/load");
    m.pushStr(enginePath(path).toStdString());
    m_spAPI->SupersonicSendOSC(m);
    setStatus(tr("Loading rig from %1…").arg(QDir::toNativeSeparators(path)), true);
    m_statusOnNextList = tr("Rig loaded from %1.").arg(QDir::toNativeSeparators(path));
}

// ── The chain ────────────────────────────────────────────────────────────────

void TracksPanel::rebuildChain()
{
    const SonicPi::TrackInfo* t = selectedTrack();
    if (!t) return;

    /*
     * RECONCILE, do not rebuild. The list is rebroadcast after every edit —
     * a bypass toggled, a plugin added two tracks over — and tearing every
     * device down for each one would lose Configure state, drop dial
     * positions mid-drag and flicker. Devices are kept by handle; only what
     * is new is built and only what is gone is destroyed, and the lane is
     * re-ordered to match the engine.
     */
    QHash<int, Device*> keep;
    for (Device* d : m_devices) keep.insert(d->info.handle, d);

    QVector<Device*> next;
    for (const SonicPi::TrackNodeInfo& n : t->nodes)
    {
        Device* dev = keep.take(n.handle);
        if (!dev)
        {
            dev = new Device();
            dev->info = n;
            buildDeviceFrame(dev);
            dev->pending = savedCuration(n);
            requestParams(n.handle);
            realisePending(dev);
        }
        else
        {
            dev->info = n;
        }
        next.push_back(dev);
    }
    for (Device* gone : keep) destroyDevice(gone);
    m_devices = next;

    // Re-order the lane to match. Everything comes out and goes back in, in
    // order, before the trailing stretch that keeps devices packed left.
    for (Device* d : m_devices) m_lane->removeWidget(d->frame);
    for (int i = 0; i < m_devices.size(); ++i)
    {
        m_lane->insertWidget(i, m_devices[i]->frame, 0, Qt::AlignTop);
        m_devices[i]->frame->show();
        refreshDeviceButtons(m_devices[i], i, m_devices.size());
        setDeviceStatus(m_devices[i], deviceStatusText(m_devices[i]), true);
    }

    m_emptyLane->setVisible(m_devices.isEmpty());
    m_laneScroll->setVisible(!m_devices.isEmpty());
}

QWidget* TracksPanel::buildDeviceFrame(Device* dev)
{
    /*
     * A DEVICE IS A CARD, using the card's own names: #qsCard is a bordered
     * box with an accent header, a bold title on it, bare glyph buttons at its
     * right, a body and a footer — which is exactly what a hosted plugin needs
     * to be. A parallel set of names would look almost-but-not-quite like the
     * quickstart deck one tab away and track only the themes someone checked.
     */
    auto* frame = new QFrame(this);
    frame->setObjectName(QStringLiteral("qsCard"));
    frame->setFrameShape(QFrame::NoFrame);
    dev->frame = frame;

    auto* cardLayout = new QVBoxLayout(frame);
    cardLayout->setContentsMargins(0, 0, 0, 0);
    cardLayout->setSpacing(0);

    // ── The accent header ────────────────────────────────────────────────────
    auto* header = new QWidget(frame);
    header->setObjectName(QStringLiteral("qsCardHeader"));
    auto* hh = new QHBoxLayout(header);
    hh->setContentsMargins(ScaleWidthForDPI(10), 0, 0, 0);
    hh->setSpacing(0);

    auto* title = new QLabel(header);
    title->setObjectName(QStringLiteral("qsCardTitle"));   // size: applyFontsIn
    dev->title = title;
    title->setToolTip(tr("%1\n%2\n%3\n\nDouble-click to open the plugin's window.")
                          .arg(QString::fromStdString(dev->info.vendor),
                               QString::fromStdString(dev->info.format).toUpper(),
                               QDir::toNativeSeparators(QString::fromStdString(dev->info.path))));
    // Double-clicking a device's name opens its window, the gesture every
    // DAW's device bar answers to; the glyph beside it is the visible way.
    title->installEventFilter(this);
    hh->addWidget(title, 1);

    /*
     * THE DEVICE TITLE BAR, as Live lays it out: on/off at the left of the
     * controls, the plugin's own window, configure, and the device's place in
     * the chain — every DAW's device has these, and every one draws them as
     * glyphs on the bar rather than words in the body. The glyphs are the
     * card's own (tabler, via utils/tablericons.h) in the quickstart cards'
     * inks: accent-contrast on the bar, and the body's ink while the hover
     * fill turns the button near-white (tintDeviceGlyph, on Enter/Leave).
     */
    const int btnW = ScaleWidthForDPI(28);
    const int btnH = ScaleHeightForDPI(28);
    auto glyphBtn = [&](TablerIcons::Glyph g, const QString& tip, bool checkable) {
        auto* b = new QPushButton(header);
        b->setObjectName(QStringLiteral("qsCardBtn"));
        b->setProperty("device", true);
        b->setCursor(Qt::PointingHandCursor);
        b->setFixedSize(btnW, btnH);
        b->setIconSize(QSize(ScaleWidthForDPI(16), ScaleWidthForDPI(16)));
        b->setProperty("glyph", static_cast<int>(g));
        b->setToolTip(tip);
        b->setAccessibleName(tip);
        b->setCheckable(checkable);
        if (checkable)
            connect(b, &QPushButton::toggled, this, [this, b]() { tintDeviceGlyph(b, b->underMouse()); });
        tintDeviceGlyph(b, false);   // after setCheckable: an off toggle starts faded
        b->installEventFilter(this);
        hh->addWidget(b);
        return b;
    };

    dev->bypassBtn = glyphBtn(TablerIcons::Glyph::Power,
                              tr("On. Off passes the signal round this plugin untouched."), true);
    // Off is the whole card faded (refreshDeviceButtons), so this glyph keeps
    // its ink: dimmed twice over it would vanish from the very card it turns
    // back on.
    dev->bypassBtn->setProperty("fadesCard", true);
    connect(dev->bypassBtn, &QPushButton::clicked, this, [this, dev](bool on) {
        if (!m_spAPI) return;
        oscpkt::Message m("/clockwork/track/plugin/bypass");
        m.pushInt32(dev->info.handle);
        m.pushInt32(on ? 0 : 1);
        m_spAPI->SupersonicSendOSC(m);
    });

    dev->editorBtn = glyphBtn(TablerIcons::Glyph::AppWindow,
                              tr("Show or hide the plugin's own window. It is the same instance you hear."), true);
    connect(dev->editorBtn, &QPushButton::clicked, this, [this, dev](bool on) {
        setEditorVisible(dev, on);
    });

    dev->configureBtn = glyphBtn(TablerIcons::Glyph::Adjustments,
                                 tr("Configure: while this is lit, each control you move in the "
                                    "plugin's window is added to this device."), true);
    dev->configureBtn->setProperty("armed", false);
    connect(dev->configureBtn, &QPushButton::clicked, this, [this, dev]() { toggleConfigure(dev); });

    dev->leftBtn = glyphBtn(TablerIcons::Glyph::ChevronLeft, tr("Move earlier in the chain."), false);
    connect(dev->leftBtn, &QPushButton::clicked, this, [this, dev]() {
        const int i = m_devices.indexOf(dev);
        if (i <= 0 || !m_spAPI) return;
        oscpkt::Message m("/clockwork/track/plugin/move");
        m.pushInt32(dev->info.handle);
        m.pushInt32(i - 1);
        m_spAPI->SupersonicSendOSC(m);
    });
    dev->rightBtn = glyphBtn(TablerIcons::Glyph::ChevronRight, tr("Move later in the chain."), false);
    connect(dev->rightBtn, &QPushButton::clicked, this, [this, dev]() {
        const int i = m_devices.indexOf(dev);
        if (i < 0 || i >= m_devices.size() - 1 || !m_spAPI) return;
        oscpkt::Message m("/clockwork/track/plugin/move");
        m.pushInt32(dev->info.handle);
        m.pushInt32(i + 1);
        m_spAPI->SupersonicSendOSC(m);
    });

    auto* remove = glyphBtn(TablerIcons::Glyph::X, tr("Take this plugin off the track."), false);
    dev->removeBtn = remove;
    remove->setProperty("corner", "tr");   // rounds to match the card's corner
    connect(remove, &QPushButton::clicked, this, [this, dev]() {
        if (!m_spAPI) return;
        oscpkt::Message m("/clockwork/track/plugin/remove");
        m.pushInt32(dev->info.handle);
        m_spAPI->SupersonicSendOSC(m);
    });

    cardLayout->addWidget(header);

    // ── The face ─────────────────────────────────────────────────────────────
    auto* body = new QWidget(frame);
    body->setObjectName(QStringLiteral("qsCardBody"));
    auto* bodyV = new QVBoxLayout(body);
    bodyV->setContentsMargins(ScaleWidthForDPI(10), ScaleHeightForDPI(10),
                              ScaleWidthForDPI(10), ScaleHeightForDPI(10));
    bodyV->setSpacing(ScaleHeightForDPI(6));

    auto* grid = new QWidget(body);
    dev->paramGrid = new QGridLayout(grid);
    dev->paramGrid->setContentsMargins(0, 0, 0, 0);
    dev->paramGrid->setHorizontalSpacing(ScaleWidthForDPI(6));
    dev->paramGrid->setVerticalSpacing(ScaleHeightForDPI(6));
    bodyV->addWidget(grid);

    // The empty state is an INSTRUCTION, not an apology. A device with no
    // controls is the normal way one starts, so this says what to do about it.
    dev->hint = new QLabel(tr("No controls here yet.\nTurn on Configure, then move a\n"
                              "control in the plugin's window."), body);
    dev->hint->setObjectName(QStringLiteral("tutSection"));
    dev->hint->setAlignment(Qt::AlignCenter);
    bodyV->addWidget(dev->hint);
    bodyV->addStretch(1);
    cardLayout->addWidget(body, 1);

    // ── The footer: what this device is doing ────────────────────────────────
    auto* footer = new QWidget(frame);
    footer->setObjectName(QStringLiteral("qsCardFooter"));
    auto* fh = new QHBoxLayout(footer);
    fh->setContentsMargins(ScaleWidthForDPI(10), ScaleHeightForDPI(6),
                           ScaleWidthForDPI(10), ScaleHeightForDPI(6));
    if (dev->info.instrument)
    {
        /*
         * WHICH NOTES THIS ONE TAKES. A note played at a track reaches every
         * instrument on it, which is what a layered sound wants and the
         * default; two instruments that should answer to different code get
         * a channel each here and `channel: 2` in the code — the way Live
         * addresses the instruments in a rack. Effects have no such thing.
         */
        dev->channel = new QComboBox(footer);
        dev->channel->addItem(tr("Ch: All"), 0);
        for (int ch = 1; ch <= 16; ++ch)
            dev->channel->addItem(tr("Ch: %1").arg(ch), ch);
        dev->channel->setToolTip(tr("The MIDI channel this instrument listens on. "
                                    "All means every note sent to the track; a number "
                                    "means only notes played with channel: that number."));
        dev->channel->setAccessibleName(tr("Listen channel"));
        dev->channel->setSizeAdjustPolicy(QComboBox::AdjustToContents);
        connect(dev->channel, QOverload<int>::of(&QComboBox::currentIndexChanged), this,
                [this, dev](int i) { setListenChannel(dev, dev->channel->itemData(i).toInt()); });
        fh->addWidget(dev->channel);
    }
    dev->status = new QLabel(footer);
    dev->status->setObjectName(QStringLiteral("qsCardBlurb"));
    dev->status->setWordWrap(true);
    fh->addWidget(dev->status, 1);
    cardLayout->addWidget(footer);

    applyFontsIn(frame);
    fitDeviceTitle(dev);   // measured in the font just applied
    return frame;
}

void TracksPanel::setListenChannel(Device* dev, int channel)
{
    if (!m_spAPI || !dev) return;
    if (channel == dev->info.channel) return;
    oscpkt::Message m("/clockwork/track/plugin/channel");
    m.pushInt32(dev->info.handle);
    m.pushInt32(channel);
    m_spAPI->SupersonicSendOSC(m);
}

void TracksPanel::tintDeviceGlyph(QPushButton* b, bool hover)
{
    const QVariant g = b->property("glyph");
    if (!g.isValid()) return;
    // The accent bar's contrast ink, as the card's title. A toggle shows its
    // state in the glyph, as Live's device bar does: full ink when on, faded
    // when off, and full again under the pointer. Nothing is painted behind
    // a checked glyph — a wash on the accent bar read as a second, paler
    // colour rather than a state; hover is a small chip (app.qss), not a
    // stripe of the bar. Configure, armed, is the
    // one toggle that is a MODE rather than a setting: its pill turns inside
    // out (app.qss, [armed]) and the glyph takes the accent on it.
    QColor ink = m_theme ? m_theme->accentContrastText() : QColor(Qt::white);
    if (b->property("armed").toBool() && m_theme)
        ink = m_theme->color("HighlightedBackground");
    else if (b->isCheckable() && !b->isChecked() && !hover && !b->property("fadesCard").toBool())
        ink.setAlphaF(0.38);
    b->setIcon(TablerIcons::icon(static_cast<TablerIcons::Glyph>(g.toInt()), ink,
                                 ScaleWidthForDPI(16), devicePixelRatioF()));
}

bool TracksPanel::eventFilter(QObject* watched, QEvent* event)
{
    if (event->type() == QEvent::Enter || event->type() == QEvent::Leave)
    {
        if (auto* b = qobject_cast<QPushButton*>(watched))
            tintDeviceGlyph(b, event->type() == QEvent::Enter);
    }
    else if (event->type() == QEvent::MouseButtonDblClick)
    {
        // A device's title, double-clicked: its window, and the header
        // toggle follows so the two never disagree about what is open.
        for (Device* dev : m_devices)
        {
            if (!dev->frame || !dev->frame->isAncestorOf(qobject_cast<QWidget*>(watched))) continue;
            if (dev->editorBtn) dev->editorBtn->setChecked(true);
            setEditorVisible(dev, true);
            return true;
        }
    }
    return QWidget::eventFilter(watched, event);
}

void TracksPanel::refreshDeviceButtons(Device* dev, int index, int count)
{
    if (!dev) return;
    if (dev->bypassBtn)
    {
        // The tint follows the state, not the signal: setChecked to the
        // value it already has says nothing, and the glyph would keep the
        // ink it was born with.
        dev->bypassBtn->setChecked(!dev->info.bypass);
        tintDeviceGlyph(dev->bypassBtn, dev->bypassBtn->underMouse());
    }
    if (dev->frame)
    {
        // An off device is the whole card faded, not one dim glyph: the
        // signal goes round it, so the card steps back from the lane. The
        // effect exists only while it is off — a card that is on paints
        // straight to the screen, with nothing rasterised in between.
        const bool faded = dev->frame->graphicsEffect() != nullptr;
        if (dev->info.bypass && !faded)
        {
            auto* fade = new QGraphicsOpacityEffect(dev->frame);
            fade->setOpacity(0.45);
            dev->frame->setGraphicsEffect(fade);
        }
        else if (!dev->info.bypass && faded)
        {
            dev->frame->setGraphicsEffect(nullptr);
        }
    }
    if (dev->channel)
    {
        // The engine's word, without a round trip back to it.
        const QSignalBlocker quiet(dev->channel);
        const int i = dev->channel->findData(dev->info.channel);
        if (i >= 0) dev->channel->setCurrentIndex(i);
    }
    if (dev->leftBtn)   dev->leftBtn->setEnabled(index > 0);
    if (dev->rightBtn)  dev->rightBtn->setEnabled(index < count - 1);
}

QString TracksPanel::deviceStatusText(const Device* dev) const
{
    if (!dev) return QString();
    if (dev->configuring) return tr("configure: move a control in the plugin's window");
    if (dev->info.bypass) return tr("off — the signal goes round it");
    QString s = dev->info.instrument
                    ? (dev->info.channel > 0 ? tr("instrument, notes on channel %1").arg(dev->info.channel)
                                             : tr("instrument"))
                    : tr("effect");
    if (dev->info.latency > 0) s += tr(" · %1 frames of latency").arg(dev->info.latency);
    const ParamList& pl = m_params.value(dev->info.handle);
    if (pl.requested && !pl.complete())
        s += tr(" · reading %1 parameters…").arg(pl.total);
    return s;
}

void TracksPanel::destroyDevice(Device* dev)
{
    if (!dev) return;
    if (dev->frame)
    {
        m_lane->removeWidget(dev->frame);
        dev->frame->deleteLater();
    }
    delete dev;
}

TracksPanel::Device* TracksPanel::deviceFor(int handle)
{
    if (handle == 0) return nullptr;
    for (Device* d : m_devices) if (d->info.handle == handle) return d;
    return nullptr;
}

// ── Parameters ───────────────────────────────────────────────────────────────

void TracksPanel::requestParams(int handle)
{
    ParamList& pl = m_params[handle];
    if (pl.requested || !m_spAPI) return;
    pl.requested = true;
    requestParamsPage(handle, 0);
}

void TracksPanel::requestParamsPage(int handle, uint32_t offset)
{
    // Answered by broadcast, one page per request — see onTrackParams, which
    // asks for the next. The list comes from the running instance, so it is
    // the plugin's real one, not a copy's.
    if (!m_spAPI) return;
    oscpkt::Message m("/clockwork/track/plugin/params");
    m.pushInt32(handle);
    m.pushInt32(static_cast<int>(offset));
    m_spAPI->SupersonicSendOSC(m);
}

// A number the way a knob's label would show it: "0.5", "20000", "-12".
void TracksPanel::publishParamNames()
{
    /*
     * ONE LIST PER TRACK, IN CHAIN ORDER, each parameter with the plugin it
     * is on. A name one plugin uses twice is offered once; a name two
     * plugins share is offered for both, and the completer gives the
     * second its plugin-prefixed key. Only complete lists are used: a
     * half-fetched Surge would offer the first page and stop.
     */
    for (const SonicPi::TrackInfo& t : m_tracks)
    {
        QList<SonicPi::TrackParam> out;
        for (const SonicPi::TrackNodeInfo& n : t.nodes)
        {
            auto it = m_params.constFind(n.handle);
            if (it == m_params.constEnd() || !it->complete()) continue;
            QSet<QString> seen;
            for (uint32_t id : it->order)
            {
                const SonicPi::TrackParamInfo& p = it->byId[id];
                const QString name = QString::fromStdString(p.name);
                if (name.isEmpty() || seen.contains(name)) continue;
                seen.insert(name);
                SonicPi::TrackParam tp;
                tp.name = name;
                tp.plugin = QString::fromStdString(n.name);
                tp.group = QString::fromStdString(p.groupName);
                tp.min = p.min;
                tp.max = p.max;
                tp.value = p.value;
                out << tp;
            }
        }
        emit trackParamsChanged(QString::fromStdString(t.name), out);
    }
}

void TracksPanel::onTrackParams(int handle, uint32_t total, uint32_t offset,
                                const std::vector<SonicPi::TrackParamInfo>& params)
{
    /*
     * ONLY THE NEXT PAGE COUNTS. The pages are broadcast, and the language
     * runtime hears them too (track_control checks names against them): a
     * page it asked for arrives here as well. Taking every page and asking
     * for the one after it, whoever it was for, is how two listeners turned
     * Surge's sixty pages into seventeen thousand — each page requested
     * twice, then four times. A page that is not the one this list is
     * waiting for is dropped; a list that is whole stays whole.
     */
    ParamList& pl = m_params[handle];
    pl.requested = true;
    pl.total = total;
    if (offset == 0 && pl.got > 0 && !pl.complete())
    {
        // The first page of a fresh listing while ours is part way: start over.
        pl.byId.clear();
        pl.order.clear();
        pl.got = 0;
    }
    if (offset != pl.got || pl.complete()) return;
    for (const SonicPi::TrackParamInfo& p : params)
    {
        if (!pl.byId.contains(p.id)) pl.order.push_back(p.id);
        pl.byId.insert(p.id, p);
    }
    pl.got = offset + static_cast<uint32_t>(params.size());
    // The engine sends one page at a time, so the rest is ours to ask for.
    // An empty page short of the total would loop forever; treat it as done.
    if (pl.got < pl.total && !params.empty()) requestParamsPage(handle, pl.got);
    else publishParamNames();

    if (Device* dev = deviceFor(handle))
    {
        // Values arrive with the list: dials already on the face take them,
        // so a device shows where the plugin actually is, not where a dial
        // happened to start.
        for (ParamRow& row : dev->rows)
        {
            auto it = pl.byId.find(row.id);
            if (it != pl.byId.end() && row.dial) row.dial->setValue(it->value, false);
        }
        realisePending(dev);
        setDeviceStatus(dev, deviceStatusText(dev), true);
    }
}

void TracksPanel::realisePending(Device* dev)
{
    if (!dev || dev->pending.isEmpty()) return;
    const ParamList& pl = m_params.value(dev->info.handle);
    QVector<uint32_t> still;
    bool added = false;
    for (uint32_t id : dev->pending)
    {
        if (pl.byId.contains(id)) { addCuratedParam(dev, id, false); added = true; }
        else if (!pl.complete())  still.push_back(id);
        // else: a parameter this plugin no longer has. Dropped.
    }
    dev->pending = still;
    if (added) relayoutParams(dev);
}

void TracksPanel::addCuratedParam(Device* dev, uint32_t id, bool persist)
{
    if (!dev) return;
    for (const ParamRow& r : dev->rows) if (r.id == id) return;   // already on the face

    const ParamList& pl = m_params.value(dev->info.handle);
    auto it = pl.byId.find(id);
    if (it == pl.byId.end()) return;
    const SonicPi::TrackParamInfo& p = it.value();

    ParamRow row;
    row.id = id;
    row.group = QString::fromStdString(p.groupName);

    /*
     * THE SYNTH DOCS' OWN DIAL, not one that looks like it. TutDial is the
     * painted rotary from the :bass_foundation panel — the arc, the value
     * inside it, the name beneath, the vertical drag, double-click to reset,
     * and setDocText, which puts the description into the house bubble.
     *
     * It works in the plugin's PLAIN units, because that is what a person
     * reads off a knob and what track/plugin/param takes.
     */
    const double lo = p.min;
    const double hi = (p.max > p.min) ? p.max : p.min + 1.0;
    const QString name = QString::fromStdString(p.name);
    auto* dial = new TutDial(name, lo, hi, qBound(lo, static_cast<double>(p.value), hi),
                             [this, dev, id]() { sendParamFromDial(dev, id); }, dev->frame);
    dial->setUiScale(m_fontScale);
    if (m_theme)
    {
        const QColor fg     = m_theme->color("Foreground");
        const QColor bg     = m_theme->color("Background");
        const QColor accent = m_theme->color("HighlightedBackground");
        dial->setColours(fg, SonicPiTheme::blend(fg, bg, 0.38), accent,
                         SonicPiTheme::blend(bg, fg, 0.28));
    }
    // The bubble teaches the code for the knob: the key the track verbs
    // take, and the plugin's own spelling for the one case the key is
    // taken by an opt of the verb's own.
    {
        const QString key = SonicPi::trackParamKey(name);
        const QString head = row.group.isEmpty() ? name : tr("%1 — %2").arg(name, row.group);
        const QString code = key.isEmpty()
            ? tr("track_control \"%1\", value, track: :%2").arg(name, trackNameOrEmpty())
            : tr("track_control %1: value, track: :%2").arg(key, trackNameOrEmpty());
        dial->setDocText(head + QStringLiteral("\n\n") + code);
    }
    row.dial = dial;
    dev->rows.push_back(row);
    if (persist) saveCuration(dev);
}

QString TracksPanel::trackNameOrEmpty() const
{
    const SonicPi::TrackInfo* t = selectedTrack();
    return t ? QString::fromStdString(t->name) : QStringLiteral("name");
}

void TracksPanel::sendParamFromDial(const Device* dev, uint32_t id)
{
    if (!dev || !m_spAPI) return;
    for (const ParamRow& r : dev->rows)
    {
        if (r.id != id || !r.dial) continue;
        oscpkt::Message m("/clockwork/track/plugin/param");
        m.pushInt32(dev->info.handle);
        m.pushInt32(static_cast<int32_t>(id));
        m.pushFloat(static_cast<float>(r.dial->value()));
        m_spAPI->SupersonicSendOSC(m);
        return;
    }
}

void TracksPanel::relayoutParams(Device* dev)
{
    if (!dev || !dev->paramGrid) return;

    // Take everything out of the grid without destroying the dials — the
    // cells are rebuilt in order, and a curated set that grows by one should
    // not have to reconstruct the controls that were already there. The
    // captions ARE destroyed: a widget taken out of a layout stays where it
    // was painted, and a caption left behind sits under the next one.
    while (QLayoutItem* item = dev->paramGrid->takeAt(0))
    {
        if (QWidget* w = item->widget())
            if (!qobject_cast<TutDial*>(w)) delete w;
        delete item;
    }

    int row = 0, col = 0;
    QString lastGroup;
    for (const ParamRow& r : dev->rows)
    {
        // A caption only when the plugin actually named the group. VST3 gives
        // a unit id — a hash — and the name comes across beside it. A plugin
        // that names nothing gets no caption rather than a made-up one.
        if (!r.group.isEmpty() && r.group != lastGroup)
        {
            if (col != 0) { col = 0; ++row; }
            auto* caption = new QLabel(r.group.toUpper(), dev->frame);
            caption->setObjectName(QStringLiteral("tutSection"));
            caption->setStyleSheet(QStringLiteral("font-size: %1px;")
                                       .arg(FontRolePx(FontRole::Small, m_fontScale)));
            dev->paramGrid->addWidget(caption, row, 0, 1, kDialsPerRow);
            ++row;
            lastGroup = r.group;
        }
        dev->paramGrid->addWidget(r.dial, row, col, Qt::AlignTop);
        r.dial->show();
        if (++col >= kDialsPerRow) { col = 0; ++row; }
    }

    if (dev->hint) dev->hint->setVisible(dev->rows.isEmpty());
}

void TracksPanel::onTrackParamEdit(int handle, uint32_t id, double normalized, bool own)
{
    Device* dev = deviceFor(handle);
    if (!dev) return;   // another track's plugin, or one this panel has not drawn

    for (ParamRow& row : dev->rows)
    {
        if (row.id != id || !row.dial) continue;
        // notify = false, or this bounces: TutDial would fire its onChange,
        // which sends the parameter straight back to the engine — at best
        // redundant, at worst a feedback loop while the knob is being dragged
        // in the plugin's own window.
        const double lo = row.dial->minimum();
        const double hi = row.dial->maximum();
        row.dial->setValue(lo + qBound(0.0, normalized, 1.0) * (hi - lo), false);
        return;
    }

    // NOT ON THE FACE. With Configure armed that is the request: the user has
    // chosen this control by using it, which is the only selection method that
    // scales to a plugin with 2855 parameters — you never read the list, you
    // just reach for the knob you want. Code setting it is not that reach.
    if (!dev->configuring || !own) return;
    ParamList& pl = m_params[dev->info.handle];
    if (pl.byId.contains(id))
    {
        // The plugin's own value is normalized here; the dial wants plain.
        SonicPi::TrackParamInfo& p = pl.byId[id];
        p.value = static_cast<float>(p.min + qBound(0.0, normalized, 1.0) * (p.max - p.min));
        addCuratedParam(dev, id, true);
        relayoutParams(dev);
    }
    else
    {
        // The list has not arrived (or not this far). Remembered, realised
        // when it does; saved now so a restart does not lose the choice.
        if (!dev->pending.contains(id)) dev->pending.push_back(id);
        saveCuration(dev);
        requestParams(handle);
    }
}

// The armed look is a property the stylesheet reads, so it follows the
// state however the state changed — a click, or a window that failed to open.
void TracksPanel::setConfiguring(Device* dev, bool on)
{
    dev->configuring = on;
    if (!dev->configureBtn) return;
    dev->configureBtn->setChecked(on);
    dev->configureBtn->setProperty("armed", on);
    dev->configureBtn->style()->unpolish(dev->configureBtn);
    dev->configureBtn->style()->polish(dev->configureBtn);
    tintDeviceGlyph(dev->configureBtn, dev->configureBtn->underMouse());
}

void TracksPanel::toggleConfigure(Device* dev)
{
    if (!dev || !dev->configureBtn) return;
    setConfiguring(dev, dev->configureBtn->isChecked());
    if (dev->configuring)
    {
        // The window comes to the front every time, not only when it was
        // shut: the click that armed Configure put Sonic Pi over it, and a
        // mode that asks for a control to be moved in a window the user
        // cannot see is two steps for one intention. Showing an open editor
        // is how the engine raises it.
        if (dev->editorBtn) dev->editorBtn->setChecked(true);
        setEditorVisible(dev, true);
    }
    setDeviceStatus(dev, deviceStatusText(dev), true);
}

void TracksPanel::setEditorVisible(Device* dev, bool show)
{
    if (!m_spAPI || !dev) return;
    // ONE INSTANCE. The engine opens the editor of the plugin that is actually
    // making sound, in a window it owns. A plugin with no window says so by
    // track/error, and the toggle springs back then (see onTrackError).
#ifdef Q_OS_WIN
    // The editor window belongs to the plugin bridge, a background process
    // the engine spawned, and Windows refuses a background process the
    // foreground unless the foreground process — this one, right now, on the
    // user's click — grants it. Granted to any process: the bridge's pid is
    // the engine's to know, and the grant lasts only until the next input.
    allowForegroundToAnyProcess();
#endif
    oscpkt::Message m("/clockwork/track/plugin/editor");
    m.pushInt32(dev->info.handle);
    m.pushInt32(show ? 1 : 0);
    m_spAPI->SupersonicSendOSC(m);
}

// ── Status ───────────────────────────────────────────────────────────────────

void TracksPanel::setDeviceStatus(Device* dev, const QString& text, bool ok)
{
    if (!dev || !dev->status) return;
    dev->status->setText(text);
    // The error tint is the theme's. Cleared rather than left set, or a
    // recovered device keeps the colour of the failure it recovered from.
    dev->status->setProperty("tracksError", !ok);
    dev->status->style()->unpolish(dev->status);
    dev->status->style()->polish(dev->status);
}

void TracksPanel::setStatus(const QString& text, bool ok)
{
    if (!m_status) return;
    m_status->setText(text);
    m_status->setProperty("tracksError", !ok);
    m_status->style()->unpolish(m_status);
    m_status->style()->polish(m_status);
    if (!ok) emit announceRequested(text);
    // A confirmation is news, not state: it goes away by itself. An error
    // waits until the next thing the user does.
    m_statusTimer->stop();
    if (ok) m_statusTimer->start(kStatusMs);
}

void TracksPanel::clearStatus()
{
    m_statusTimer->stop();
    if (m_status) m_status->clear();
}

void TracksPanel::onTrackError(const QString& verb, const QString& detail, int handle)
{
    // Whatever was in flight is not going to land: no "is on" after a failure.
    m_statusOnNextList.clear();
    if (verb == QLatin1String("bridge"))
    {
        // The process that hosts every plugin died or hung. The engine is
        // restarting it with the tracks it had; until the list comes back
        // every device is a picture of something that is not running, and
        // says so. A scan in flight died with it.
        m_bridgeDown = true;
        if (m_scanning)
        {
            m_scanning = false;
            m_rescanButton->setEnabled(true);
            m_browserCount->setText(tr("the scan did not finish"));
        }
        for (Device* d : m_devices) setDeviceStatus(d, tr("restarting…"), false);
        setStatus(detail, false);
        return;
    }
    Device* dev = deviceFor(handle);
    if (verb == QLatin1String("plugin/editor") && dev)
    {
        // No window to show: the toggle springs back rather than lying about
        // the window's state, and so does Configure, which needs the window.
        if (dev->editorBtn) dev->editorBtn->setChecked(false);
        setConfiguring(dev, false);
        setDeviceStatus(dev, detail, false);
        return;
    }
    if (dev)
    {
        setDeviceStatus(dev, detail, false);
        return;
    }
    if (verb == QLatin1String("rig/load") && !m_tracks.empty()
        && detail.contains(QLatin1String("could not be found")))
    {
        setStatus(tr("%1 — the tracks are back, with a gap where each was. "
                     "Plugin folders... adds where to look.").arg(detail), false);
        return;
    }
    setStatus(detail, false);
}

// ── Remembering a curated face ───────────────────────────────────────────────

QVector<uint32_t> TracksPanel::savedCuration(const SonicPi::TrackNodeInfo& node) const
{
    QVector<uint32_t> ids;
    QSettings s = panelSettings();
    const QVariantList raw = s.value(curationKey(node)).toList();
    for (const QVariant& v : raw)
    {
        bool ok = false;
        const uint id = v.toUInt(&ok);
        if (ok) ids.push_back(id);
    }
    return ids;
}

void TracksPanel::saveCuration(const Device* dev) const
{
    if (!dev) return;
    QVariantList raw;
    for (const ParamRow& r : dev->rows) raw.push_back(QVariant(r.id));
    for (uint32_t id : dev->pending) raw.push_back(QVariant(id));
    QSettings s = panelSettings();
    s.setValue(curationKey(dev->info), raw);
}

// ── Theme ────────────────────────────────────────────────────────────────────

void TracksPanel::applyTheme(SonicPiTheme* theme)
{
    m_theme = theme;
    if (!theme) return;

    /*
     * NO STYLESHEET IS BUILT HERE. The parts carry the HOUSE names — qsCard*
     * for a device, docsNavPage/docsFilter/docsNavList for the two columns,
     * tutDialGroup and tutSection for the code block and captions — and the
     * panel itself is a TracksPanel rule beside QuickstartPane's, so app.qss
     * already says what all of it looks like. What is left is the one thing
     * a stylesheet cannot express: the icons, which are painted.
     */
    const QColor fg = theme->color("Foreground");
    const QColor bgc = theme->color("Background");
    const QColor accent = theme->color("HighlightedBackground");
    const int side = ScaleWidthForDPI(16);

    // The code block's glyphs sit on the panel, so they take the panel's ink;
    // a device's sit on the accent bar and take its.
    for (QPushButton* b : m_codeBlock->findChildren<QPushButton*>())
    {
        const QVariant g = b->property("glyph");
        if (!g.isValid()) continue;
        b->setIcon(TablerIcons::icon(static_cast<TablerIcons::Glyph>(g.toInt()), fg, side,
                                     devicePixelRatioF()));
    }
    for (Device* dev : m_devices)
    {
        if (!dev->frame) continue;
        for (QPushButton* b : dev->frame->findChildren<QPushButton*>())
            tintDeviceGlyph(b, b->underMouse());
        for (ParamRow& r : dev->rows)
            if (r.dial)
                r.dial->setColours(fg, SonicPiTheme::blend(fg, bgc, 0.38), accent,
                                   SonicPiTheme::blend(bgc, fg, 0.28));
    }

    for (Device* dev : m_devices) fitDeviceTitle(dev);
    // A theme changes the padding and font the columns are measured under.
    QTimer::singleShot(0, this, [this]() { fitColumns(); });
    // The dividers take the docs pane's colours: its line at rest, its hover.
    if (m_split)
        m_split->setDividerColors(theme->color("PaneBackground"), theme->color("WindowBorder"),
                                  theme->color("ScrollBarHover"));
    if (m_ampDial)
        m_ampDial->setColours(fg, SonicPiTheme::blend(fg, bgc, 0.38), accent,
                              SonicPiTheme::blend(bgc, fg, 0.28));
    if (m_scope)   // the quickstart cards' rings: accent outside, half-way in
        m_scope->setColours(accent, SonicPiTheme::blend(fg, accent, 0.5));
    refreshHeader();   // the code lines take the editor's colours
}
