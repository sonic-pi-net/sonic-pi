//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright (C) 2024 by Sam Aaron
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include "metricspanel.h"
#include "nodetreegraph.h"
#include "model/sonicpitheme.h"

#include <api/sonicpi_api.h>
#include <api/audio/server_shm.hpp>
#include <api/osc/osc_pkt.hh>

#include <atomic>
#include <chrono>
#include <cstdint>
#include <cstring>
#include <vector>

// Generated from SuperSonic's canonical metrics schema
// (js/lib/metrics_schema.js) — metric descriptions shared by every GUI.
#include "supersonic/src/metrics_schema.h"

#include <QBrush>
#include <QEasingCurve>
#include <QFont>
#include <QFrame>
#include <QGridLayout>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QHideEvent>
#include <QHostAddress>
#include <QLabel>
#include <QResizeEvent>
#include <QScrollArea>
#include <QScrollBar>
#include <QSignalBlocker>
#include <QShowEvent>
#include <QSizePolicy>
#include <QSplitter>
#include <QSplitterHandle>
#include <QStyle>
#include <QTextBlock>
#include <QTextCharFormat>
#include <QTextCursor>
#include <QTextDocument>
#include <QTextEdit>
#include <QTextFrame>
#include <QTextFrameFormat>
#include <QDateTime>
#include <QTime>
#include <QUdpSocket>
#include <QTimer>
#include <QVariantAnimation>
#include <QToolButton>
#include <QVBoxLayout>
#include <QHash>
#include <QEvent>
#include <QMouseEvent>

#include "chevronbutton.h"
#include "dpi.h"
#include "utils/reducedmotion.h"

// Custom QTextCharFormat property storing a run's colour role, so text already
// in the OSC/debug views can be re-tinted on a theme change (see recolourLogViews).
static constexpr int RoleProp = QTextFormat::UserProperty + 1;

// ─── Static layout model ────────────────────────────────────────────────
//
// Transcribed from the canonical SuperSonic web schema
// (external/supersonic/js/lib/metrics_offsets.js). Field indices 0-49 are
// the PerformanceMetrics struct (see external/supersonic/src/shared_memory.h):
// 0-8 scsynth, 9-10 OSC out, 11-14 OSC in, 15-16 debug, 17-22 ring usage/peak,
// 23-25 late timing, 26 direct-write fails, 27-38 Link, 39-45 version/audio
// config, 46-49 SuperClock readouts. Cells whose source has no native writer
// are marked `na` and always render "-".

namespace
{
constexpr int kMetricCount = 50; // meaningful PerformanceMetrics fields read (0-49; 50-51 are padding)
// Cross-platform system-info fields, written into the struct by shared C++.
constexpr int kFieldVersionMajor   = 39;
constexpr int kFieldVersionMinor   = 40;
constexpr int kFieldVersionPatch   = 41;
constexpr int kFieldSampleRate     = 42;
constexpr int kFieldBlockSize      = 43;
constexpr int kFieldOutputChannels = 44;
constexpr int kFieldInputChannels  = 45;
constexpr int kFieldProcessCount      = 0;  // audio process() callbacks (tick counter)
constexpr int kFieldSchedLastLateTick = 25; // process count when the last scsynth late fired
constexpr int kFieldClockTempo     = 46; // milli-BPM
constexpr int kFieldClockBeat      = 47; // beat * 100
constexpr int kFieldClockPhase     = 48; // phase * 100
constexpr int kFieldClockPlaying   = 49;
// Native-only live stats appended after the struct fields in the panel's value
// array (sourced from AudioProcessor_GetNativeStats, not the metrics struct).
constexpr int kFieldSynthDefs   = 50;
constexpr int kFieldBuffers     = 51;
constexpr int kFieldBufferBytes = 52;
constexpr int kFieldCpuAvg      = 53; // DSP load %, centi (native_stats)
constexpr int kFieldCpuPeak     = 54; // DSP load % peak, centi (native_stats)
constexpr int kFieldOverruns    = 55; // audio callback overruns (native_stats)
constexpr int kPanelFieldCount  = 56;

// Poll cadence while visible (~6-7 Hz).
constexpr int kRefreshMs = 150;

// Minimum sensible width for a metric card. The grid snaps to a single row of
// all cards when the pane is at least (card count * this) wide, otherwise two
// rows; the pane is then pinned to exactly the height those rows need. Set so
// the two-row layout is preferred until the pane is genuinely wide.
constexpr int kCardMinW = 112;

// Ring capacities, mirrored from external/supersonic/src/memory_profile.h
// (IN/OUT/NRT_OUT_BUFFER_SIZE); used to scale the ring usage % readouts.
constexpr uint32_t kInBufferCap = 786432;  // 768 KB
constexpr uint32_t kOutBufferCap = 131072; // 128 KB
constexpr uint32_t kNrtOutBufferCap = 65536; // 64 KB

enum Fmt
{
    F_Plain,
    F_Bytes,
    F_Signed,
    F_Headroom,
    F_MilliBpm,   // raw is milli-BPM (bpm * 1000) → "X.X"
    F_Centi       // raw is value * 100 → "X.XX"
};

enum Kind
{
    K_Normal,
    K_Muted,
    K_Dim,
    K_Error
};

struct Seg
{
    bool isText;       // literal separator/suffix vs a metric value
    const char* text;  // literal text when isText
    int field;         // metric index 0-49, or -1
    Fmt fmt;
    Kind kind;
    bool na;           // force "-" (no native writer)
    bool nativeOnly;   // render "-" when the segment carries no native stats
};

struct RowDef
{
    const char* label;
    std::vector<Seg> segs;
    // Ring usage rows (in/out/nrt) render "used% / peak%" computed from these
    // fields against the ring capacity, instead of formatting `segs`. cap == 0
    // marks a plain value row.
    int usedField;
    int peakField;
    uint32_t cap;
    // Age rows render "nowField - thenField ticks" (nowField >= 0); "-" when
    // thenField is 0 (event has never fired this session).
    int nowField;
    int thenField;
    const char* tip;   // tooltip body explaining the metric (untranslated,
                       // like the row labels — this is a developer panel)
};

struct PanelDef
{
    const char* title;
    std::vector<RowDef> rows;
};

// Cell factories.
Seg V(int f, Kind k = K_Normal, Fmt fmt = F_Plain) { return Seg{ false, "", f, fmt, k, false, false }; }
// Native-only value: renders "-" (not 0) on a segment with no native stats.
Seg Vn(int f, Kind k = K_Normal, Fmt fmt = F_Plain) { return Seg{ false, "", f, fmt, k, false, true }; }
Seg T(const char* t, Kind k = K_Muted) { return Seg{ true, t, -1, F_Plain, k, false, false }; }

RowDef ValRow(const char* label, std::vector<Seg> segs, const char* tip = "")
{
    return RowDef{ label, std::move(segs), -1, -1, 0, -1, -1, tip };
}
// Ring usage % readout: "used% / peak%" of the ring capacity (no bar graphic).
RowDef PctRow(const char* label, int used, int peak, uint32_t cap, const char* tip = "")
{
    return RowDef{ label, {}, used, peak, cap, -1, -1, tip };
}
// Age readout: "nowField - thenField ticks" (process calls since an event).
RowDef AgeRow(const char* label, int nowF, int thenF, const char* tip = "")
{
    return RowDef{ label, {}, -1, -1, 0, nowF, thenF, tip };
}

// Field indices match the struct order in shared_memory.h; late-ms fields
// (23, 24) and Link-audio drift (36) are int32 so they use F_Signed.
// Browser-only metrics (7 wasm_errors, 13 osc_in_dropped, 26 direct-write
// fails) are omitted (no native writer).
const std::vector<PanelDef>& panelLayout()
{
    // All row tooltips come from SuperSonic's canonical metrics schema (see
    // supersonic/js/lib/metrics_schema.js → generated metrics_schema.h): a
    // row with no tip falls back to the schema description of its first
    // metric field, and rows combining several fields name a schema
    // composite — mirroring the <supersonic-metrics> web component.
    const auto composite = supersonic::metrics_schema::descriptionForComposite;
    static const std::vector<PanelDef> panels = {
        { "Engine",
          { ValRow("version", { V(kFieldVersionMajor), T("."), V(kFieldVersionMinor), T("."), V(kFieldVersionPatch) },
                   composite("engineVersion")),
            ValRow("rate", { V(kFieldSampleRate), T(" Hz") }),
            ValRow("block", { V(kFieldBlockSize), T(" frames") }),
            ValRow("channels", { V(kFieldOutputChannels), T(" | "), V(kFieldInputChannels, K_Muted) },
                   composite("busChannelsOutIn")),
            ValRow("ticks", { V(0, K_Dim) }) } },
        { "OSC",
          { ValRow("sent", { V(9), T(" | "), V(10, K_Muted, F_Bytes) },
                   composite("oscSentCountBytes")),
            ValRow("recv", { V(11), T(" | "), V(12, K_Muted, F_Bytes) },
                   composite("oscRecvCountBytes")),
            PctRow("in", 17, 20, kInBufferCap, composite("inRingUsedPeak")),
            PctRow("out rt", 18, 21, kOutBufferCap, composite("outRingUsedPeak")),
            PctRow("out nrt", 19, 22, kNrtOutBufferCap, composite("nrtRingUsedPeak")) } },
        { "Clock",
          { ValRow("tempo", { V(kFieldClockTempo, K_Normal, F_MilliBpm), T(" bpm") }),
            ValRow("beat", { V(kFieldClockBeat, K_Dim, F_Centi) }),
            ValRow("phase", { V(kFieldClockPhase, K_Dim, F_Centi) }),
            ValRow("playing", { V(kFieldClockPlaying, K_Muted) }),
            ValRow("peers", { V(27) },
                   "Connected Ableton Link peers (0 = local session, not network-synced)") } },
        { "DSP",
          { ValRow("load", { Vn(kFieldCpuAvg, K_Normal, F_Centi), T("%") }),
            ValRow("peak", { Vn(kFieldCpuPeak, K_Dim, F_Centi), T("%") }),
            ValRow("overruns", { Vn(kFieldOverruns, K_Error) }) } },
        { "Link Audio",
          { ValRow("in", { V(32), T(" ch @ "), V(33, K_Muted), T(" Hz") },
                   composite("linkAudioChannelsRate")),
            ValRow("underruns", { V(34, K_Error) }),
            ValRow("buffered", { V(35, K_Dim), T(" ms") }),
            ValRow("drift", { V(36, K_Dim, F_Signed), T(" ppm") }),
            ValRow("publish", { V(37), T(" | "), V(38, K_Muted), T(" sinks") },
                   composite("linkAudioPublishSinks")) } },
        { "scsynth",
          { ValRow("msgs", { V(1, K_Muted) }),
            ValRow("queue", { V(3), T(" | "), V(4, K_Muted) },
                   composite("schedulerQueueCurrentPeak")),
            ValRow("max|last", { V(23, K_Error, F_Signed), T(" | "), V(24, K_Dim, F_Signed), T(" ms") },
                   composite("schedulerLateWorstLast")),
            AgeRow("late age", kFieldProcessCount, kFieldSchedLastLateTick,
                   "Process calls since the last scheduler late (larger = longer ago; - = none this session)"),
            ValRow("debug", { V(15, K_Muted), T(" ("), V(16, K_Muted, F_Bytes), T(")") },
                   composite("debugCountBytes")) } },
        { "Buffers",
          { ValRow("synthdefs", { Vn(kFieldSynthDefs) }),
            ValRow("buffers", { Vn(kFieldBuffers) }),
            ValRow("buf bytes", { Vn(kFieldBufferBytes, K_Muted, F_Bytes) }) } },
        { "Errors",
          { ValRow("dropped", { V(2, K_Error) }),
            ValRow("q drop", { V(5, K_Error) }),
            ValRow("seq gaps", { V(6, K_Error) }),
            ValRow("lates", { V(8, K_Error) }),
            ValRow("corrupt", { V(14, K_Error) }) } },
    };
    return panels;
}

QString formatBytes(uint32_t bytes)
{
    if (bytes < 1024)
        return QString::number(bytes) + " B";
    double kb = bytes / 1024.0;
    if (kb < 1024.0)
        return QString::number(kb, 'f', 1) + " KB";
    double mb = kb / 1024.0;
    if (mb < 1024.0)
        return QString::number(mb, 'f', 1) + " MB";
    return QString::number(mb / 1024.0, 'f', 1) + " GB";
}

QString formatField(uint32_t raw, Fmt fmt)
{
    switch (fmt)
    {
    case F_Bytes:
        return formatBytes(raw);
    case F_Signed:
        return QString::number(static_cast<int32_t>(raw));
    case F_Headroom:
        return raw == 0xFFFFFFFFu ? QStringLiteral("-") : QString::number(raw);
    case F_MilliBpm:
        return QString::number(raw / 1000.0, 'f', 1);
    case F_Centi:
        return QString::number(raw / 100.0, 'f', 2);
    case F_Plain:
    default:
        return QString::number(raw);
    }
}

QColor blend(const QColor& a, const QColor& b, double t)
{
    return QColor(int(a.red() * t + b.red() * (1 - t)),
                  int(a.green() * t + b.green() * (1 - t)),
                  int(a.blue() * t + b.blue() * (1 - t)));
}

QFont makeMonoFont()
{
    QFont mono("Hack", 8, -1, false);
    mono.setStyleHint(QFont::Monospace);
    mono.setFixedPitch(true);
    return mono;
}

// Card frame with an uppercase title. bodyOut receives the inner layout to
// populate; titleOut the title label (registered for theme recolouring).
QFrame* makeCard(const QString& title, QVBoxLayout** bodyOut, QLabel** titleOut)
{
    auto* card = new QFrame;
    card->setObjectName("ssCard");
    auto* v = new QVBoxLayout(card);
    v->setContentsMargins(5, 3, 5, 4);
    v->setSpacing(2);

    auto* lbl = new QLabel(title.toUpper());
    lbl->setObjectName("ssCardTitle");
    v->addWidget(lbl);

    if (titleOut) *titleOut = lbl;
    if (bodyOut) *bodyOut = v;
    return card;
}
} // namespace

// ─── MetricsPanel ───────────────────────────────────────────────────────

MetricsPanel::MetricsPanel(std::shared_ptr<SonicPi::SonicPiAPI> api, QWidget* parent)
    : QWidget(parent)
    , m_api(std::move(api))
{
    m_textColor = QColor("#cccccc");
    m_bgColor = QColor("#1e1e1e");
    m_borderColor = QColor("#444444");

    m_timer = new QTimer(this);
    m_timer->setInterval(kRefreshMs);
    connect(m_timer, &QTimer::timeout, this, &MetricsPanel::refresh);

    buildUi();
}

QSize MetricsPanel::sizeHint() const { return QSize(900, 340); }
QSize MetricsPanel::minimumSizeHint() const { return QSize(360, 110); }

QColor MetricsPanel::kindColor(int kind) const
{
    switch (kind)
    {
    case K_Muted:
        return blend(m_textColor, m_bgColor, 0.45);
    case K_Dim:
        return blend(m_textColor, m_bgColor, 0.70);
    case K_Error:
        // K_Error renders as normal text (no red).
        return m_textColor;
    case K_Normal:
    default:
        return m_textColor;
    }
}

QColor MetricsPanel::colorForRole(const QString& role) const
{
    if (role == QLatin1String("kdim"))   return kindColor(K_Dim);
    if (role == QLatin1String("kmuted")) return kindColor(K_Muted);
    return m_theme ? m_theme->color(role) : m_textColor;
}

void MetricsPanel::recolourLogViews()
{
    QTextEdit* views[] = { m_oscOutView, m_oscInView, m_debugView };
    for (QTextEdit* v : views)
    {
        if (!v) continue;
        QTextDocument* doc = v->document();
        QTextCursor edit(doc);
        edit.beginEditBlock();
        for (QTextBlock block = doc->begin(); block.isValid(); block = block.next())
        {
            for (QTextBlock::iterator it = block.begin(); !it.atEnd(); ++it)
            {
                const QTextFragment frag = it.fragment();
                if (!frag.isValid()) continue;
                QTextCharFormat f = frag.charFormat();
                if (!f.hasProperty(RoleProp)) continue;
                f.setForeground(colorForRole(f.property(RoleProp).toString()));
                QTextCursor fc(doc);
                fc.setPosition(frag.position());
                fc.setPosition(frag.position() + frag.length(), QTextCursor::KeepAnchor);
                fc.setCharFormat(f);
            }
        }
        edit.endEditBlock();
    }
}

void MetricsPanel::buildUi()
{
    auto* outer = new QVBoxLayout(this);
    outer->setContentsMargins(0, 0, 0, 0);
    outer->setSpacing(0);

    QFont mono = makeMonoFont();

    // Left: node tree + metric grid. Right: debug + OSC in/out logs.
    auto* mainRow = new ThinSplitter(Qt::Horizontal, this);
    m_mainSplit = mainRow;
    outer->addWidget(mainRow);

    auto* leftCol = new QSplitter(Qt::Vertical);
    m_leftSplit = leftCol;
    buildNodeColumn(leftCol);

    auto* scroll = new QScrollArea;
    scroll->setWidgetResizable(true);
    scroll->setFrameShape(QFrame::NoFrame);
    // Horizontal AsNeeded keeps the scroll area's minimum width small so the
    // left column can shrink and the main divider stays freely draggable; the
    // grid scrolls sideways when narrow.
    scroll->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    // The pane is pinned to its content's height (updateMetricsHeight), so no
    // vertical scrolling is ever needed.
    scroll->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);

    auto* content = new QWidget;
    content->setObjectName("ssZones");
    auto* grid = new QGridLayout(content);
    grid->setContentsMargins(0, 0, 0, 0);
    grid->setHorizontalSpacing(0);
    grid->setVerticalSpacing(0);

    // Builds one value/bar row into `rows` at row index `r`, registering it for
    // refresh().
    auto addRow = [&](QGridLayout* rows, int r, const RowDef& row, const char* panelTitle) {
        // Tooltip explaining the metric, applied to every widget in the row.
        // Title is "card · row" so the popup names the (cryptic) metric.
        // Body: the row's explicit tip, else the schema description of its
        // first metric field (fields >= kFieldSynthDefs live in the
        // NATIVE_STATS segment — see the panel's field remapping above).
        const char* tip = (row.tip && *row.tip) ? row.tip : nullptr;
        if (!tip)
        {
            int f = -1;
            if (row.cap > 0)   // ring usage % row
                f = row.usedField;
            else if (row.nowField >= 0)   // age row → describe the event field
                f = row.thenField;
            else
                for (const Seg& seg : row.segs)
                {
                    if (!seg.isText)
                    {
                        f = seg.field;
                        break;
                    }
                }
            if (f >= kFieldSynthDefs)
                tip = supersonic::metrics_schema::descriptionForNativeStat(f - kFieldSynthDefs);
            else if (f >= 0)
                tip = supersonic::metrics_schema::descriptionForOffset(f);
        }
        const QString tipBody = tip ? QString::fromUtf8(tip) : QString();
        const QString tipTitle = QString::fromUtf8(panelTitle)
            + QStringLiteral(" · ") + QString::fromUtf8(row.label);
        auto applyTip = [&](QWidget* w) {
            if (!tipBody.isEmpty())
            {
                w->setProperty("tipTitle", tipTitle);
                w->setToolTip(tipBody);
            }
        };

        auto* lbl = new QLabel(QString::fromUtf8(row.label));
        lbl->setFont(mono);
        lbl->setProperty("ssRole", "rowlabel");
        applyTip(lbl);
        m_rowLabels.append(lbl);
        rows->addWidget(lbl, r, 0, Qt::AlignLeft);

        auto* val = new QLabel;
        val->setFont(mono);
        applyTip(val);
        val->setTextFormat(Qt::RichText);
        val->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
        // Don't let the value's width drive the card width — otherwise a digit
        // crossing (e.g. 9→10, or a growing counter) reflows the whole grid.
        val->setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Preferred);
        rows->addWidget(val, r, 1, 1, 2, Qt::AlignRight);
        m_valueRows.append({ &row, val, QString() });
    };

    // Build the cards; their placement in the grid is done by reflowMetricsGrid
    // so they can re-flow by the available height (a single row when short).
    const auto& panels = panelLayout();
    const int nPanels = static_cast<int>(panels.size());
    for (int p = 0; p < nPanels; ++p)
    {
        const PanelDef& panel = panels[p];
        QVBoxLayout* body = nullptr;
        QLabel* title = nullptr;
        QFrame* card = makeCard(QString::fromUtf8(panel.title), &body, &title);
        card->setObjectName("ssCell");
        card->setFont(mono);
        title->setFont(mono);
        m_rowLabels.append(title);

        auto* rows = new QGridLayout;
        rows->setContentsMargins(0, 0, 0, 0);
        rows->setHorizontalSpacing(5);
        rows->setVerticalSpacing(1);
        rows->setColumnStretch(1, 1);
        int r = 0;
        for (const RowDef& row : panel.rows)
            addRow(rows, r++, row, panel.title);
        body->addLayout(rows);
        body->addStretch(1);

        m_metricsCards.append(card);
    }
    m_metricsGrid = grid;
    m_metricsScroll = scroll;
    reflowMetricsGrid(5);   // initial: the full five-column grid

    scroll->setWidget(content);
    scroll->viewport()->installEventFilter(this);   // re-flow when the pane height changes

    leftCol->addWidget(scroll);
    mainRow->addWidget(leftCol);

    auto* rightCol = new ThinSplitter(Qt::Vertical);
    m_rightSplit = rightCol;
    buildLogs(rightCol);
    mainRow->addWidget(rightCol);

    mainRow->setStretchFactor(0, 618);  // (tree + metrics) : logs ≈ golden ratio
    mainRow->setStretchFactor(1, 382);
    // A wide grab area whose centre is a thin 2px line at rest, revealed to the
    // full width on hover (ThinSplitter). Fixed px (not DPI-scaled) so the reveal
    // stays clearly wider than the resting line on macOS's sub-1.0 display scale.
    const int kHandleW = 7;
    const int kLineW = 2;
    mainRow->setHandleWidth(kHandleW);
    mainRow->setChildrenCollapsible(false);

    // Both vertical columns are draggable, and laid out by revealColumns()
    // until the user drags a divider (then that column is left to the user).
    for (QSplitter* col : { leftCol, static_cast<QSplitter*>(rightCol) })
    {
        // Non-collapsible so a zero-height pane stays visible and keeps its
        // handle, leaving the divider bar (and its chevron) draggable even when
        // the metrics are minimised to nothing.
        col->setChildrenCollapsible(false);
        col->setHandleWidth(kHandleW);
        for (int i = 0; i < col->count(); ++i)
        {
            QWidget* child = col->widget(i);
            // Let the card layouts shrink below their content width, and give
            // each pane a small explicit minimum, so columns stay narrow enough
            // for the main divider to move. Ignored vertical policy + a zero
            // height floor let the reveal ease a pane up from nothing.
            if (QLayout* l = child->layout())
                l->setSizeConstraint(QLayout::SetNoConstraint);
            child->setMinimumSize(80, 0);
            QSizePolicy sp = child->sizePolicy();
            sp.setVerticalPolicy(QSizePolicy::Ignored);
            child->setSizePolicy(sp);
        }
        col->installEventFilter(this);   // track height changes to re-reveal
    }

    // Chevron grip on the node-tree / metrics divider. Parented to the panel,
    // not the splitter (a QSplitter would adopt a child widget as a pane), so
    // it floats as an overlay, positioned onto the divider by
    // positionMetricsToggle().
    // Spans the whole tree/metrics divider (sized/placed in positionMetricsToggle)
    // so the divider line and its glyph are one button: hover or click anywhere
    // on the divider hits it. A click toggles the metrics' visibility.
    m_metricsToggle = new ChevronButton(this);
    // Thin divider line across the full width + a 48px knob box on the right
    // (6px inset) holding the triangle — restores the box look while keeping the
    // whole divider as one hover/click target. On hover the line reveals to the
    // handle width (kHandleW), matching the ThinSplitter dividers.
    m_metricsToggle->setBox(kLineW, 48, 6, ChevronButton::Horizontal, kHandleW);
    connect(m_metricsToggle, &QToolButton::clicked, this, &MetricsPanel::toggleMetrics);
    updateChevron();

    // Vertical chevron on the main (left columns | logs) divider — the rotated
    // sibling of the metrics chevron. A knob centred on the divider: clicking it
    // (or double-clicking the divider line, handled in eventFilter) minimises the
    // logs column off to the right and restores it. The line itself stays
    // draggable for resizing, so the knob is short rather than full-height.
    m_logsToggle = new ChevronButton(this);
    m_logsToggle->setBox(kLineW, 48, 0, ChevronButton::Vertical, kHandleW);
    connect(m_logsToggle, &QToolButton::clicked, this, &MetricsPanel::toggleLogs);
    updateLogsChevron();
    // The short logs knob overlays the main divider handle; filter both so their
    // hover states stay in sync and they reveal as one control (as the full-width
    // metrics chevron does over its own divider).
    if (QSplitterHandle* mainHandle = mainRow->handle(1))
        mainHandle->installEventFilter(this);
    m_logsToggle->installEventFilter(this);

    // Dragging a right-column divider takes it out of auto-reveal (so the drag
    // isn't undone on the next dock resize). The left column's metrics pane is
    // fixed-height (updateMetricsHeight), so its divider doesn't move — no
    // manual tracking is needed there.
    connect(rightCol, &QSplitter::splitterMoved, this, [this](int, int) {
        m_rightManual = true;
    });

    renderDisconnected();
}

// ─── Transport-ring tailing (OSC in/out + debug) ────────────────────────
//
// Mirror of the engine's Message framing (server_shm wire contract): a 16-byte
// header then payload, in a byte ring with wrap + PADDING_MAGIC end-marker. We
// read passively with our own cursor and never touch the engine's tail, so
// observation is best-effort (a slow reader may be lapped — we resync to head).

namespace
{
struct ShmMessage
{
    uint32_t magic;
    uint32_t length;     // total frame size incl. header
    uint32_t sequence;
    uint32_t source_id;
};
constexpr uint32_t kMsgMagic = 0xDEADBEEFu;
constexpr uint32_t kPadMagic = 0xBADDCAFEu;

template <typename Cursor, typename Fn>
void walkRing(const ring_view& rv, Cursor& cur, std::vector<uint8_t>& scratch, Fn&& onFrame)
{
    if (!rv.base || !rv.head) { cur.primed = false; return; }
    const uint32_t size = rv.size;
    const uint32_t head = static_cast<uint32_t>(rv.head->load(std::memory_order_acquire));
    if (!cur.primed) { cur.pos = static_cast<int32_t>(head); cur.primed = true; return; }

    uint32_t pos = static_cast<uint32_t>(cur.pos);
    int guard = 0;
    while (pos != head && guard++ < 8192)
    {
        uint32_t avail = (head - pos + size) % size;
        if (avail < sizeof(ShmMessage)) break;

        ShmMessage hdr;
        uint32_t first = size - pos;
        if (sizeof(ShmMessage) <= first)
            std::memcpy(&hdr, rv.base + pos, sizeof(ShmMessage));
        else {
            std::memcpy(&hdr, rv.base + pos, first);
            std::memcpy(reinterpret_cast<uint8_t*>(&hdr) + first, rv.base, sizeof(ShmMessage) - first);
        }

        if (hdr.magic == kPadMagic) { pos = 0; continue; }
        if (hdr.magic != kMsgMagic) { pos = head; break; }            // lapped/corrupt → resync
        uint32_t total = hdr.length;
        if (total < sizeof(ShmMessage) || total > size) { pos = head; break; }
        if (avail < total) break;                                     // partial frame; wait

        uint32_t paySize  = total - sizeof(ShmMessage);
        uint32_t payStart = (pos + sizeof(ShmMessage)) % size;
        scratch.resize(paySize);
        uint32_t pfirst = size - payStart;
        if (paySize <= pfirst)
            std::memcpy(scratch.data(), rv.base + payStart, paySize);
        else {
            std::memcpy(scratch.data(), rv.base + payStart, pfirst);
            std::memcpy(scratch.data() + pfirst, rv.base, paySize - pfirst);
        }

        onFrame(hdr.sequence, hdr.source_id, scratch.data(), paySize);
        pos = (pos + total) % size;
    }
    cur.pos = static_cast<int32_t>(pos);
}
} // namespace

void MetricsPanel::buildNodeColumn(QSplitter* topRow)
{
    QFont mono = makeMonoFont();

    QVBoxLayout* body = nullptr;
    QLabel* title = nullptr;
    QFrame* card = makeCard(tr("Node Tree"), &body, &title);
    card->setObjectName("ssCardFlat");  // borderless
    card->setFont(mono);
    title->setFont(mono);
    m_rowLabels.append(title);

    // Legend + live counts (populated in updateNodeTree()).
    m_treeStats = new QLabel(card);
    m_treeStats->setFont(mono);
    m_treeStats->setTextFormat(Qt::RichText);
    body->addWidget(m_treeStats);

    m_nodeGraph = new NodeTreeGraph(card);
    body->addWidget(m_nodeGraph, 1);
    topRow->addWidget(card);
}

namespace
{
// Read-only log view that behaves like a terminal: newest lines sit at the
// bottom. When the text is taller than the view it scrolls and stays pinned to
// the bottom (unless the user scrolls up); when it's shorter, a top margin
// pushes it down so the empty space is above, not below.
class LogView : public QTextEdit
{
public:
    explicit LogView(QWidget* parent = nullptr) : QTextEdit(parent)
    {
        QScrollBar* sb = verticalScrollBar();
        // Track whether the view is parked at the bottom — but only from genuine
        // scrolls, not our own programmatic setValue()s (guarded by m_adjusting),
        // so layout churn can't silently un-pin a quiet pane.
        connect(sb, &QScrollBar::valueChanged, this, [this](int v) {
            if (m_adjusting) return;
            m_pinned = v >= verticalScrollBar()->maximum() - 2;
        });
        // A large one-shot insert (e.g. the Info pane's boot summary) updates the
        // scrollbar range asynchronously, so updateBottomFill()'s setValue(maximum)
        // can land short of the true bottom. A busy pane (To/From) is nudged the
        // rest of the way by the next line a tick later; a quiet pane (Info) would
        // sit stranded mid-history. Re-assert the bottom once the range catches up,
        // while still pinned.
        connect(sb, &QScrollBar::rangeChanged, this, [this](int, int max) {
            if (!m_pinned || m_adjusting) return;
            QScrollBar* s = verticalScrollBar();
            if (s->value() != max) {
                const QSignalBlocker block(s);   // re-pin without re-evaluating m_pinned
                s->setValue(max);
            }
        });
        connect(document(), &QTextDocument::contentsChanged, this,
                [this]() { updateBottomFill(); });
    }

protected:
    void resizeEvent(QResizeEvent* e) override
    {
        QTextEdit::resizeEvent(e);
        updateBottomFill();
    }

private:
    void updateBottomFill()
    {
        // Mutating the document below re-fires contentsChanged, which re-enters
        // this slot; without this guard that recursion is unbounded and blows
        // the stack (a silent crash on startup). The QSignalBlocker is the
        // primary defence; the bool guards the resizeEvent path too.
        if (m_adjusting)
            return;
        m_adjusting = true;

        QScrollBar* sb = verticalScrollBar();
        const bool atBottom = m_pinned || sb->value() >= sb->maximum() - 2;

        // Bottom-align short content by pushing it down with a top margin on the
        // document's root frame (the widget won't reset this, unlike the
        // viewport margins). Measure the content's natural height with the
        // margin zeroed first, so the gap can't drift across updates.
        if (QTextFrame* root = document()->rootFrame())
        {
            const QSignalBlocker block(document());
            QTextFrameFormat fmt = root->frameFormat();
            if (fmt.topMargin() != 0)
            {
                fmt.setTopMargin(0);
                root->setFrameFormat(fmt);
            }
            const int contentH = document()->size().toSize().height();
            const int gap = qMax(0, viewport()->height() - contentH);
            if (gap != 0)
            {
                fmt.setTopMargin(gap);
                root->setFrameFormat(fmt);
            }
        }
        if (atBottom)
        {
            sb->setValue(sb->maximum());
            m_pinned = true;   // committed to the bottom; the rangeChanged re-assert relies on this
        }

        m_adjusting = false;
    }

    bool m_pinned = true;
    bool m_adjusting = false;
};
} // namespace

void MetricsPanel::buildLogs(QSplitter* col)
{
    QFont mono = makeMonoFont();

    auto addLogCard = [&](const QString& title) -> QTextEdit* {
        QVBoxLayout* body = nullptr;
        QLabel* t = nullptr;
        QFrame* card = makeCard(title, &body, &t);
        card->setObjectName("ssCardFlat");  // borderless
        // Drop the card body's right margin so the log's scrollbar sits flush to
        // the panel edge, matching the editor / output-log panes (their bars are
        // flush + the shared 2dx handle inset). Left inset stays for the text.
        body->setContentsMargins(5, 3, 0, 4);
        card->setFont(mono);
        t->setFont(mono);
        m_rowLabels.append(t);

        auto* view = new LogView(card);
        view->setReadOnly(true);
        view->setFont(mono);
        view->setLineWrapMode(QTextEdit::NoWrap);
        view->setFrameShape(QFrame::NoFrame);
        view->document()->setMaximumBlockCount(2000);  // bound memory
        body->addWidget(view, 1);
        col->addWidget(card);
        return view;
    };
    m_debugView  = addLogCard(tr("Info"));

    // Seed with the engine's own boot banner so the pane doesn't start
    // empty (matches what SuperSonic prints to its log on boot).
    m_debugView->setPlainText(
        QStringLiteral("░█▀▀░█░█░█▀█░█▀▀░█▀▄░█▀▀░█▀█░█▀█░▀█▀░█▀▀\n")
      + QStringLiteral("░▀▀█░█░█░█▀▀░█▀▀░█▀▄░▀▀█░█░█░█░█░░█░░█░░\n")
      + QStringLiteral("░▀▀▀░▀▀▀░▀░░░▀▀▀░▀░▀░▀▀▀░▀▀▀░▀░▀░▀▀▀░▀▀▀"));

    m_oscOutView = addLogCard(tr("To SuperSonic"));    // host → engine (what Sonic Pi sent)
    m_oscInView  = addLogCard(tr("From SuperSonic"));  // engine → host (replies)
}

// Wall-clock drain stamp with microsecond resolution. QTime is millisecond-only,
// so take a single std::chrono reading and derive both the HH:mm:ss (via Qt, for
// local-tz/DST correctness) and the µs-of-second fraction from the same value.
static QString hiResStamp()
{
    const int64_t us =
        std::chrono::duration_cast<std::chrono::microseconds>(
            std::chrono::system_clock::now().time_since_epoch())
            .count();
    // HH:mm:ss.mmm.uuu — split the sub-second part into millis and micros so the
    // groups are readable at a glance (e.g. 09:13:39.966.142).
    return QDateTime::fromMSecsSinceEpoch(us / 1000)
               .time()
               .toString(QStringLiteral("HH:mm:ss"))
         + QStringLiteral(".%1.%2")
               .arg((us / 1000) % 1000, 3, 10, QLatin1Char('0'))
               .arg(us % 1000, 3, 10, QLatin1Char('0'));
}

// The drain timestamp as coloured runs (using the metric rows' two greys): the
// main time HH:mm:ss.mmm in the brighter grey, the trailing microseconds — the
// least-significant, noisiest part — in the dimmer grey so they recede.
QVector<LogRun> MetricsPanel::stampRuns() const
{
    const QColor bright = kindColor(K_Dim);    // main time
    const QColor dim    = kindColor(K_Muted);  // microseconds
    const QString s = hiResStamp();            // HH:mm:ss.mmm.uuu
    const int lastDot = s.lastIndexOf(QLatin1Char('.'));
    QVector<LogRun> runs;
    if (lastDot < 0) {
        runs.append({ bright, QLatin1Char('[') + s + QLatin1Char(']'), QStringLiteral("kdim") });
        return runs;
    }
    runs.append({ bright, QLatin1Char('[') + s.left(lastDot), QStringLiteral("kdim") });   // [HH:mm:ss.mmm
    runs.append({ dim,    s.mid(lastDot), QStringLiteral("kmuted") });                      // .uuu
    runs.append({ bright, QStringLiteral("]"), QStringLiteral("kdim") });
    return runs;
}

QVector<LogRun> MetricsPanel::formatOscRuns(const uint8_t* data, uint32_t size,
                                            uint32_t sourceId)
{
    // Sonic Pi theme syntax colours (fall back to fixed hues pre-theme). Built as
    // coloured runs (not HTML) so the log inserts them via QTextCharFormat,
    // skipping the rich-text HTML parser on this per-message hot path.
    auto tc = [&](const char* name, const char* fallback) -> QColor {
        return m_theme ? m_theme->color(name) : QColor(QString::fromLatin1(fallback));
    };
    const QColor cMuted = m_theme ? m_theme->color("CommentForeground") : kindColor(K_Muted);
    const QColor cSrc  = tc("KeywordForeground", "#e0af68");
    const QColor cAddr = tc("FunctionMethodNameForeground", "#ff5fff");  // deep pink
    const QColor cNum  = tc("NumberForeground", "#ff9e64");
    const QColor cStr  = tc("DoubleQuotedStringForeground", "#9ece6a");
    // Roles (theme keys) so these runs re-tint when the theme/hue changes.
    const QString rMuted = QStringLiteral("CommentForeground");
    const QString rSrc   = QStringLiteral("KeywordForeground");
    const QString rAddr  = QStringLiteral("FunctionMethodNameForeground");
    const QString rNum   = QStringLiteral("NumberForeground");
    const QString rStr   = QStringLiteral("DoubleQuotedStringForeground");

    // Drain-time stamp (no engine timestamp in the ring header); drop detection lives in the Errors panel.
    QVector<LogRun> runs = stampRuns();
    if (sourceId != 0)
        runs.append({ cSrc, QStringLiteral(" ch%1").arg(sourceId, 3, 10, QLatin1Char('0')), rSrc });

    oscpkt::PacketReader pr(data, size);
    oscpkt::Message* msg;
    int count = 0;
    while (pr.isOk() && (msg = pr.popMessage()) != nullptr)
    {
        if (count++ > 0) runs.append({ cMuted, QStringLiteral(" |"), rMuted });
        runs.append({ cAddr, QLatin1Char(' ') + QString::fromStdString(msg->addressPattern()), rAddr });
        oscpkt::Message::ArgReader ar = msg->arg();
        while (ar.nbArgRemaining() && ar.isOk())
        {
            if (ar.isInt32())      { int32_t i; ar.popInt32(i); runs.append({ cNum, QLatin1Char(' ') + QString::number(i), rNum }); }
            else if (ar.isInt64()) { int64_t i; ar.popInt64(i); runs.append({ cNum, QLatin1Char(' ') + QString::number(static_cast<qlonglong>(i)), rNum }); }
            else if (ar.isFloat()) { float f;   ar.popFloat(f); runs.append({ cNum, QLatin1Char(' ') + QString::number(f, 'g', 6), rNum }); }
            else if (ar.isDouble()){ double d;  ar.popDouble(d); runs.append({ cNum, QLatin1Char(' ') + QString::number(d, 'g', 6), rNum }); }
            else if (ar.isStr())   { std::string s; ar.popStr(s); runs.append({ cStr, QStringLiteral(" \"") + QString::fromStdString(s) + QLatin1Char('"'), rStr }); }
            else if (ar.isBlob())  { std::vector<char> b; ar.popBlob(b); runs.append({ cMuted, QStringLiteral(" <%1 bytes>").arg(b.size()), rMuted }); }
            else                   { ar.pop(); runs.append({ cMuted, QStringLiteral(" ?"), rMuted }); }
        }
    }
    return runs;
}

// Flood cap: under heavy traffic we'd format and insert thousands of lines per
// refresh that maximumBlockCount(2000) trims away the same frame. Keep only the
// most recent and note how many were dropped — bounds the per-refresh relayout.
static constexpr int kMaxLinesPerRefresh = 256;

// Append a whole refresh's worth of log lines in one coalesced edit. Each line
// stays its own block (so maximumBlockCount still bounds memory), but the
// expensive QTextDocument relayout + text shaping happens once for the batch
// instead of once per QTextEdit::append(). Runs are inserted with
// QTextCharFormat (no HTML parse); '\n' inside a run becomes a soft line break so
// a multi-line entry stays a single block. Auto-scrolls to the bottom.
static void appendLogBatch(QTextEdit* view, QVector<QVector<LogRun>>& lines)
{
    if (!view || lines.isEmpty()) return;
    if (lines.size() > kMaxLinesPerRefresh)
    {
        const int dropped = lines.size() - kMaxLinesPerRefresh;
        lines.remove(0, dropped);   // keep the newest
        lines.prepend({ { QColor(128, 128, 128),
                          QStringLiteral("… %1 lines suppressed (flood)").arg(dropped) } });
    }
    QTextCursor c(view->document());
    c.movePosition(QTextCursor::End);
    const bool startEmpty = view->document()->isEmpty();
    c.beginEditBlock();
    for (int i = 0; i < lines.size(); ++i)
    {
        // append() doesn't prepend an empty block to an empty document.
        if (!(startEmpty && i == 0)) c.insertBlock();
        for (const LogRun& r : lines.at(i))
        {
            QTextCharFormat fmt;
            if (r.color.isValid()) fmt.setForeground(r.color);
            if (!r.role.isEmpty()) fmt.setProperty(RoleProp, r.role);
            QString t = r.text;
            t.replace(QLatin1Char('\n'), QChar(QChar::LineSeparator));  // keep multi-line entries in one block
            c.insertText(t, fmt);
        }
    }
    c.endEditBlock();   // single relayout + maximumBlockCount trim here
    // No moveCursor/ensureCursorVisible: that scrolls horizontally to the end of
    // the last line, drifting the view right. LogView::updateBottomFill keeps it
    // pinned to the bottom-left (and only while the user is already at the bottom).
}

void MetricsPanel::drainOscRing(bool outgoing)
{
    if (!m_api) return;
    QTextEdit* view = outgoing ? m_oscOutView : m_oscInView;
    if (!view) return;
    ring_view rv = outgoing ? m_api->AudioProcessor_GetInRing()
                            : m_api->AudioProcessor_GetOutRing();
    RingCursor& cur = outgoing ? m_inCursor : m_outCursor;
    QVector<QVector<LogRun>> lines;
    walkRing(rv, cur, m_scratch,
        [&](uint32_t /*seq*/, uint32_t src, const uint8_t* payload, uint32_t n) {
            lines.append(formatOscRuns(payload, n, src));
        });
    appendLogBatch(view, lines);
}

// Drain one engine→host ring, splitting by type: /supersonic/debug → Debug pane
// (text + local timestamp), everything else → From-SuperSonic pane (formatted
// OSC). Parse as OSC; never dump raw bytes.
void MetricsPanel::drainEgressRing(bool nrt)
{
    if (!m_api) return;
    ring_view   rv  = nrt ? m_api->AudioProcessor_GetDebugRing()
                          : m_api->AudioProcessor_GetOutRing();
    RingCursor& cur = nrt ? m_debugCursor : m_outCursor;
    QVector<QVector<LogRun>> debugLines, oscInLines;
    walkRing(rv, cur, m_scratch,
        [&](uint32_t /*seq*/, uint32_t src, const uint8_t* payload, uint32_t n) {
            // NRT-out frames carry a leading [route:u32] word (OUT too once
            // unified). OSC addresses start with '/', a route word doesn't, so
            // skip 4 bytes when the first byte isn't '/'.
            const uint8_t* osc  = payload;
            uint32_t       oscN = n;
            if (n >= 4 && payload[0] != '/') { osc += 4; oscN -= 4; }

            oscpkt::PacketReader pr(osc, oscN);
            oscpkt::Message* msg = pr.isOk() ? pr.popMessage() : nullptr;
            if (msg && msg->addressPattern() == "/supersonic/debug") {
                oscpkt::Message::ArgReader ar = msg->arg();
                if (ar.isStr() && m_debugView) {
                    std::string s; ar.popStr(s);
                    QString text = QString::fromStdString(s);
                    while (text.endsWith('\n') || text.endsWith('\r')) text.chop(1);
                    if (!text.isEmpty()) {
                        // A leading \x01 marks the engine's boot banner (the
                        // status summary): render it verbatim with no timestamp.
                        // Everything else is a timestamped debug line. Embedded
                        // '\n' becomes a soft break in appendLogBatch (one block).
                        const bool banner = text.startsWith(QChar(0x01));
                        if (banner) text.remove(0, 1);
                        if (banner) {
                            debugLines.append({ { QColor(), text } });
                        } else {
                            QVector<LogRun> line = stampRuns();
                            line.append({ QColor(), QStringLiteral(" ") });
                            line.append({ QColor(), text });
                            debugLines.append(line);
                        }
                    }
                    return;
                }
            }
            if (m_oscInView)
                oscInLines.append(formatOscRuns(osc, oscN, src));
        });
    // One coalesced relayout per view, instead of one per drained message.
    appendLogBatch(m_debugView, debugLines);
    appendLogBatch(m_oscInView, oscInLines);
}

void MetricsPanel::updateNodeTree()
{
    if (!m_nodeGraph) return;
    node_tree_view nt = m_api ? m_api->AudioProcessor_GetNodeTree() : node_tree_view{};
    if (!nt.header || !nt.entries) {
        if (m_lastTreeVersion != 0xFFFFFFFFu) {
            m_nodeGraph->setTree({});
            if (m_treeStats) m_treeStats->setText(QString());
            m_lastTreeVersion = 0xFFFFFFFFu;
        }
        return;
    }
    uint32_t version = reinterpret_cast<const std::atomic<uint32_t>*>(nt.header + 4)
                           ->load(std::memory_order_relaxed);
    if (version == m_lastTreeVersion) return;   // unchanged — skip rebuild
    m_lastTreeVersion = version;

    QVector<NodeTreeGraph::Node> nodes;
    int groups = 0, fx = 0, samples = 0, synths = 0;

    for (uint32_t i = 0; i < nt.max_nodes; ++i)
    {
        const uint8_t* e = nt.entries + static_cast<size_t>(i) * nt.entry_bytes;
        int32_t id = *reinterpret_cast<const int32_t*>(e + 0);
        if (id < 0) continue;                            // empty slot
        int32_t parent  = *reinterpret_cast<const int32_t*>(e + 4);
        int32_t isGroup = *reinterpret_cast<const int32_t*>(e + 8);
        // prev_id @12, next_id @16, head_id @20 — the scsynth sibling chain that
        // encodes true execution order. Slot/array order here is allocation
        // order, not sibling order, so the graph must follow this chain.
        int32_t nextId  = *reinterpret_cast<const int32_t*>(e + 16);
        int32_t headId  = *reinterpret_cast<const int32_t*>(e + 20);
        const char* nm  = reinterpret_cast<const char*>(e + 24);
        QString name = QString::fromUtf8(nm, qstrnlen(nm, 32));

        const bool isFx     = name.contains("-fx_") || name.contains("-fx-");
        const bool isSample = name.contains("stereo_player") || name.contains("mono_player");
        NodeTreeGraph::Kind kind;
        if (isGroup)       { kind = NodeTreeGraph::Group;  ++groups; }
        else if (isFx)     { kind = NodeTreeGraph::Fx;     ++fx; ++synths; }
        else if (isSample) { kind = NodeTreeGraph::Sample; ++samples; ++synths; }
        else               { kind = NodeTreeGraph::Synth;  ++synths; }

        NodeTreeGraph::Node node;
        node.id = id;
        node.parent = parent;
        node.head = headId;
        node.next = nextId;
        node.kind = kind;
        node.label = isGroup ? (name.isEmpty() ? QStringLiteral("group") : name)
                             : name;
        nodes.append(node);
    }

    const int pureSynths = synths - fx - samples;

    m_nodeGraph->setTree(nodes);
    if (m_treeStats)
    {
        // Swatch colours match the graph's node colours (set in applyTheme).
        auto sw = [&](const char* themeName, const char* label, int n) {
            const QString dot = m_theme ? m_theme->color(themeName).name() : m_textColor.name();
            return QStringLiteral("<span style=\"color:%1\">&#9679;</span> "
                                  "<span style=\"color:%2\">%3 %4</span>")
                .arg(dot, kindColor(K_Dim).name(), QString::fromUtf8(label), QString::number(n));
        };
        const QString gap = QStringLiteral("&nbsp;&nbsp;&nbsp;");
        m_treeStats->setText(sw("NumberForeground", "Groups", groups) + gap
                             + sw("FunctionMethodNameForeground", "Synths", pureSynths) + gap
                             + sw("KeywordForeground", "FX", fx) + gap
                             + sw("DoubleQuotedStringForeground", "Samples", samples));
    }
}

void MetricsPanel::renderDisconnected()
{
    for (ValueRowUi& ui : m_valueRows)
    {
        const RowDef* def = static_cast<const RowDef*>(ui.def);
        QString html;
        if (def->cap > 0 || def->nowField >= 0)   // ring usage % / age row
            html = QString("<span style=\"color:%1\">-</span>").arg(kindColor(K_Muted).name());
        else
            for (const Seg& s : def->segs)
            {
                if (s.isText)
                    html += QString("<span style=\"color:%1\">%2</span>")
                                .arg(kindColor(s.kind).name(), QString::fromUtf8(s.text).toHtmlEscaped());
                else
                    html += QString("<span style=\"color:%1\">-</span>").arg(kindColor(s.kind).name());
            }
        if (html != ui.lastHtml)
        {
            ui.value->setText(html);
            ui.lastHtml = html;
        }
    }
    // Drop ring cursors so a reconnect re-primes at the live head (no replay).
    m_inCursor = RingCursor{};
    m_outCursor = RingCursor{};
    m_debugCursor = RingCursor{};
}

void MetricsPanel::refresh()
{
    const std::atomic<uint32_t>* m = m_api ? m_api->AudioProcessor_GetMetrics() : nullptr;
    if (!m)
    {
        renderDisconnected();
        return;
    }

    // Snapshot once per tick; relaxed loads (display-only).
    uint32_t v[kPanelFieldCount] = {0};
    for (int i = 0; i < kMetricCount; ++i)
        v[i] = m[i].load(std::memory_order_relaxed);

    // Native stats live in a separate region; appended after the struct fields.
    const native_stats ns = m_api->AudioProcessor_GetNativeStats();
    v[kFieldSynthDefs]   = ns.synthdefs;
    v[kFieldBuffers]     = ns.buffers;
    v[kFieldBufferBytes] = ns.buffer_bytes;
    v[kFieldCpuAvg]      = ns.cpu_load_avg_centi;
    v[kFieldCpuPeak]     = ns.cpu_load_peak_centi;
    v[kFieldOverruns]    = ns.callback_overruns;
    // Native-only metrics read "-" (not a misleading 0) when this segment
    // doesn't produce them (e.g. a web-origin engine).
    const bool nativeOk = m_api->AudioProcessor_HasNativeStats();

    for (ValueRowUi& ui : m_valueRows)
    {
        const RowDef* def = static_cast<const RowDef*>(ui.def);
        QString html;
        if (def->cap > 0)   // ring usage % readout: "used% / peak%"
        {
            const uint32_t used = (def->usedField >= 0) ? v[def->usedField] : 0;
            const uint32_t peak = (def->peakField >= 0) ? v[def->peakField] : 0;
            const double cap = double(def->cap);
            const double usedPct = (used / cap) * 100.0;
            const double peakPct = (peak / cap) * 100.0;
            html = QString("<span style=\"color:%1\">%2</span>"
                           "<span style=\"color:%3\"> / %4%</span>")
                       .arg(kindColor(K_Normal).name(), QString::number(usedPct, 'f', 1),
                            kindColor(K_Muted).name(), QString::number(peakPct, 'f', 1));
        }
        else if (def->nowField >= 0)   // age readout: process calls since an event
        {
            const uint32_t now  = v[def->nowField];
            const uint32_t then = v[def->thenField];
            if (then == 0)   // event never fired this session
                html = QString("<span style=\"color:%1\">-</span>").arg(kindColor(K_Dim).name());
            else
            {
                const uint32_t age = now >= then ? now - then : 0;
                html = QString("<span style=\"color:%1\">%2</span>"
                               "<span style=\"color:%3\"> ticks</span>")
                           .arg(kindColor(K_Dim).name(), QString::number(age),
                                kindColor(K_Muted).name());
            }
        }
        else
        {
            for (const Seg& s : def->segs)
            {
                QString piece;
                if (s.isText)
                    piece = QString::fromUtf8(s.text).toHtmlEscaped();
                else if (s.na || s.field < 0 || s.field >= kPanelFieldCount)
                    piece = QStringLiteral("-");
                else if (s.nativeOnly && !nativeOk)
                    piece = QStringLiteral("-");
                else
                    piece = formatField(v[s.field], s.fmt);

                html += QString("<span style=\"color:%1\">%2</span>").arg(kindColor(s.kind).name(), piece);
            }
        }
        if (html != ui.lastHtml)
        {
            ui.value->setText(html);
            ui.lastHtml = html;
        }
    }

    // Tail the rings + node tree.
    drainOscRing(/*outgoing=*/true);   // IN ring     → To SuperSonic (what Sonic Pi sent)
    drainEgressRing(/*nrt=*/false);    // OUT ring     → /supersonic/debug → Debug, rest → From SuperSonic
    drainEgressRing(/*nrt=*/true);     // NRT-out ring → /supersonic/debug → Debug, rest → From SuperSonic
    updateNodeTree();

    // Now that the debug cursor is primed (tailing live), ask SuperSonic once
    // for its build/runtime summary — it replies down the debug ring, so the
    // next drain shows it in the Info pane just after the ascii art.
    if (m_debugCursor.primed && !m_summaryRequested)
        requestSupersonicSummary();
}

void MetricsPanel::requestSupersonicSummary()
{
    const int port = m_api ? m_api->GetPort(SonicPi::SonicPiPortId::scsynth) : 0;
    if (port <= 0)
        return;
    if (!m_summarySocket)
    {
        m_summarySocket = new QUdpSocket(this);
        m_summarySocket->bind(QHostAddress::LocalHost, 0);
    }
    // OSC "/supersonic/summary" with an empty (",") type tag, 4-byte padded.
    QByteArray pkt;
    auto pad = [&pkt](const char* str) {
        pkt.append(str);
        pkt.append('\0');
        while (pkt.size() % 4 != 0) pkt.append('\0');
    };
    pad("/supersonic/summary");
    pad(",");
    m_summarySocket->writeDatagram(pkt, QHostAddress::LocalHost, static_cast<quint16>(port));
    m_summaryRequested = true;
}

void MetricsPanel::applyTheme(SonicPiTheme* theme)
{
    m_theme = theme;
    m_textColor   = theme->color("LogForeground");
    m_bgColor     = theme->color("LogBackground");
    m_borderColor = theme->color("MarginForeground");

    // All colours come from the palette; label colours via the
    // ssRole/objectName selectors set once at construction.
    const QString fg     = m_textColor.name();
    const QString bg     = m_bgColor.name();
    const QString border = m_borderColor.name();
    const QString dim    = kindColor(K_Dim).name();
    const QString muted  = kindColor(K_Muted).name();
    const QString winBorder = theme->color("WindowBorder").name();     // separator bar
    const int gridW = ScaleHeightForDPI(1);   // DPI-scaled grid line (a bare 1px reads as a faint hairline)

    setStyleSheet(QString(
        // Enforce the (small) panel font in the sheet itself — a setStyleSheet()
        // call otherwise resets fonts applied via setFont() back to the default.
        "MetricsPanel, MetricsPanel * { font-family:'Hack'; font-size:11px; }"
        "MetricsPanel, QScrollArea, QFrame#ssCard, QFrame#ssCardFlat,"
        " QFrame#ssCell { background:%1; }"
        // Border-top/left on the container + border-right/bottom per cell =
        // shared single grid lines, no gaps.
        "QWidget#ssZones { background:%1; border-top:%7px solid %6; border-left:%7px solid %6; }"
        "QFrame#ssCell { border-right:%7px solid %6; border-bottom:%7px solid %6; }"
        "QFrame#ssCell[lastrow=\"true\"] { border-bottom:none; }"
        // Rightmost cells drop their right border so the grid doesn't draw a
        // line hard up against the scrollbar.
        "QFrame#ssCell[lastcol=\"true\"] { border-right:none; }"
        // #ssCardFlat is the same card without the border (node tree, logs).
        "QFrame#ssCard { border:1px solid %3; border-radius:4px; }"
        "QLabel#ssCardTitle { color:%5; padding-bottom:1px; }"
        "QLabel[ssRole=\"rowlabel\"] { color:%4; }"
        "QTextEdit { color:%2; background:%1; border:none; }")
        .arg(bg, fg, border, dim, muted, winBorder).arg(gridW));

    // The main/logs dividers paint themselves (ThinSplitter): a thin centre line
    // at rest, revealed full-width on hover (Qt's QSS can't do this). Push the
    // theme colours in; bg blends the wide grab area with the panel.
    const QColor divLine  = theme->color("WindowBorder");
    const QColor divHover = theme->color("ScrollBarHover");
    if (m_mainSplit)  m_mainSplit->setDividerColors(m_bgColor, divLine, divHover);
    if (m_rightSplit) m_rightSplit->setDividerColors(m_bgColor, divLine, divHover);
    // The tree/metrics divider line is painted by the metrics ChevronButton (its
    // band spans the whole divider), so that splitter's own handle stays
    // transparent to avoid a doubled line.
    if (m_leftSplit)
        m_leftSplit->setStyleSheet("QSplitter::handle { background:transparent; image:none; }");
    // The logs chevron's band is short, so the main divider's full-height line is
    // the splitter handle itself — shown while the logs are visible, hidden once
    // they're collapsed (setDividerLineVisible toggles it).
    setDividerLineVisible(m_mainSplit, !m_logsMinimised);

    // The chevron grip is painted by ChevronButton (not styled via QSS): fill
    // with the exact divider-line colour, brighten to the accent on hover like
    // the splitter handle, glyph in the foreground colour. In high-contrast mode
    // LogForeground is near-black, which vanishes on the grey divider grip — so
    // force the glyph to white there to keep the chevron legible.
    if (m_metricsToggle)
    {
        const QColor glyph = (theme->getColourScheme() == SonicPiTheme::HighContrastScheme)
                                 ? QColor(Qt::white)
                                 : m_textColor;
        m_metricsToggle->setColors(theme->color("WindowBorder"),
                                   theme->color("ScrollBarHover"),
                                   glyph);
    }

    if (m_logsToggle)
    {
        const QColor glyph = (theme->getColourScheme() == SonicPiTheme::HighContrastScheme)
                                 ? QColor(Qt::white)
                                 : m_textColor;
        m_logsToggle->setColors(theme->color("WindowBorder"),
                                theme->color("ScrollBarHover"),
                                glyph);
    }

    if (m_nodeGraph)
        m_nodeGraph->applyTheme(m_textColor, m_bgColor, m_borderColor,
                                theme->color("NumberForeground"),             // group  (blue)
                                theme->color("FunctionMethodNameForeground"), // synth  (pink)
                                theme->color("KeywordForeground"),            // fx     (yellow)
                                theme->color("DoubleQuotedStringForeground"));// sample (green)

    // Re-tint the OSC/debug text already on screen so it tracks the new theme
    // (scheme + hue) instead of keeping the colours it was inserted with.
    recolourLogViews();

    // Re-render with the new palette on the next tick by clearing the diff cache.
    for (ValueRowUi& ui : m_valueRows)
        ui.lastHtml.clear();
    m_lastTreeVersion = 0xFFFFFFFFu;  // force legend (colours) to re-render

    if (isVisible())
        refresh();
    else
        renderDisconnected();
}

void MetricsPanel::setTitlesVisible(bool)
{
    // Card titles (objectName "ssCardTitle", see makeCard) label otherwise-cryptic
    // metric groups, so they stay visible regardless of the "show pane titles"
    // preference.
    const QList<QLabel*> labels = findChildren<QLabel*>();
    for (QLabel* l : labels)
        if (l->objectName() == QLatin1String("ssCardTitle"))
            l->setVisible(true);
}

void MetricsPanel::seedMainSplit()
{
    // Seed once, the first time the splitter has a real width.
    if (m_splitInit || !m_mainSplit || m_mainSplit->width() <= 0)
        return;
    constexpr double kPhi = 0.618;
    const int w = m_mainSplit->width();
    m_mainSplit->setSizes({ int(w * kPhi), w - int(w * kPhi) });
    m_splitInit = true;
}

void MetricsPanel::showEvent(QShowEvent* e)
{
    QWidget::showEvent(e);
    seedMainSplit();
    reflowMetrics();   // snap rows by width + pin the metrics pane height
    revealColumns();
    if (m_metricsToggle)
        m_metricsToggle->raise();   // keep the chevron grips on top
    if (m_logsToggle)
        m_logsToggle->raise();
    positionLogsToggle();
    refresh();
    m_timer->start();
}

void MetricsPanel::hideEvent(QHideEvent* e)
{
    QWidget::hideEvent(e);
    m_timer->stop();
}

bool MetricsPanel::eventFilter(QObject* obj, QEvent* e)
{
    // Double-clicking the main divider line toggles the logs column, mirroring a
    // click on its chevron knob.
    if (m_mainSplit && e->type() == QEvent::MouseButtonDblClick
        && obj == m_mainSplit->handle(1))
    {
        toggleLogs();
        return true;
    }
    // Keep the logs chevron knob and the main divider handle it overlays in sync:
    // hovering either reveals both, so they act as one control.
    QSplitterHandle* mainHandle = m_mainSplit ? m_mainSplit->handle(1) : nullptr;
    if (obj == mainHandle || obj == m_logsToggle)
    {
        if (e->type() == QEvent::Enter || e->type() == QEvent::HoverEnter)
        {
            if (m_mainSplit) m_mainSplit->setForcedHover(true);
            if (m_logsToggle) m_logsToggle->setHovering(true);
        }
        else if (e->type() == QEvent::Leave || e->type() == QEvent::HoverLeave)
        {
            if (m_mainSplit) m_mainSplit->setForcedHover(false);
            if (m_logsToggle) m_logsToggle->setHovering(false);
        }
    }
    if ((obj == m_leftSplit || obj == m_rightSplit) && e->type() == QEvent::Resize)
        revealColumns();
    // Re-flow the metric cards (snap 1/2 rows by width) as the pane resizes.
    else if (m_metricsScroll && obj == m_metricsScroll->viewport() && e->type() == QEvent::Resize)
        reflowMetrics();
    return QWidget::eventFilter(obj, e);
}

namespace
{
} // namespace

void MetricsPanel::reflowMetrics()
{
    if (!m_metricsScroll || m_metricsCards.isEmpty())
        return;
    const int n = m_metricsCards.size();
    const int w = m_metricsScroll->viewport()->width();
    // Two layouts only: a single row of all cards, or two rows. Choose by width,
    // with ±40px hysteresis so it doesn't flip-flop at the boundary.
    const int oneRowW = n * kCardMinW;
    int rows;
    if (m_metricsCols >= n)                 // currently a single row
        rows = (w >= oneRowW - 40) ? 1 : 2;
    else                                    // currently two rows
        rows = (w >= oneRowW + 40) ? 1 : 2;
    const int cols = (n + rows - 1) / rows;
    reflowMetricsGrid(cols);
    updateMetricsHeight();
    revealColumns();   // re-align the bottom log pane with the new metrics height
}

void MetricsPanel::updateMetricsHeight()
{
    if (!m_metricsScroll)
        return;
    QWidget* content = m_metricsScroll->widget();
    if (!content || !content->layout())
        return;
    content->layout()->activate();   // make sizeHint reflect the new row count
    // Exactly the height the current 1 or 2 rows need — never more.
    int needed = content->sizeHint().height() + 1;   // +1 for the top grid border
    if (content->sizeHint().width() > m_metricsScroll->viewport()->width())
        needed += m_metricsScroll->horizontalScrollBar()->sizeHint().height();
    m_metricsNeededH = needed;
    // Pin the pane: a QSplitter honours a fixed-height child, so the node tree
    // above absorbs all remaining height. (Skip while minimised — see toggle.)
    if (!m_metricsMinimised && m_metricsScroll->maximumHeight() != needed)
        m_metricsScroll->setFixedHeight(needed);
}

void MetricsPanel::reflowMetricsGrid(int cols)
{
    if (!m_metricsGrid || cols < 1 || cols == m_metricsCols)
        return;
    m_metricsCols = cols;

    const int n = m_metricsCards.size();
    const int rows = (n + cols - 1) / cols;
    for (QFrame* c : m_metricsCards)
        m_metricsGrid->removeWidget(c);
    for (int i = 0; i < n; ++i)
    {
        const int r = i / cols;
        const int cc = i % cols;
        // One column per card (no spanning) — a partial last row simply ends.
        m_metricsGrid->addWidget(m_metricsCards[i], r, cc, 1, 1);
        // Bottom-row cells drop their bottom border, true right-edge cells their
        // right border; re-polish so the dynamic property changes take effect in
        // the stylesheet.
        const bool lastRow = (r == rows - 1);
        const bool lastCol = (cc == cols - 1);
        QFrame* card = m_metricsCards[i];
        if (card->property("lastrow").toBool() != lastRow ||
            card->property("lastcol").toBool() != lastCol)
        {
            card->setProperty("lastrow", lastRow);
            card->setProperty("lastcol", lastCol);
            card->style()->unpolish(card);
            card->style()->polish(card);
        }
    }
    // Equal stretch across the active extent; collapse the rest.
    for (int c = 0; c < 16; ++c) m_metricsGrid->setColumnStretch(c, c < cols ? 1 : 0);
    for (int r = 0; r < 16; ++r) m_metricsGrid->setRowStretch(r, r < rows ? 1 : 0);
}

void MetricsPanel::revealColumns()
{
    if (m_revealing)   // our own setSizes can re-enter via resize events; ignore
        return;
    m_revealing = true;

    // The left column is laid out by updateMetricsHeight(): the metrics pane is
    // pinned to its needed height and the node tree takes the rest. The right
    // (logs) column splits its three panes (Info / To / From) evenly at any
    // height, so they start equal on boot and grow together — until the user
    // drags a divider (m_rightManual).
    if (m_rightSplit && m_rightSplit->height() > 0 && !m_rightManual)
    {
        // Even split at any height (no progressive top-down reveal). Panes have a
        // zero height floor (Ignored vertical policy), so nothing clamps it.
        const int n = m_rightSplit->count();
        if (n > 0)
        {
            const int avail = qMax(0, m_rightSplit->height()
                                      - m_rightSplit->handleWidth() * (n - 1));
            const int each = avail / n;
            QList<int> sizes;
            sizes.reserve(n);
            for (int i = 0; i < n; ++i)
                sizes << each;
            sizes[n - 1] += avail - each * n;   // rounding remainder to the last
            if (sizes != m_rightSplit->sizes())
            {
                QSignalBlocker block(m_rightSplit);
                m_rightSplit->setSizes(sizes);
            }
        }
    }

    positionMetricsToggle();
    positionLogsToggle();
    m_revealing = false;
}

void MetricsPanel::positionMetricsToggle()
{
    if (!m_metricsToggle || !m_leftSplit)
        return;
    const int h = m_leftSplit->height();
    if (h <= 0)
        return;
    const int hw = m_leftSplit->handleWidth();
    // The grip spans the whole divider width and is a bit taller than the line so
    // its knob box can rise above/below it; centre that band on the divider. The
    // metrics pane is fixed-height and sits at the bottom, so the divider is
    // directly above it — derive its position from that height (sizes() would be
    // momentarily stale right after a toggle, as the splitter relayouts async).
    const int m = m_metricsMinimised ? 0 : qBound(0, m_metricsNeededH, qMax(0, h - hw));
    const int dividerCentre = h - m - hw / 2;
    const int boxH = ScaleHeightForDPI(18);
    const int top = qBound(0, dividerCentre - boxH / 2, qMax(0, h - boxH));
    m_metricsToggle->setGeometry(0, top, m_leftSplit->width(), boxH);
}

void MetricsPanel::toggleMetrics()
{
    m_metricsMinimised = !m_metricsMinimised;

    // Ease the metrics pane to nothing on minimise (node tree fills the column),
    // or back to the height its rows need. The QSplitter follows the fixed-height
    // child automatically, so animating that height animates the divider.
    animateMetrics(m_metricsMinimised ? 0 : m_metricsNeededH);

    updateChevron();
}

void MetricsPanel::animateMetrics(int targetHeight)
{
    if (!m_metricsScroll)
        return;
    if (!m_metricsAnim)
    {
        m_metricsAnim = new QVariantAnimation(this);
        m_metricsAnim->setDuration(160);
        m_metricsAnim->setEasingCurve(QEasingCurve::InOutCubic);
        connect(m_metricsAnim, &QVariantAnimation::valueChanged, this,
                [this](const QVariant& v) {
                    if (m_metricsScroll)
                        m_metricsScroll->setFixedHeight(v.toInt());
                    positionMetricsToggle();
                });
    }
    m_metricsAnim->stop();
    // Reduced motion: run the same path in a single frame so the layout logic
    // (and any finished handlers) still execute, just without the tween.
    m_metricsAnim->setDuration(SonicPi::prefersReducedMotion() ? 1 : 160);
    m_metricsAnim->setStartValue(m_metricsScroll->height());
    m_metricsAnim->setEndValue(targetHeight);
    m_metricsAnim->start();
}

void MetricsPanel::updateChevron()
{
    if (!m_metricsToggle)
        return;
    // Points down when the metrics are shown (click to collapse), up when
    // minimised (click to show). The glyph is painted by ChevronButton.
    m_metricsToggle->setDir(m_metricsMinimised ? ChevronButton::Up : ChevronButton::Down);
    m_metricsToggle->setToolTip(m_metricsMinimised ? tr("Show metrics") : tr("Minimise metrics"));
    m_metricsToggle->setAccessibleName(m_metricsMinimised ? tr("Show metrics") : tr("Minimise metrics"));
    // The divider line shows while the metrics are visible, and disappears (just
    // the knob remains) once collapsed.
    m_metricsToggle->setLineVisible(!m_metricsMinimised);
}

void MetricsPanel::setDividerLineVisible(ThinSplitter* s, bool visible)
{
    if (s)
        s->setLineVisible(visible);
}

void MetricsPanel::toggleLogs()
{
    if (!m_mainSplit || !m_rightSplit)
        return;
    m_logsMinimised = !m_logsMinimised;
    int target;
    if (m_logsMinimised)
    {
        // Remember the divider position, then ease the logs column to zero width.
        m_savedMainSizes = m_mainSplit->sizes();
        target = 0;
    }
    else if (m_savedMainSizes.size() == 2)
    {
        target = m_savedMainSizes[1];
    }
    else
    {
        const int w = m_mainSplit->width();
        target = w - int(w * 0.618);   // golden ratio fallback
    }
    animateLogs(target);
    updateLogsChevron();
}

void MetricsPanel::animateLogs(int targetLogsWidth)
{
    if (!m_mainSplit || !m_rightSplit)
        return;
    if (!m_logsAnim)
    {
        m_logsAnim = new QVariantAnimation(this);
        m_logsAnim->setDuration(160);
        m_logsAnim->setEasingCurve(QEasingCurve::InOutCubic);
        // Drive the logs column width via its max width (a QSplitter honours a
        // child's max width even when the split is non-collapsible by drag), then
        // re-flow the main split to suit.
        connect(m_logsAnim, &QVariantAnimation::valueChanged, this,
                [this](const QVariant& v) {
                    if (!m_mainSplit || !m_rightSplit)
                        return;
                    const int rw = v.toInt();
                    m_rightSplit->setMaximumWidth(rw);
                    const QList<int> s = m_mainSplit->sizes();
                    const int total = s.value(0) + s.value(1);
                    m_mainSplit->setSizes({ total - rw, rw });
                    positionLogsToggle();
                });
        connect(m_logsAnim, &QVariantAnimation::finished, this, [this]() {
            if (!m_rightSplit)
                return;
            // When restored, lift the width clamp so the divider is draggable
            // again; when minimised, leave it clamped at zero.
            if (!m_logsMinimised)
            {
                m_rightSplit->setMaximumWidth(QWIDGETSIZE_MAX);
                if (m_savedMainSizes.size() == 2)
                    m_mainSplit->setSizes(m_savedMainSizes);
            }
            positionLogsToggle();
        });
    }
    m_logsAnim->stop();
    // Reduced motion: single-frame run — see animateMetrics().
    m_logsAnim->setDuration(SonicPi::prefersReducedMotion() ? 1 : 160);
    m_logsAnim->setStartValue(m_rightSplit->width());
    m_logsAnim->setEndValue(targetLogsWidth);
    m_logsAnim->start();
}

void MetricsPanel::updateLogsChevron()
{
    if (!m_logsToggle)
        return;
    // Points right when the logs are shown (click to push them off to the
    // right), left when minimised (click to bring them back).
    m_logsToggle->setDir(m_logsMinimised ? ChevronButton::Left : ChevronButton::Right);
    m_logsToggle->setToolTip(m_logsMinimised ? tr("Show logs") : tr("Minimise logs"));
    m_logsToggle->setAccessibleName(m_logsMinimised ? tr("Show logs") : tr("Minimise logs"));
    // The divider line (chevron band + the full-height splitter handle behind it)
    // shows while the logs are visible, and disappears once collapsed.
    m_logsToggle->setLineVisible(!m_logsMinimised);
    setDividerLineVisible(m_mainSplit, !m_logsMinimised);
}

void MetricsPanel::positionLogsToggle()
{
    if (!m_logsToggle || !m_mainSplit || !m_leftSplit)
        return;
    const int w = m_mainSplit->width();
    const int h = m_mainSplit->height();
    if (w <= 0 || h <= 0)
        return;
    const int hw = m_mainSplit->handleWidth();
    // The divider sits just right of the left column. Centre a short knob band on
    // it — short enough that the rest of the divider line stays draggable.
    const int dividerCentreX = m_leftSplit->width() + hw / 2;
    const int bandW = ScaleHeightForDPI(18);
    const int bandH = ScaleHeightForDPI(64);
    const int left = qBound(0, dividerCentreX - bandW / 2, qMax(0, w - bandW));
    // Near the top of the divider (top-right of the pane) rather than centred.
    const int top = qBound(0, ScaleHeightForDPI(10), qMax(0, h - bandH));
    m_logsToggle->setGeometry(left, top, bandW, bandH);
}
