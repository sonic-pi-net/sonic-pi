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
#include <cstdint>
#include <cstring>
#include <vector>

#include <QBrush>
#include <QFont>
#include <QFrame>
#include <QGridLayout>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QHideEvent>
#include <QLabel>
#include <QProgressBar>
#include <QResizeEvent>
#include <QScrollArea>
#include <QScrollBar>
#include <QShowEvent>
#include <QSizePolicy>
#include <QSplitter>
#include <QSplitterHandle>
#include <QStyle>
#include <QTextDocument>
#include <QTextEdit>
#include <QTextFrame>
#include <QTextFrameFormat>
#include <QTime>
#include <QTimer>
#include <QToolButton>
#include <QVBoxLayout>
#include <QHash>
#include <QEvent>
#include <QMouseEvent>

#include "chevronbutton.h"
#include "dpi.h"

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
constexpr int kFieldClockTempo     = 46; // milli-BPM
constexpr int kFieldClockBeat      = 47; // beat * 100
constexpr int kFieldClockPhase     = 48; // phase * 100
constexpr int kFieldClockPlaying   = 49;
// Native-only live stats appended after the struct fields in the panel's value
// array (sourced from AudioProcessor_GetNativeStats, not the metrics struct).
constexpr int kFieldSynthDefs   = 50;
constexpr int kFieldBuffers     = 51;
constexpr int kFieldBufferBytes = 52;
constexpr int kPanelFieldCount  = 53;

// Poll cadence while visible (~6-7 Hz).
constexpr int kRefreshMs = 150;

// Per-widget reveal heights (logical px). As a column grows, each widget in
// turn grows to its reveal height before the next begins to appear; once every
// widget is revealed the surplus is shared evenly between them.
constexpr int kTreeReveal    = 220;  // left:  node tree
constexpr int kMetricsReveal = 220;  // left:  metrics grid
constexpr int kLogReveal     = 150;  // right: each of Debug / To / From

// Approx height of one metric card row; used to decide how many rows of cards
// fit the metrics pane so they re-flow (down to a single row when it's short).
constexpr int kCardRowH = 100;

// Ring capacities, mirrored from external/supersonic/src/memory_profile.h
// (IN/OUT/NRT_OUT_BUFFER_SIZE); used to scale the level bars.
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
    K_Green,
    K_Error
};

enum BarColor
{
    BC_Blue,
    BC_Green,
    BC_Purple
};

struct Seg
{
    bool isText;       // literal separator/suffix vs a metric value
    const char* text;  // literal text when isText
    int field;         // metric index 0-49, or -1
    Fmt fmt;
    Kind kind;
    bool na;           // force "-" (no native writer)
};

struct RowDef
{
    const char* label;
    bool isBar;
    std::vector<Seg> segs; // value row
    // bar row:
    int usedField;
    int peakField;
    uint32_t cap;
    BarColor barColor;
};

struct PanelDef
{
    const char* title;
    std::vector<RowDef> rows;
};

// Cell factories.
Seg V(int f, Kind k = K_Normal, Fmt fmt = F_Plain) { return Seg{ false, "", f, fmt, k, false }; }
Seg T(const char* t, Kind k = K_Muted) { return Seg{ true, t, -1, F_Plain, k, false }; }

RowDef ValRow(const char* label, std::vector<Seg> segs)
{
    return RowDef{ label, false, std::move(segs), -1, -1, 0, BC_Blue };
}
RowDef BarRow(const char* label, int used, int peak, uint32_t cap, BarColor c)
{
    return RowDef{ label, true, {}, used, peak, cap, c };
}

// Field indices match the struct order in shared_memory.h; late-ms fields
// (23, 24) and Link-audio drift (36) are int32 so they use F_Signed.
// Browser-only metrics (7 wasm_errors, 13 osc_in_dropped, 26 direct-write
// fails) are omitted (no native writer).
const std::vector<PanelDef>& panelLayout()
{
    static const std::vector<PanelDef> panels = {
        { "scsynth",
          { ValRow("ticks", { V(0, K_Dim), T(" | "), V(1, K_Muted), T(" msgs") }),
            ValRow("dropped", { V(2, K_Error) }),
            ValRow("seq gaps", { V(6, K_Error) }),
            ValRow("debug", { V(15, K_Muted), T(" ("), V(16, K_Muted, F_Bytes), T(")") }) } },
        { "scsynth Queue",
          { ValRow("queue", { V(3), T(" | "), V(4, K_Muted) }),
            ValRow("dropped", { V(5, K_Error) }),
            ValRow("lates", { V(8, K_Error) }),
            ValRow("max | last", { V(23, K_Error, F_Signed), T(" | "), V(24, K_Dim, F_Signed), T(" ms") }) } },
        { "OSC In",
          { ValRow("received", { V(11) }),
            ValRow("bytes", { V(12, K_Muted, F_Bytes) }),
            ValRow("corrupted", { V(14, K_Error) }) } },
        { "OSC Out",
          { ValRow("sent", { V(9) }),
            ValRow("bytes", { V(10, K_Muted, F_Bytes) }) } },
        { "Ring Level",
          { BarRow("in", 17, 20, kInBufferCap, BC_Blue),
            BarRow("out", 18, 21, kOutBufferCap, BC_Green),
            BarRow("nrt", 19, 22, kNrtOutBufferCap, BC_Purple) } },
        { "Buffers",
          { ValRow("synthdefs", { V(kFieldSynthDefs) }),
            ValRow("buffers", { V(kFieldBuffers, K_Green) }),
            ValRow("buf bytes", { V(kFieldBufferBytes, K_Muted, F_Bytes) }) } },
        { "Link",
          { ValRow("peers", { V(27, K_Green) }),
            ValRow("tempo", { V(28, K_Normal, F_MilliBpm), T(" bpm") }),
            ValRow("beat", { V(29, K_Dim, F_Centi) }),
            ValRow("phase", { V(30, K_Dim, F_Centi) }),
            ValRow("playing", { V(31, K_Muted) }) } },
        { "Link Audio",
          { ValRow("in", { V(32), T(" ch @ "), V(33, K_Muted), T(" Hz") }),
            ValRow("underruns", { V(34, K_Error) }),
            ValRow("buffered", { V(35, K_Dim), T(" ms") }),
            ValRow("drift", { V(36, K_Dim, F_Signed), T(" ppm") }),
            ValRow("publish", { V(37, K_Green), T(" | "), V(38, K_Muted), T(" sinks") }) } },
        { "Engine",
          { ValRow("version", { V(kFieldVersionMajor), T("."), V(kFieldVersionMinor), T("."), V(kFieldVersionPatch) }),
            ValRow("rate", { V(kFieldSampleRate), T(" Hz") }),
            ValRow("block", { V(kFieldBlockSize), T(" frames") }),
            ValRow("channels", { V(kFieldOutputChannels), T(" | "), V(kFieldInputChannels, K_Muted) }) } },
        { "Clock",
          { ValRow("tempo", { V(kFieldClockTempo, K_Normal, F_MilliBpm), T(" bpm") }),
            ValRow("beat", { V(kFieldClockBeat, K_Dim, F_Centi) }),
            ValRow("phase", { V(kFieldClockPhase, K_Dim, F_Centi) }),
            ValRow("playing", { V(kFieldClockPlaying, K_Muted) }) } },
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

// Bar fill hue from the theme palette (hex is the pre-theme fallback).
QString barChunkColor(BarColor c, SonicPiTheme* t)
{
    switch (c)
    {
    case BC_Green:
        return t ? t->color("DoubleQuotedStringForeground").name() : QStringLiteral("#9ece6a");
    case BC_Purple:
        return t ? t->color("FunctionMethodNameForeground").name() : QStringLiteral("#bb9af7");
    case BC_Blue:
    default:
        return t ? t->color("NumberForeground").name() : QStringLiteral("#7aa2f7");
    }
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
    v->setContentsMargins(9, 5, 9, 6);
    v->setSpacing(4);

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
    case K_Green:
        return m_theme ? m_theme->color("DoubleQuotedStringForeground") : QColor("#9ece6a");
    case K_Error:
        // K_Error renders as normal text (no red).
        return m_textColor;
    case K_Normal:
    default:
        return m_textColor;
    }
}

void MetricsPanel::buildUi()
{
    auto* outer = new QVBoxLayout(this);
    outer->setContentsMargins(0, 0, 0, 0);
    outer->setSpacing(0);

    QFont mono = makeMonoFont();

    // Left: node tree + metric grid. Right: debug + OSC in/out logs.
    auto* mainRow = new QSplitter(Qt::Horizontal, this);
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
    // Vertical AlwaysOn reserves the scrollbar gutter so the viewport width is
    // constant at any height.
    scroll->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOn);

    auto* content = new QWidget;
    content->setObjectName("ssZones");
    auto* grid = new QGridLayout(content);
    grid->setContentsMargins(0, 0, 0, 0);
    grid->setHorizontalSpacing(0);
    grid->setVerticalSpacing(0);

    // Builds one value/bar row into `rows` at row index `r`, registering it for
    // refresh().
    auto addRow = [&](QGridLayout* rows, int r, const RowDef& row) {
        auto* lbl = new QLabel(QString::fromUtf8(row.label));
        lbl->setFont(mono);
        lbl->setProperty("ssRole", "rowlabel");
        m_rowLabels.append(lbl);
        rows->addWidget(lbl, r, 0, Qt::AlignLeft);

        if (row.isBar)
        {
            auto* bar = new QProgressBar;
            bar->setRange(0, 1000);
            bar->setTextVisible(false);
            bar->setFixedHeight(8);
            rows->addWidget(bar, r, 1);
            auto* txt = new QLabel;
            txt->setFont(mono);
            txt->setTextFormat(Qt::RichText);
            txt->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
            rows->addWidget(txt, r, 2, Qt::AlignRight);
            m_barRows.append({ &row, bar, txt, QString() });
        }
        else
        {
            auto* val = new QLabel;
            val->setFont(mono);
            val->setTextFormat(Qt::RichText);
            val->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
            rows->addWidget(val, r, 1, 1, 2, Qt::AlignRight);
            m_valueRows.append({ &row, val, QString() });
        }
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
        rows->setHorizontalSpacing(8);
        rows->setVerticalSpacing(2);
        rows->setColumnStretch(1, 1);
        int r = 0;
        for (const RowDef& row : panel.rows)
            addRow(rows, r++, row);
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

    auto* rightCol = new QSplitter(Qt::Vertical);
    m_rightSplit = rightCol;
    buildLogs(rightCol);
    mainRow->addWidget(rightCol);

    mainRow->setStretchFactor(0, 618);  // (tree + metrics) : logs ≈ golden ratio
    mainRow->setStretchFactor(1, 382);
    // Match the rest of the app's dividers, which app.qss sizes as 6dx.
    const int kHandleW = ScaleHeightForDPI(6);
    mainRow->setHandleWidth(kHandleW);
    mainRow->setChildrenCollapsible(false);

    // Both vertical columns are draggable, and laid out by revealColumns()
    // until the user drags a divider (then that column is left to the user).
    for (QSplitter* col : { leftCol, rightCol })
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
    m_metricsToggle = new ChevronButton(this);
    m_metricsToggle->setFixedSize(48, 22);
    connect(m_metricsToggle, &QToolButton::clicked, this, &MetricsPanel::toggleMetrics);
    m_metricsToggle->setDragHandler([this](const QPoint& g) { dragMetricsDividerTo(g); });
    updateChevron();

    // Double-clicking the divider line itself toggles the metrics too, as if
    // the chevron were clicked (handled in eventFilter).
    if (QSplitterHandle* h = leftCol->handle(1))
        h->installEventFilter(this);

    // Dragging a column's divider takes it out of auto-reveal (so the drag
    // isn't undone on the next dock resize). The left column also re-anchors
    // the chevron knob onto its moved divider and keeps the chevron's
    // minimised state (and remembered height) in sync with the drag.
    connect(leftCol, &QSplitter::splitterMoved, this, [this, leftCol](int, int) {
        m_leftManual = true;
        const QList<int> s = leftCol->sizes();
        if (s.size() > 1)
        {
            const bool collapsed = s[1] <= 2;   // dragged (almost) shut
            if (s[1] > 0)
                m_savedMetricsH = s[1];
            if (m_metricsMinimised != collapsed)
            {
                m_metricsMinimised = collapsed;
                updateChevron();
            }
        }
        positionMetricsToggle();
    });
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
        connect(verticalScrollBar(), &QScrollBar::valueChanged, this,
                [this](int v) { m_pinned = v >= verticalScrollBar()->maximum() - 2; });
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
        QScrollBar* sb = verticalScrollBar();
        const bool atBottom = m_pinned || sb->value() >= sb->maximum() - 2;

        // Bottom-align by pushing the content down with a top margin on the
        // document's root frame (the widget won't reset this, unlike the
        // viewport margins). The document height already includes the current
        // margin, so subtract it to get the content's own height.
        const int contentH = document()->size().toSize().height() - m_topGap;
        const int gap = qMax(0, viewport()->height() - contentH);
        if (gap != m_topGap)
        {
            m_topGap = gap;
            if (QTextFrame* root = document()->rootFrame())
            {
                QTextFrameFormat fmt = root->frameFormat();
                fmt.setTopMargin(gap);
                root->setFrameFormat(fmt);
            }
        }
        if (atBottom)
            sb->setValue(sb->maximum());
    }

    bool m_pinned = true;
    int m_topGap = 0;
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
    m_debugView  = addLogCard(tr("Debug"));

    // Seed with the engine's own boot banner so the pane doesn't start
    // empty (matches what SuperSonic prints to its log on boot).
    m_debugView->setPlainText(
        QStringLiteral("░█▀▀░█░█░█▀█░█▀▀░█▀▄░█▀▀░█▀█░█▀█░▀█▀░█▀▀\n")
      + QStringLiteral("░▀▀█░█░█░█▀▀░█▀▀░█▀▄░▀▀█░█░█░█░█░░█░░█░░\n")
      + QStringLiteral("░▀▀▀░▀▀▀░▀░░░▀▀▀░▀░▀░▀▀▀░▀▀▀░▀░▀░▀▀▀░▀▀▀"));

    m_oscOutView = addLogCard(tr("To SuperSonic"));    // host → engine (what Sonic Pi sent)
    m_oscInView  = addLogCard(tr("From SuperSonic"));  // engine → host (replies)
}

QString MetricsPanel::formatOscHtml(const uint8_t* data, uint32_t size,
                                    uint32_t sequence, uint32_t sourceId, bool outgoing)
{
    // Sonic Pi theme syntax colours (fall back to fixed hues pre-theme).
    auto tc = [&](const char* name, const char* fallback) {
        return m_theme ? m_theme->color(name).name() : QString::fromLatin1(fallback);
    };
    const QString cMuted = m_theme ? m_theme->color("CommentForeground").name() : kindColor(K_Muted).name();
    const QString cSrc  = tc("KeywordForeground", "#e0af68");
    const QString cAddr = tc("FunctionMethodNameForeground", "#ff5fff");  // deep pink
    const QString cNum  = tc("NumberForeground", "#ff9e64");
    const QString cStr  = tc("DoubleQuotedStringForeground", "#9ece6a");
    auto span = [](const QString& col, const QString& txt) {
        return QStringLiteral("<span style=\"color:%1\">%2</span>").arg(col, txt);
    };

    QString out = span(cMuted, QStringLiteral("[%1]").arg(sequence));
    if (outgoing && sourceId != 0)
        out += " " + span(cSrc, QStringLiteral("ch%1").arg(sourceId));

    oscpkt::PacketReader pr(data, size);
    oscpkt::Message* msg;
    int count = 0;
    while (pr.isOk() && (msg = pr.popMessage()) != nullptr)
    {
        if (count++ > 0) out += " " + span(cMuted, QStringLiteral("|"));
        out += " " + span(cAddr, QString::fromStdString(msg->addressPattern()).toHtmlEscaped());
        oscpkt::Message::ArgReader ar = msg->arg();
        while (ar.nbArgRemaining() && ar.isOk())
        {
            if (ar.isInt32())      { int32_t i; ar.popInt32(i); out += " " + span(cNum, QString::number(i)); }
            else if (ar.isInt64()) { int64_t i; ar.popInt64(i); out += " " + span(cNum, QString::number(static_cast<qlonglong>(i))); }
            else if (ar.isFloat()) { float f;   ar.popFloat(f); out += " " + span(cNum, QString::number(f, 'g', 6)); }
            else if (ar.isDouble()){ double d;  ar.popDouble(d); out += " " + span(cNum, QString::number(d, 'g', 6)); }
            else if (ar.isStr())   { std::string s; ar.popStr(s); out += " " + span(cStr, "\"" + QString::fromStdString(s).toHtmlEscaped() + "\""); }
            else if (ar.isBlob())  { std::vector<char> b; ar.popBlob(b); out += " " + span(cMuted, QStringLiteral("&lt;%1 bytes&gt;").arg(b.size())); }
            else                   { ar.pop(); out += " " + span(cMuted, QStringLiteral("?")); }
        }
    }
    return out;
}

void MetricsPanel::drainOscRing(bool outgoing)
{
    if (!m_api) return;
    QTextEdit* view = outgoing ? m_oscOutView : m_oscInView;
    if (!view) return;
    ring_view rv = outgoing ? m_api->AudioProcessor_GetInRing()
                            : m_api->AudioProcessor_GetOutRing();
    RingCursor& cur = outgoing ? m_inCursor : m_outCursor;
    walkRing(rv, cur, m_scratch,
        [&](uint32_t seq, uint32_t src, const uint8_t* payload, uint32_t n) {
            view->append(formatOscHtml(payload, n, seq, src, outgoing));
        });
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
    // Debug-line timestamps in the Sonic Pi accent blue.
    const QString cTime = m_theme ? m_theme->color("ScrollBarHover").name() : QStringLiteral("#7aa2f7");
    walkRing(rv, cur, m_scratch,
        [&](uint32_t seq, uint32_t src, const uint8_t* payload, uint32_t n) {
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
                        const QString ts = QTime::currentTime().toString(QStringLiteral("HH:mm:ss.zzz"));
                        m_debugView->append(QStringLiteral("<span style=\"color:%1\">[%2]</span> %3")
                                                .arg(cTime, ts, text.toHtmlEscaped()));
                    }
                    return;
                }
            }
            if (m_oscInView)
                m_oscInView->append(formatOscHtml(osc, oscN, seq, src, /*outgoing=*/false));
        });
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
    const QString dash = QString("<span style=\"color:%1\">-</span>").arg(kindColor(K_Muted).name());
    for (ValueRowUi& ui : m_valueRows)
    {
        const RowDef* def = static_cast<const RowDef*>(ui.def);
        QString html;
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
    for (BarRowUi& ui : m_barRows)
    {
        ui.bar->setValue(0);
        const QString t = dash;
        if (t != ui.lastText)
        {
            ui.text->setText(t);
            ui.lastText = t;
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

    for (ValueRowUi& ui : m_valueRows)
    {
        const RowDef* def = static_cast<const RowDef*>(ui.def);
        QString html;
        for (const Seg& s : def->segs)
        {
            QString piece;
            if (s.isText)
                piece = QString::fromUtf8(s.text).toHtmlEscaped();
            else if (s.na || s.field < 0 || s.field >= kPanelFieldCount)
                piece = QStringLiteral("-");
            else
                piece = formatField(v[s.field], s.fmt);

            html += QString("<span style=\"color:%1\">%2</span>").arg(kindColor(s.kind).name(), piece);
        }
        if (html != ui.lastHtml)
        {
            ui.value->setText(html);
            ui.lastHtml = html;
        }
    }

    for (BarRowUi& ui : m_barRows)
    {
        const RowDef* def = static_cast<const RowDef*>(ui.def);
        const uint32_t used = (def->usedField >= 0) ? v[def->usedField] : 0;
        const uint32_t peak = (def->peakField >= 0) ? v[def->peakField] : 0;
        const double cap = def->cap > 0 ? double(def->cap) : 0.0;
        const double usedPct = cap > 0 ? (used / cap) * 100.0 : 0.0;
        const double peakPct = cap > 0 ? (peak / cap) * 100.0 : 0.0;

        ui.bar->setValue(int(usedPct * 10.0));  // styling is set once in applyTheme

        const QString t = QString("<span style=\"color:%1\">%2%</span>"
                                  "<span style=\"color:%3\"> pk %4%</span>")
                              .arg(kindColor(K_Normal).name(), QString::number(usedPct, 'f', 1),
                                   kindColor(K_Muted).name(), QString::number(peakPct, 'f', 1));
        if (t != ui.lastText)
        {
            ui.text->setText(t);
            ui.lastText = t;
        }
    }

    // Tail the rings + node tree.
    drainOscRing(/*outgoing=*/true);   // IN ring     → To SuperSonic (what Sonic Pi sent)
    drainEgressRing(/*nrt=*/false);    // OUT ring     → /supersonic/debug → Debug, rest → From SuperSonic
    drainEgressRing(/*nrt=*/true);     // NRT-out ring → /supersonic/debug → Debug, rest → From SuperSonic
    updateNodeTree();
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
    const QString faint  = blend(m_borderColor, m_bgColor, 0.55).name();
    const QString winBorder = theme->color("WindowBorder").name();     // separator bar
    const QString hover     = theme->color("ScrollBarHover").name();   // blue highlight

    setStyleSheet(QString(
        // Enforce the (small) panel font in the sheet itself — a setStyleSheet()
        // call otherwise resets fonts applied via setFont() back to the default.
        "MetricsPanel, MetricsPanel * { font-family:'Hack'; font-size:11px; }"
        "MetricsPanel, QScrollArea, QFrame#ssCard, QFrame#ssCardFlat,"
        " QFrame#ssCell { background:%1; }"
        // Border-top/left on the container + border-right/bottom per cell =
        // shared single grid lines, no gaps.
        "QWidget#ssZones { background:%1; border-top:1px solid %7; border-left:1px solid %7; }"
        "QFrame#ssCell { border-right:1px solid %7; border-bottom:1px solid %7; }"
        "QFrame#ssCell[lastrow=\"true\"] { border-bottom:none; }"
        // #ssCardFlat is the same card without the border (node tree, logs).
        "QFrame#ssCard { border:1px solid %3; border-radius:4px; }"
        "QLabel#ssCardTitle { color:%5; padding-bottom:3px; }"
        "QLabel[ssRole=\"rowlabel\"] { color:%4; }"
        "QTextEdit { color:%2; background:%1; border:none; }"
        // Plain themed line: the app-wide handle's grip image (app.qss
        // url(images/...)) doesn't resolve in this widget's stylesheet scope, so
        // it would render as an invisible handle here. Thickness is unified via
        // setHandleWidth(ScaleHeightForDPI(6)) instead. Orientation-specific so
        // these win over app.qss's ::handle:horizontal/:vertical rules — needed
        // for the hover colour to take effect.
        "QSplitter::handle:horizontal { background:%7; image:none; }"
        "QSplitter::handle:vertical { background:%7; image:none; }"
        "QSplitter::handle:horizontal:hover { background:%8; }"
        "QSplitter::handle:vertical:hover { background:%8; }")
        .arg(bg, fg, border, dim, muted, faint).arg(winBorder, hover));

    // The chevron grip is painted by ChevronButton (not styled via QSS): fill
    // with the exact divider-line colour, brighten to the accent on hover like
    // the splitter handle, glyph in the foreground colour.
    if (m_metricsToggle)
        m_metricsToggle->setColors(theme->color("WindowBorder"),
                                   theme->color("ScrollBarHover"),
                                   m_textColor);

    if (m_nodeGraph)
        m_nodeGraph->applyTheme(m_textColor, m_bgColor, m_borderColor,
                                theme->color("NumberForeground"),             // group  (blue)
                                theme->color("FunctionMethodNameForeground"), // synth  (pink)
                                theme->color("KeywordForeground"),            // fx     (yellow)
                                theme->color("DoubleQuotedStringForeground"));// sample (green)

    // Bar chrome is theme-static, so it's applied here rather than in refresh().
    for (BarRowUi& ui : m_barRows)
    {
        const RowDef* def = static_cast<const RowDef*>(ui.def);
        ui.bar->setStyleSheet(QString("QProgressBar{background:%1;border:1px solid %2;border-radius:2px;}"
                                      "QProgressBar::chunk{background:%3;border-radius:2px;}")
                                  .arg(m_bgColor.name(), m_borderColor.name(),
                                       barChunkColor(def->barColor, m_theme)));
    }

    // Re-render with the new palette on the next tick by clearing the diff cache.
    for (ValueRowUi& ui : m_valueRows)
        ui.lastHtml.clear();
    for (BarRowUi& ui : m_barRows)
        ui.lastText.clear();
    m_lastTreeVersion = 0xFFFFFFFFu;  // force legend (colours) to re-render

    if (isVisible())
        refresh();
    else
        renderDisconnected();
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
    revealColumns();
    if (m_metricsToggle)
        m_metricsToggle->raise();   // keep the chevron grip on top
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
    if ((obj == m_leftSplit || obj == m_rightSplit) && e->type() == QEvent::Resize)
        revealColumns();
    // Re-flow the metric cards (one row when the pane is short) as it resizes.
    else if (m_metricsScroll && obj == m_metricsScroll->viewport() && e->type() == QEvent::Resize)
        reflowMetrics();
    // Double-click on the node-tree / metrics divider toggles the metrics.
    else if (e->type() == QEvent::MouseButtonDblClick && m_leftSplit &&
             obj == m_leftSplit->handle(1))
    {
        toggleMetrics();
        return true;
    }
    return QWidget::eventFilter(obj, e);
}

namespace
{
// Progressive top-down reveal, then even stretch. `targets` holds each widget's
// reveal height (one entry per widget). While the column is shorter than the
// sum of the targets, widgets fill top-down (each capped at its target) so they
// appear in order; once it's taller, the surplus is split evenly so they grow
// together. The two regimes meet continuously at the boundary.
void revealStack(QSplitter* s, const QVector<int>& targets)
{
    const int n = s->count();
    if (n == 0 || n != targets.size())
        return;
    int avail = s->height() - s->handleWidth() * (n - 1);
    if (avail < 0)
        avail = 0;

    int sumTargets = 0;
    for (int t : targets)
        sumTargets += t;

    QList<int> sizes;
    sizes.reserve(n);
    if (avail <= sumTargets)
    {
        int remaining = avail;
        for (int i = 0; i < n; ++i)
        {
            const int give = qMax(0, qMin(remaining, targets[i]));
            sizes << give;
            remaining -= give;
        }
    }
    else
    {
        const int surplus = avail - sumTargets;
        const int each = surplus / n;
        for (int i = 0; i < n; ++i)
            sizes << targets[i] + each;
        sizes[n - 1] += surplus - each * n;   // rounding remainder to the last
    }
    // Only re-apply when the split actually changes — in particular a no-op
    // when only the column width changes, leaving the main divider untouched.
    if (sizes == s->sizes())
        return;
    QSignalBlocker block(s);
    s->setSizes(sizes);
}
} // namespace

void MetricsPanel::reflowMetrics()
{
    if (!m_metricsScroll || m_metricsCards.isEmpty())
        return;
    const int h = m_metricsScroll->viewport()->height();
    const int n = m_metricsCards.size();
    int cols;
    if (h >= 3 * kCardRowH)      cols = 5;   // tall: the designed five-column grid
    else if (h >= 2 * kCardRowH) cols = 6;   // medium: two rows
    else                         cols = n;   // short: a single row (scrolls sideways)
    reflowMetricsGrid(cols);
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
        // The last card spans any leftover columns so there's no empty cell.
        const int span = (i == n - 1) ? (cols - cc) : 1;
        m_metricsGrid->addWidget(m_metricsCards[i], r, cc, 1, span);
        // Bottom-row cells drop their bottom border; re-polish so the dynamic
        // property change takes effect in the stylesheet.
        const bool lastRow = (r == rows - 1);
        if (m_metricsCards[i]->property("lastrow").toBool() != lastRow)
        {
            m_metricsCards[i]->setProperty("lastrow", lastRow);
            m_metricsCards[i]->style()->unpolish(m_metricsCards[i]);
            m_metricsCards[i]->style()->polish(m_metricsCards[i]);
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

    if (m_leftSplit && m_leftSplit->count() > 1 && m_leftSplit->height() > 0)
    {
        if (m_metricsMinimised)
        {
            // Collapse the metrics to nothing but keep it visible: the node
            // tree fills the column and the divider bar sits at the bottom,
            // still draggable.
            const int avail = qMax(0, m_leftSplit->height() - m_leftSplit->handleWidth());
            const QList<int> want{ avail, 0 };
            if (m_leftSplit->sizes() != want)
            {
                QSignalBlocker block(m_leftSplit);
                m_leftSplit->setSizes(want);
            }
        }
        // Auto-reveal only until the user takes manual control of the column;
        // after that (and when not minimised) leave its split to the user.
        else if (!m_leftManual)
        {
            revealStack(m_leftSplit, { kTreeReveal, kMetricsReveal });
        }
    }
    if (m_rightSplit && m_rightSplit->height() > 0 && !m_rightManual)
        revealStack(m_rightSplit, { kLogReveal, kLogReveal, kLogReveal });

    positionMetricsToggle();
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
    const int btnW = m_metricsToggle->width();
    const int btnH = m_metricsToggle->height();
    // Centre the grip on the divider. When minimised the divider sits at the
    // bottom; take it from the geometry directly rather than sizes(), which can
    // be momentarily stale after a toggle.
    int dividerCentre;
    if (m_metricsMinimised)
    {
        dividerCentre = h - hw / 2;
    }
    else
    {
        const QList<int> sizes = m_leftSplit->sizes();
        dividerCentre = (sizes.isEmpty() ? h : sizes[0]) + hw / 2;
    }
    const int x = m_leftSplit->width() - btnW - 6;          // right edge, small inset
    int y = qBound(0, dividerCentre - btnH / 2, qMax(0, h - btnH));
    m_metricsToggle->move(x, y);
}

void MetricsPanel::dragMetricsDividerTo(const QPoint& globalPos)
{
    if (!m_leftSplit || m_leftSplit->count() < 2)
        return;
    const int hw = m_leftSplit->handleWidth();
    const int avail = qMax(0, m_leftSplit->height() - hw);
    // Cursor in the column's coords → node-tree height (divider top).
    const int y = m_leftSplit->mapFromGlobal(globalPos).y();
    const int node = qBound(0, y - hw / 2, avail);
    const QList<int> want{ node, avail - node };
    if (m_leftSplit->sizes() != want)
    {
        QSignalBlocker block(m_leftSplit);
        m_leftSplit->setSizes(want);
    }
    m_leftManual = true;
    const int metrics = avail - node;
    if (metrics > 0)
        m_savedMetricsH = metrics;
    const bool collapsed = metrics <= 2;
    if (m_metricsMinimised != collapsed)
    {
        m_metricsMinimised = collapsed;
        updateChevron();
    }
    positionMetricsToggle();
}

void MetricsPanel::toggleMetrics()
{
    m_metricsMinimised = !m_metricsMinimised;

    // Collapse the metrics to nothing on minimise; restore the remembered
    // height (or the reveal default) on show.
    if (m_leftSplit && m_leftSplit->count() > 1 && m_leftSplit->height() > 0)
    {
        const int avail = qMax(0, m_leftSplit->height() - m_leftSplit->handleWidth());
        const QList<int> cur = m_leftSplit->sizes();
        QList<int> want;
        if (m_metricsMinimised)
        {
            if (cur.size() > 1 && cur[1] > 0)
                m_savedMetricsH = cur[1];   // remember for restore
            want = { avail, 0 };
        }
        else
        {
            const int target = m_savedMetricsH > 0 ? m_savedMetricsH : kMetricsReveal;
            const int m = qBound(0, target, avail);
            want = { avail - m, m };
        }
        if (cur != want)
        {
            QSignalBlocker block(m_leftSplit);
            m_leftSplit->setSizes(want);
        }
    }

    updateChevron();
    positionMetricsToggle();
}

void MetricsPanel::updateChevron()
{
    if (!m_metricsToggle)
        return;
    // Points down when the metrics are shown (click to collapse), up when
    // minimised (click to show). The glyph is painted by ChevronButton.
    m_metricsToggle->setDir(m_metricsMinimised ? ChevronButton::Up : ChevronButton::Down);
    m_metricsToggle->setToolTip(m_metricsMinimised ? tr("Show metrics") : tr("Minimise metrics"));
}
