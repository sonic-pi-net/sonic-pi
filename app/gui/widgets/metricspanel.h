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

#ifndef METRICSPANEL_H
#define METRICSPANEL_H

#include <QColor>
#include <QList>
#include <QString>
#include <QVector>
#include <QWidget>
#include <cstdint>
#include <memory>
#include <vector>

#include "thinsplitter.h"

class QLabel;

class QTimer;
class QShowEvent;
class QHideEvent;
class QTextEdit;
class QSplitter;
class QToolButton;
class QVBoxLayout;
class QFrame;
class QGridLayout;
class QScrollArea;
class QUdpSocket;
class QVariantAnimation;
class ChevronButton;
class NodeTreeGraph;
class SonicPiTheme;

namespace SonicPi
{
class SonicPiAPI;
}

// A coloured text run for the OSC/debug logs. These are built and inserted via
// QTextCharFormat instead of generating HTML and calling insertHtml(), which
// skips the rich-text HTML parser on the per-message hot path. An invalid colour
// means "use the view's default foreground".
struct LogRun { QColor color; QString text; };

// Live engine performance dashboard: reads the PerformanceMetrics the engine
// publishes into shared memory (via SonicPiAPI::AudioProcessor_GetMetrics) and
// renders it. Polling is gated on visibility. Fields with no native writer
// render as "-".
class MetricsPanel : public QWidget
{
    Q_OBJECT
public:
    explicit MetricsPanel(std::shared_ptr<SonicPi::SonicPiAPI> api, QWidget* parent = nullptr);

    // Apply the active Sonic Pi theme (log fg/bg + syntax colours for the
    // OSC/node rendering).
    void applyTheme(SonicPiTheme* theme);

    // Show/hide the card titles (DEBUG / scsynth / Link / …), following the
    // "show pane titles" preference like the dock titles do.
    void setTitlesVisible(bool visible);

    QSize sizeHint() const override;
    QSize minimumSizeHint() const override;

protected:
    void showEvent(QShowEvent* e) override;
    void hideEvent(QHideEvent* e) override;
    // Watches the vertical column splitters for resizes so the reveal layout
    // tracks the available height (the panel itself doesn't resize when only
    // an inner splitter does).
    bool eventFilter(QObject* obj, QEvent* e) override;

private slots:
    void refresh();

private:
    // Runtime binding from a static layout row to the widgets that render it.
    struct ValueRowUi
    {
        const void* def;     // const RowDef* (opaque to the header)
        QLabel* value;
        QString lastHtml;
    };

    // Private read cursor for passively tailing a transport ring (we never
    // touch the engine's consumer tail). `primed` is false until the first
    // refresh seeds the cursor at the current head (a live tail, no replay).
    struct RingCursor
    {
        int32_t pos = 0;
        bool primed = false;
    };

    void buildUi();
    void renderDisconnected();
    QColor kindColor(int kind) const;

    // OSC in/out + debug logs and the node-tree graph, fed from the engine's
    // shm rings + node-tree mirror.
    void buildNodeColumn(QSplitter* topRow);  // node-tree graph (right)
    void buildLogs(QSplitter* vsplit);        // tabbed OSC in/out + debug (bottom)
    // Lay out both vertical columns as a progressive top-down reveal: each
    // widget grows to its reveal height before the next appears; once all are
    // revealed the surplus is shared evenly between them.
    void revealColumns();
    // Re-flow the metric cards by the available height: the full 5-column grid
    // when tall, fewer rows when shorter, a single row when there's little
    // space. reflowMetricsGrid does the placement for a given column count.
    void reflowMetrics();
    void reflowMetricsGrid(int cols);
    // Pin the metrics pane to exactly the height its current 1 or 2 rows need,
    // so it never stretches and the node tree above takes any spare height.
    void updateMetricsHeight();
    // Seed the main (tree+metrics | logs) split to the golden ratio, once,
    // when it first has a real width.
    void seedMainSplit();
    // Chevron on the node-tree / metrics divider: collapse or restore the
    // metrics grid.
    void toggleMetrics();
    void updateChevron();
    // Place the chevron knob onto the current node-tree / metrics divider.
    void positionMetricsToggle();
    // Chevron on the main (left columns | logs) divider: collapse the logs
    // column to the right, or restore it. Mirrors the metrics chevron, rotated.
    void toggleLogs();
    void updateLogsChevron();
    void positionLogsToggle();
    // Ease the metrics pane height / logs column width to a target instead of
    // snapping, so collapse and restore animate.
    void animateMetrics(int targetHeight);
    void animateLogs(int targetLogsWidth);
    // Show or hide a splitter's own handle line (used for the main divider, whose
    // full-height line comes from the handle rather than the short chevron band).
    void setDividerLineVisible(ThinSplitter* s, bool visible);
    // Ask SuperSonic (once, after we're tailing the debug ring) to push its
    // build/runtime summary down the debug channel so it shows in the Info pane.
    void requestSupersonicSummary();
    void drainOscRing(bool outgoing);   // outgoing = IN ring (sent), else OUT ring (replies)
    void drainEgressRing(bool nrt);     // OUT (false) / NRT-out (true): /supersonic/debug → Debug pane, rest → From-SuperSonic
    void updateNodeTree();
    QVector<LogRun> stampRuns() const;   // bracketed hi-res timestamp, alternating grey shades
    QVector<LogRun> formatOscRuns(const uint8_t* data, uint32_t size,
                                  uint32_t sourceId);

    std::shared_ptr<SonicPi::SonicPiAPI> m_api;
    QTimer* m_timer = nullptr;
    SonicPiTheme* m_theme = nullptr;  // active theme, for syntax colours

    ThinSplitter* m_mainSplit = nullptr; // left (tree + metrics) | right (logs)
    QSplitter* m_leftSplit = nullptr;    // node tree / metrics (chevron paints line)
    ThinSplitter* m_rightSplit = nullptr;// debug / to / from logs

    QVector<QFrame*> m_metricsCards;  // metric cards, in order, re-flowed by height
    QGridLayout* m_metricsGrid = nullptr;
    QScrollArea* m_metricsScroll = nullptr;
    int m_metricsCols = 0;            // current column count of the metric grid
    ChevronButton* m_metricsToggle = nullptr; // chevron grip on the tree/metrics divider
    ChevronButton* m_logsToggle = nullptr;    // chevron grip on the main (logs) divider
    bool m_splitInit = false;         // seed the main (horizontal) split once, on first show
    bool m_metricsMinimised = false;  // user collapsed the metrics via the chevron
    bool m_logsMinimised = false;     // user collapsed the logs column via the chevron
    QList<int> m_savedMainSizes;      // main-split sizes to restore when logs reappear
    QVariantAnimation* m_metricsAnim = nullptr; // eases the metrics pane height
    QVariantAnimation* m_logsAnim = nullptr;    // eases the logs column width
    bool m_revealing = false;         // re-entrancy guard for revealColumns()
    bool m_rightManual = false;       // user dragged a right column divider — stop auto-revealing it
    int m_metricsNeededH = 0;         // fixed height the current 1/2 rows of cards need

    QColor m_textColor;
    QColor m_bgColor;
    QColor m_borderColor;

    QVector<ValueRowUi> m_valueRows;
    QVector<QLabel*> m_rowLabels; // row labels + card titles, recoloured on theme change

    QTextEdit* m_oscOutView = nullptr;   // OSC host→engine (what Sonic Pi sent)
    QTextEdit* m_oscInView = nullptr;    // OSC engine→host (replies)
    QTextEdit* m_debugView = nullptr;    // engine debug/log text
    NodeTreeGraph* m_nodeGraph = nullptr;
    QLabel* m_treeStats = nullptr;       // node-tree legend + counts

    RingCursor m_inCursor;
    RingCursor m_outCursor;
    RingCursor m_debugCursor;
    QUdpSocket* m_summarySocket = nullptr;  // sends the one-shot /supersonic/summary request
    bool m_summaryRequested = false;
    std::vector<uint8_t> m_scratch;
    uint32_t m_lastTreeVersion = 0xFFFFFFFFu;
};

#endif // METRICSPANEL_H
