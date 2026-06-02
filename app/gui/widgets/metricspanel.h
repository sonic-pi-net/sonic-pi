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
#include <QString>
#include <QVector>
#include <QWidget>
#include <cstdint>
#include <memory>
#include <vector>

class QLabel;
class QProgressBar;
class QTimer;
class QShowEvent;
class QHideEvent;
class QTextEdit;
class QSplitter;
class QVBoxLayout;
class NodeTreeGraph;
class SonicPiTheme;

namespace SonicPi
{
class SonicPiAPI;
}

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

    QSize sizeHint() const override;
    QSize minimumSizeHint() const override;

protected:
    void showEvent(QShowEvent* e) override;
    void hideEvent(QHideEvent* e) override;

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
    struct BarRowUi
    {
        const void* def;     // const RowDef*
        QProgressBar* bar;
        QLabel* text;
        QString lastText;
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
    void drainOscRing(bool outgoing);   // outgoing = IN ring (sent), else OUT ring (replies)
    void drainDebugRing();
    void updateNodeTree();
    QString formatOscHtml(const uint8_t* data, uint32_t size,
                          uint32_t sequence, uint32_t sourceId, bool outgoing);

    std::shared_ptr<SonicPi::SonicPiAPI> m_api;
    QTimer* m_timer = nullptr;
    SonicPiTheme* m_theme = nullptr;  // active theme, for syntax colours

    QSplitter* m_mainSplit = nullptr; // left (tree + metrics) | right (logs)
    QSplitter* m_leftSplit = nullptr; // node tree / metrics table
    bool m_splitInit = false;         // seed split positions once, on first show

    QColor m_textColor;
    QColor m_bgColor;
    QColor m_borderColor;

    QVector<ValueRowUi> m_valueRows;
    QVector<BarRowUi> m_barRows;
    QVector<QLabel*> m_rowLabels; // row labels + card titles, recoloured on theme change

    QTextEdit* m_oscOutView = nullptr;   // OSC host→engine (what Sonic Pi sent)
    QTextEdit* m_oscInView = nullptr;    // OSC engine→host (replies)
    QTextEdit* m_debugView = nullptr;    // engine debug/log text
    NodeTreeGraph* m_nodeGraph = nullptr;
    QLabel* m_treeStats = nullptr;       // node-tree legend + counts

    RingCursor m_inCursor;
    RingCursor m_outCursor;
    RingCursor m_debugCursor;
    std::vector<uint8_t> m_scratch;
    uint32_t m_lastTreeVersion = 0xFFFFFFFFu;
};

#endif // METRICSPANEL_H
