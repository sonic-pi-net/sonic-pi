//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include "sonicpiscintilla.h"
#include "completionpopup.h"
#include "utils/scintilla_api.h"
#include "utils/completion_context.h"
#include "utils/flash_style.h"
#include "dpi.h"
#include "kiss_fftr.h"
#include <algorithm>
#include <cmath>
#include <iostream>
#include <QAccessible>
#include <QCheckBox>
#include <QLabel>
#include <QKeyEvent>
#include <QTimer>
#include <QFocusEvent>
#include <QMouseEvent>
#include <QWheelEvent>

#include <QDrag>
#include <QDragEnterEvent>
#include <QDropEvent>
#include <QRegularExpression>
#include <QSet>
#include <QMenu>
#include <QContextMenuEvent>
#include <QPainter>
#include <QImage>
#include <QColor>
#include <QFont>
#include <QFontMetrics>
#include <QPainterPath>
#include <QPolygonF>
#include <QSettings>
#include <QShortcut>
#include <Qsci/qscicommandset.h>
#include <Qsci/qscilexer.h>
#include <Qsci/qscilexerruby.h>
#include <QPainter>
#if (QT_VERSION >= QT_VERSION_CHECK(5, 14, 0))
#include <QRecursiveMutex>
#endif

// Container indicator (>= INDICATOR_CONTAINER) for the error underline.
static const int kErrorIndicator = 20;

// Transient trigger flash (distinct from the error markers/indicator so the
// two can coexist): indicator 21 washes the line's code text, marker 12 is a
// small dot in the gutter gap between the line numbers and the code.
static const int kFlashIndicator = 21;
static const int kFlashGutterMarker = 12;

// How long a line stays lit after a flash — shared with every other flash
// renderer (see utils/flash_style.h).
static const int kFlashHoldMs = SonicPi::kFlashHoldMs;

// Tiny inline oscilloscope + spectrum pinned to a live_loop's header line,
// fed by the loop's own scope-buffer tap (with_fx :scope_out). Decorative:
// mouse-transparent, no focus. Drawing mirrors the Examples jukebox scope
// (tutorialpane.cpp) at postage-stamp size, plus FFT bars behind the trace.
// The whole box warms from grey to the accent colour with the signal level so
// a quiet loop reads as dormant at a glance. SonicPiScintilla owns
// positioning and polling via its shared timer.
class LiveLoopScopeWidget : public QWidget
{
public:
    // Analysis sizes: last 512 samples of the tap, 20 log-spaced bars.
    static const int kFftSize = 512;
    static const int kBars = 20;

    explicit LiveLoopScopeWidget(QWidget* parent)
        : QWidget(parent)
    {
        setAttribute(Qt::WA_TransparentForMouseEvents, true);
        setFocusPolicy(Qt::NoFocus);
    }

    ~LiveLoopScopeWidget() override
    {
        if (m_fftCfg)
            kiss_fftr_free(m_fftCfg);
    }

    void setReader(const shm_scope_buffer_reader& r) { m_reader = r; }

    void setColours(const QColor& wave, const QColor& quiet, const QColor& panel)
    {
        m_wave = wave;
        m_quiet = quiet;
        m_panel = panel;
        update();
    }

    void poll()
    {
        unsigned int frames = 0;
        if (!m_reader.pull(frames) || frames == 0)
        {
            // No fresh audio: decay the level so the box cools back to grey.
            m_level *= 0.86f;
            decayBars();
            update();
            return;
        }
        float* d = m_reader.data();
        if (!d)
            return;
        unsigned int stride = m_reader.max_frames();
        unsigned int ch = m_reader.channels();
        m_samples.resize(frames);
        float peak = 0.0f;
        for (unsigned int i = 0; i < frames; i++)
        {
            float v = ch >= 2 ? 0.5f * (d[i] + d[stride + i]) : d[i];
            m_samples[i] = v;
            peak = qMax(peak, qAbs(v));
        }
        // Fast attack, ~0.5s decay: the colour snaps on with a hit and fades out.
        m_level = qMax(peak, m_level * 0.86f);
        updateSpectrum();
        update();
    }

protected:
    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing, true);
        const qreal w = width();
        const qreal h = height();
        const qreal mid = h / 2.0;
        const qreal radius = ScaleWidthForDPI(3);

        // 0 = dormant grey, 1 = full accent. Mapped in dB so colour means
        // "playing", not "loud": silence (< -60dB) stays grey and anything
        // above -40dB is fully lit, however quiet the mix level.
        const qreal db = 20.0 * std::log10((double)qMax(m_level, 1e-6f));
        const qreal heat = qBound(0.0, (db + 60.0) / 20.0, 1.0);
        QColor wave = lerp(m_quiet, m_wave.isValid() ? m_wave : palette().highlight().color(), heat);

        QRectF panelRect(0.5, 0.5, w - 1.0, h - 1.0);
        if (m_panel.isValid())
        {
            p.setPen(Qt::NoPen);
            p.setBrush(m_panel);
            p.drawRoundedRect(panelRect, radius, radius);
        }
        QPainterPath clip;
        clip.addRoundedRect(panelRect, radius, radius);
        p.setClipPath(clip);

        // Spectrum: bottom-anchored log-spaced bars behind the trace.
        if (!m_bars.empty())
        {
            QColor barCol = wave;
            barCol.setAlpha(55);
            p.setPen(Qt::NoPen);
            p.setBrush(barCol);
            const qreal barW = w / m_bars.size();
            for (size_t i = 0; i < m_bars.size(); i++)
            {
                qreal bh = qBound(0.0, (double)m_bars[i], 1.0) * (h - 2.0);
                if (bh < 0.5)
                    continue;
                p.drawRect(QRectF(i * barW + 0.5, h - 1.0 - bh, barW - 1.0, bh));
            }
        }

        QColor base = wave;
        base.setAlpha(60);
        p.setPen(QPen(base, 1.0));
        p.drawLine(QPointF(0, mid), QPointF(w, mid));

        if (m_samples.size() >= 2 && w >= 2)
        {
            const qreal amp = mid * 0.9;
            const size_t n = m_samples.size();
            const int cols = qMax(2, (int)w);
            QPainterPath line;
            for (int x = 0; x < cols; x++)
            {
                size_t idx = (size_t)((qreal)x / (cols - 1) * (n - 1));
                qreal v = qBound(-1.0, (double)m_samples[idx], 1.0);
                qreal y = mid - v * amp;
                qreal px = (qreal)x / (cols - 1) * w;
                if (x == 0)
                    line.moveTo(px, y);
                else
                    line.lineTo(px, y);
            }
            QPainterPath body = line;
            body.lineTo(w, mid);
            body.lineTo(0, mid);
            body.closeSubpath();
            QColor fill = wave;
            fill.setAlpha(60);
            p.fillPath(body, fill);

            QPen wavePen(wave);
            wavePen.setWidthF(1.6);
            wavePen.setJoinStyle(Qt::RoundJoin);
            wavePen.setCapStyle(Qt::RoundCap);
            p.setPen(wavePen);
            p.drawPath(line);
        }

    }

private:
    static QColor lerp(const QColor& a, const QColor& b, qreal t)
    {
        return QColor(a.red() + (int)((b.red() - a.red()) * t),
                      a.green() + (int)((b.green() - a.green()) * t),
                      a.blue() + (int)((b.blue() - a.blue()) * t),
                      a.alpha() + (int)((b.alpha() - a.alpha()) * t));
    }

    void decayBars()
    {
        for (float& b : m_bars)
            b *= 0.8f;
    }

    void updateSpectrum()
    {
        if (!m_fftCfg)
        {
            m_fftCfg = kiss_fftr_alloc(kFftSize, 0, 0, 0);
            m_fftIn.resize(kFftSize);
            m_fftOut.resize(kFftSize / 2 + 1);
            m_bars.assign(kBars, 0.0f);
        }
        // Last kFftSize samples, Hann-windowed (zero-padded when short).
        const int n = (int)m_samples.size();
        for (int i = 0; i < kFftSize; i++)
        {
            int src = n - kFftSize + i;
            float v = (src >= 0 && src < n) ? m_samples[src] : 0.0f;
            const float kTau = 6.283185307f;
            float win = 0.5f * (1.0f - std::cos(kTau * i / (kFftSize - 1)));
            m_fftIn[i] = v * win;
        }
        kiss_fftr(m_fftCfg, m_fftIn.data(), m_fftOut.data());

        // Log-spaced buckets over bins 1..N/2; sqrt of peak magnitude per
        // bucket for a musical-feeling scale. Bars decay so hits linger.
        const int maxBin = kFftSize / 2;
        for (int b = 0; b < kBars; b++)
        {
            int lo = (int)std::pow((double)maxBin, (double)b / kBars);
            int hi = (int)std::pow((double)maxBin, (double)(b + 1) / kBars);
            lo = qMax(1, lo);
            hi = qMax(lo + 1, hi);
            hi = qMin(hi, maxBin + 1);
            float mag = 0.0f;
            for (int k = lo; k < hi; k++)
            {
                float m = std::sqrt(m_fftOut[k].r * m_fftOut[k].r + m_fftOut[k].i * m_fftOut[k].i);
                mag = qMax(mag, m);
            }
            float v = std::sqrt(mag / (kFftSize / 8.0f));
            m_bars[b] = qMax(v, m_bars[b] * 0.8f);
        }
    }

    QColor m_wave;
    QColor m_quiet;
    QColor m_panel;
    std::vector<float> m_samples;
    std::vector<float> m_bars;
    float m_level = 0.0f;
    shm_scope_buffer_reader m_reader;
    kiss_fftr_cfg m_fftCfg = nullptr;
    std::vector<kiss_fft_scalar> m_fftIn;
    std::vector<kiss_fft_cpx> m_fftOut;
};

SonicPiScintilla::SonicPiScintilla(SonicPiLexer* lexer, SonicPiTheme* theme, QString fileName, bool autoIndent)
    : QsciScintilla()
{
    setAcceptDrops(true);

    this->theme = theme;
    this->fileName = fileName;
    this->autoIndent = autoIndent;
    this->selectionMode = false;
    standardCommands()->clearKeys();
    standardCommands()->clearAlternateKeys();
    QString skey;
    QSettings settings(QSettings::IniFormat, QSettings::UserScope, "sonic-pi.net", "scintilla-key-bindings");
#if (QT_VERSION >= QT_VERSION_CHECK(5, 14, 0))
    mutex = new QRecursiveMutex();
#else
    mutex = new QMutex(QMutex::Recursive);
#endif

#if defined(Q_OS_MAC)
    int SPi_CTRL = Qt::META;
    int SPi_META = Qt::CTRL;
#else
    int SPi_CTRL = Qt::CTRL;
    int SPi_META = Qt::ALT;
#endif

    // basic navigation
    addKeyBinding(settings, QsciCommand::PageDown, Qt::Key_PageDown);
    addKeyBinding(settings, QsciCommand::PageUp, Qt::Key_PageUp);
    addOtherKeyBinding(settings, QsciCommand::LineDown, Qt::Key_Down);
    addKeyBinding(settings, QsciCommand::LineDownExtend, Qt::Key_Down | Qt::SHIFT);
    addOtherKeyBinding(settings, QsciCommand::LineUp, Qt::Key_Up);
    addKeyBinding(settings, QsciCommand::LineUpExtend, Qt::Key_Up | Qt::SHIFT);
    addOtherKeyBinding(settings, QsciCommand::CharRight, Qt::Key_Right);
    addKeyBinding(settings, QsciCommand::CharRightExtend, Qt::Key_Right | Qt::SHIFT);
    addOtherKeyBinding(settings, QsciCommand::CharLeft, Qt::Key_Left);
    addKeyBinding(settings, QsciCommand::CharLeftExtend, Qt::Key_Left | Qt::SHIFT);
    addOtherKeyBinding(settings, QsciCommand::Delete, Qt::Key_Delete);
    addOtherKeyBinding(settings, QsciCommand::DeleteBack, Qt::Key_Backspace);
    addKeyBinding(settings, QsciCommand::VCHome, Qt::Key_Home);
    addKeyBinding(settings, QsciCommand::VCHomeExtend, Qt::Key_Home | Qt::SHIFT);
    addOtherKeyBinding(settings, QsciCommand::LineEnd, Qt::Key_End);
    addKeyBinding(settings, QsciCommand::LineEndExtend, Qt::Key_End | Qt::SHIFT);
    addKeyBinding(settings, QsciCommand::Backtab, Qt::Key_Tab | Qt::SHIFT);

    standardCommands()->readSettings(settings);

    this->setMatchedBraceBackgroundColor(theme->color("MatchedBraceBackground"));
    this->setMatchedBraceForegroundColor(theme->color("MatchedBraceForeground"));

    setIndentationWidth(ScaleHeightForDPI(2));
    setIndentationGuides(true);
    setIndentationGuidesForegroundColor(theme->color("IndentationGuidesForeground"));
    setBraceMatching(SonicPiScintilla::SloppyBraceMatch);

    // Drop the default sunken frame so the editor's scrollbars sit flush to the
    // edge, matching every other pane's shared 2dx scrollbar offset.
    setFrameShape(QFrame::NoFrame);

    // TODO: add preference toggle for this:
    // this->setFolding(SonicPiScintilla::CircledTreeFoldStyle, 2);
    setCaretLineVisible(true);
    setCaretLineBackgroundColor(theme->color("CaretLineBackground"));
    setFoldMarginColors(theme->color("FoldMarginForeground"), theme->color("FoldMarginForeground"));
    setMarginLineNumbers(0, true);

    setMarginsBackgroundColor(theme->color("MarginBackground"));
    setMarginsForegroundColor(theme->color("MarginForeground"));
    setMarginsFont(QFont("Hack", 15, -1, true));
    setUtf8(true);
    setText("# Loading previous buffer contents. Please wait...");
    setLexer((QsciLexer*)lexer);

    // marker 9: translucent full-line wash over the code area.
    markerDefine(QsciScintilla::Background, 9);
    setMarkerBackgroundColor(theme->color("MarkerBackground"), 9);
    SendScintilla(SCI_MARKERSETALPHA, 9, 40);

    // Trigger pulse used by flashLine: a box behind just the code text (drawn
    // under it), or the gutter dot (image marker built in
    // applyFlashMarkerColours, sized by updateErrorMarginWidth). STRAIGHTBOX
    // not FULLBOX: it hugs the glyphs, so the extra descent added below each
    // line (squiggle room) stays unwashed and the pulse sits centred on the text.
    SendScintilla(SCI_INDICSETSTYLE, (unsigned long)kFlashIndicator, (long)INDIC_STRAIGHTBOX);
    SendScintilla(SCI_INDICSETUNDER, (unsigned long)kFlashIndicator, (long)1);
    applyFlashMarkerColours();

    // Follow insert/delete edits so run-time flash anchors track live edits.
    SendScintilla(SCI_SETMODEVENTMASK,
                  SendScintilla(SCI_GETMODEVENTMASK) | SC_MOD_INSERTTEXT | SC_MOD_DELETETEXT);
    connect(this, &QsciScintillaBase::SCN_MODIFIED, this,
            [this](int position, int modificationType, const char*, int length, int, int, int,
                   int, int, int) { trackEditForFlash(position, modificationType, length); });

    // No blank inset before the text, so the error line's wash meets the symbol
    // margin with no untinted seam.
    SendScintilla(SCI_SETMARGINLEFT, (unsigned long)0, (long)0);

    // marker 8 (gutter dot) and marker 10 (number-margin tint) are RGBA images
    // sized to the live line height / margin width, so setLineErrorMarker builds
    // them. Route marker 10 to the number margin only, not the symbol margin.
    // The flash dot (12) lives in the symbol margin only.
    SendScintilla(SCI_SETMARGINMASKN, (unsigned long)0, (long)(1 << 10));
    long errSymMask = SendScintilla(SCI_GETMARGINMASKN, (unsigned long)1);
    SendScintilla(SCI_SETMARGINMASKN, (unsigned long)1,
                  (long)((errSymMask & ~(1 << 10)) | (1 << kFlashGutterMarker)));

    // Zig-zag (squiggle) underline beneath the offending code on the error line.
    // An indicator is vector-drawn by Scintilla, so it tracks zoom and edits for
    // free; its colour is set per error (pink runtime / blue syntax) in
    // applyErrorMarkers. The squiggle itself is Sonic Pi's enlarged smooth
    // zig-zag (patched in QScintilla_src .../Indicator.cpp) matching the error
    // card's; the extra descent below grants it room to sit clear of the text.
    SendScintilla(SCI_INDICSETSTYLE, (unsigned long)kErrorIndicator, (long)INDIC_SQUIGGLE);
    SendScintilla(SCI_SETEXTRADESCENT, (long)ScaleHeightForDPI(8));

    // Drive completion through our own popup (CompletionPopup) rather than
    // Scintilla's built-in list, so each row can show a kind badge + summary.
    setAutoCompletionSource(SonicPiScintilla::AcsNone);
    setAutoCompletionThreshold(-1);
    setAutoCompletionCaseSensitivity(false);
    m_completion = new CompletionPopup(this);
    m_completion->applyTheme(theme->color("Background"), theme->color("Foreground"),
                             theme->color("HighlightedBackground"),
                             theme->contrastingText(theme->color("HighlightedBackground")));
    // Clicking the mini piano accepts that note like Tab/Return.
    connect(m_completion, &CompletionPopup::accepted, this, [this]() { acceptCompletion(); });
    // The popup's close button cancels completion exactly like Escape.
    connect(m_completion, &CompletionPopup::dismissRequested, this, [this]() {
        if (m_pvSlider) restoreOriginal(); else clearPreview();
        endPreview();
        m_completion->hidePopup();
    });
    // Live-preview the selected entry (list navigation, note, slider drag) in the
    // buffer in place of the typed word.
    connect(m_completion, &CompletionPopup::previewChanged, this,
            [this](const QString& text) { applyPreview(text); });
    // Backstop refresh for buffer changes that don't arrive via the keypress path
    // (e.g. programmatic edits). The keypress path is the authoritative trigger
    // (it runs after the caret settles); this only re-filters an already-open
    // popup, guarded by isShowing(). Skip our own preview edits (m_pvGuard).
    connect(this, &QsciScintilla::textChanged, this, [this]() {
        if (m_pvGuard) return;
        if (m_completion && m_completion->isShowing()) updateCompletion();
    });
    // The popup's "Docs" button opens the help pane (handled by MainWindow).
    connect(m_completion, &CompletionPopup::docsRequested, this,
            [this](const QString& name) { m_completion->hidePopup(); emit docsRequested(name); });
    // Relay popup navigation announcements up to MainWindow's screen-reader helper.
    connect(m_completion, &CompletionPopup::announceRequested, this,
            &SonicPiScintilla::announceRequested);
    // Relay synth/fx audition requests up to MainWindow, which runs the snippet.
    connect(m_completion, &CompletionPopup::auditionRequested, this,
            &SonicPiScintilla::auditionRequested);

    setSelectionBackgroundColor(theme->color("SelectionBackground"));
    setSelectionForegroundColor(theme->contrastingText(theme->color("SelectionBackground")));
    setCaretWidth(ScaleHeightForDPI(5));
    setCaretForegroundColor(theme->color("CaretForeground"));
    setEolMode(EolUnix);

    SendScintilla(SCI_SETWORDCHARS, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789:_?!");
}

void SonicPiScintilla::redraw()
{
    mutex->lock();
    setMarginsBackgroundColor(theme->color("MarginBackground"));
    setMarginsForegroundColor(theme->color("MarginForeground"));
    setSelectionBackgroundColor(theme->color("SelectionBackground"));
    setSelectionForegroundColor(theme->contrastingText(theme->color("SelectionBackground")));
    setCaretLineBackgroundColor(theme->color("CaretLineBackground"));
    setFoldMarginColors(theme->color("FoldMarginForeground"), theme->color("FoldMarginForeground"));
    setIndentationGuidesForegroundColor(theme->color("IndentationGuidesForeground"));
    setMatchedBraceBackgroundColor(theme->color("MatchedBraceBackground"));
    setMatchedBraceForegroundColor(theme->color("MatchedBraceForeground"));
    if (m_completion)
    {
        m_completion->applyTheme(theme->color("Background"), theme->color("Foreground"),
                                 theme->color("HighlightedBackground"),
                                 theme->contrastingText(theme->color("HighlightedBackground")));
    }
    // Re-tint the error-line markers (gutter dot/washes + squiggle) so a visible
    // error tracks the new theme (applyErrorMarkers runs under the held mutex).
    if (m_errorLine >= 0)
        applyErrorMarkers(m_errorLine);
    applyFlashMarkerColours();
    applyLoopScopeColours();
    mutex->unlock();
}

void SonicPiScintilla::applyFlashMarkerColours()
{
    // Code wash: translucent accent behind the text.
    QColor accent = theme->color("HighlightedBackground");
    SendScintilla(SCI_INDICSETFORE, (unsigned long)kFlashIndicator, accent);
    SendScintilla(SCI_INDICSETALPHA, kFlashIndicator, m_flashAlpha);
    SendScintilla(SCI_INDICSETOUTLINEALPHA, kFlashIndicator, m_flashAlpha);

    // Gutter dot: a small accent disc centred in the symbol-margin gap. An
    // image marker (like the error dot) so it can be smaller than Scintilla's
    // own circle; rebuilt per theme/zoom since images don't scale themselves.
    int h = SendScintilla(SCI_TEXTHEIGHT, (unsigned long)0);
    int gapW = SendScintilla(SCI_GETMARGINWIDTHN, (unsigned long)1);
    if (h <= 0 || gapW <= 0)
        return;
    qreal dpr = devicePixelRatioF();
    if (dpr < 1.0) dpr = 1.0;
    QImage dot(qRound(gapW * dpr), qRound(h * dpr), QImage::Format_ARGB32);
    dot.fill(Qt::transparent);
    {
        QPainter dp(&dot);
        dp.setRenderHint(QPainter::Antialiasing, true);
        dp.setPen(Qt::NoPen);
        dp.setBrush(accent);
        // Centre on the text, not the cell: the cell's extra descent (squiggle
        // room) sits below the glyphs.
        int extraDescent = SendScintilla(SCI_GETEXTRADESCENT);
        qreal cy = ((h - extraDescent) / 2.0) * dpr;
        qreal d = qMin(gapW, h - extraDescent) * 0.55 * dpr;
        dp.drawEllipse(QRectF((dot.width() - d) / 2.0, cy - d / 2.0, d, d));
    }
    dot.setDevicePixelRatio(dpr);
    SendScintilla(SCI_RGBAIMAGESETSCALE, (unsigned long)qRound(dpr * 100));
    markerDefine(dot, kFlashGutterMarker);
}

void SonicPiScintilla::highlightCurrentLine()
{
    mutex->lock();
    setCaretLineBackgroundColor(theme->color("SelectionBackground"));
    mutex->unlock();
}

void SonicPiScintilla::unhighlightCurrentLine()
{
    mutex->lock();
    setCaretLineBackgroundColor(theme->color("CaretLineBackground"));
    mutex->unlock();
}

void SonicPiScintilla::snapshotRunLines()
{
    // Anchor each run-time line to its start position; trackEditForFlash nudges
    // these as the buffer is edited so flashRunLine can resolve the current line.
    int n = lines();
    m_runLinePos.resize(n);
    for (int i = 0; i < n; i++)
        m_runLinePos[i] = (int)SendScintilla(SCI_POSITIONFROMLINE, (unsigned long)i);
}

void SonicPiScintilla::remapFlashAnchorsAcrossReplace(const QVector<int>& oldAnchorLines,
                                                      const QStringList& oldStripped,
                                                      const QStringList& newStripped)
{
    const int oldN = oldStripped.size();
    const int newN = newStripped.size();
    // Longest matching run of lines from the top and from the bottom (indent is
    // ignored). Everything between is the changed region.
    int pre = 0;
    while (pre < oldN && pre < newN && oldStripped[pre] == newStripped[pre])
        pre++;
    int suf = 0;
    while (suf < oldN - pre && suf < newN - pre && oldStripped[oldN - 1 - suf] == newStripped[newN - 1 - suf])
        suf++;
    const int delta = newN - oldN;

    m_runLinePos.resize(oldAnchorLines.size());
    for (int i = 0; i < oldAnchorLines.size(); i++)
    {
        int L = oldAnchorLines[i];
        int nl;
        if (L < pre)
            nl = L; // unchanged prefix
        else if (L >= oldN - suf)
            nl = L + delta; // unchanged suffix, shifted by the size change
        else
            nl = qBound(pre, L, newN - 1); // inside the edited region, best effort
        nl = qBound(0, nl, newN > 0 ? newN - 1 : 0);
        m_runLinePos[i] = (int)SendScintilla(SCI_POSITIONFROMLINE, (unsigned long)nl);
    }
}

void SonicPiScintilla::trackEditForFlash(int position, int modificationType, int length)
{
    // Full-buffer replaces are handled by remapFlashAnchorsAcrossReplace, which
    // has the before/after line context; ignore their piecemeal notifications.
    if (m_runLinePos.isEmpty() || m_inReplaceBuffer)
        return;
    // Shift anchors at or after an insertion forward; pull anchors after a
    // deletion back (clamped into the deleted range's start).
    if (modificationType & SC_MOD_INSERTTEXT)
    {
        for (int& p : m_runLinePos)
            if (p >= position)
                p += length;
    }
    else if (modificationType & SC_MOD_DELETETEXT)
    {
        for (int& p : m_runLinePos)
            if (p > position)
                p = qMax(position, p - length);
    }
}

// Map a run-time line to its current line via the edit-tracking anchors
// (identity when the line predates the anchors).
int SonicPiScintilla::runLineToCurrent(int runLine)
{
    if (runLine >= 0 && runLine < m_runLinePos.size())
        return (int)SendScintilla(SCI_LINEFROMPOSITION, (unsigned long)m_runLinePos[runLine]);
    return runLine;
}

void SonicPiScintilla::flashRunLine(int runLine, bool codeWash, bool gutterDot)
{
    if (runLine < 0)
        return;
    flashLine(runLineToCurrent(runLine), codeWash, gutterDot);
}

void SonicPiScintilla::setLiveLoopScope(const QString& name, int runLine,
                                        const shm_scope_buffer_reader& reader)
{
    LiveLoopScopeWidget* w = m_loopScopes.value(name);
    if (!w)
    {
        w = new LiveLoopScopeWidget(viewport());
        m_loopScopes[name] = w;
    }
    w->setReader(reader);
    m_loopScopeLines[name] = runLine;
    applyLoopScopeColours();

    if (!m_loopScopeTimer)
    {
        m_loopScopeTimer = new QTimer(this);
        connect(m_loopScopeTimer, &QTimer::timeout, this, [this]() {
            positionLiveLoopScopes();
            for (LiveLoopScopeWidget* s : m_loopScopes)
                s->poll();
        });
    }
    if (!m_loopScopeTimer->isActive())
        m_loopScopeTimer->start(33);
    positionLiveLoopScopes();
}

void SonicPiScintilla::endLiveLoopScope(const QString& name)
{
    if (LiveLoopScopeWidget* w = m_loopScopes.take(name))
        w->deleteLater();
    m_loopScopeLines.remove(name);
    if (m_loopScopes.isEmpty() && m_loopScopeTimer)
        m_loopScopeTimer->stop();
}

void SonicPiScintilla::clearLiveLoopScopes()
{
    for (LiveLoopScopeWidget* w : m_loopScopes)
        w->deleteLater();
    m_loopScopes.clear();
    m_loopScopeLines.clear();
    if (m_loopScopeTimer)
        m_loopScopeTimer->stop();
}

void SonicPiScintilla::positionLiveLoopScopes()
{
    if (m_loopScopes.isEmpty())
        return;
    int lineH = SendScintilla(SCI_TEXTHEIGHT, (unsigned long)0);
    int extraDescent = SendScintilla(SCI_GETEXTRADESCENT);
    int h = lineH - extraDescent;
    int w = ScaleWidthForDPI(230);
    int viewW = viewport()->width();
    int viewH = viewport()->height();
    for (auto it = m_loopScopes.begin(); it != m_loopScopes.end(); ++it)
    {
        int cur = runLineToCurrent(m_loopScopeLines.value(it.key(), -1));
        if (cur < 0 || cur >= lines())
        {
            it.value()->hide();
            continue;
        }
        long pos = SendScintilla(SCI_POSITIONFROMLINE, (unsigned long)cur);
        int y = (int)SendScintilla(SCI_POINTYFROMPOSITION, (unsigned long)0, pos);
        if (y + h < 0 || y > viewH)
        {
            it.value()->hide();
            continue;
        }
        // Sit just after the line's text (the live_loop header's `do`),
        // clamped on-screen for long lines / narrow viewports.
        long endPos = SendScintilla(SCI_GETLINEENDPOSITION, (unsigned long)cur);
        int x = (int)SendScintilla(SCI_POINTXFROMPOSITION, (unsigned long)0, endPos)
              + ScaleWidthForDPI(12);
        x = qBound(0, x, qMax(0, viewW - w - ScaleWidthForDPI(6)));
        it.value()->setGeometry(x, y, w, h);
        it.value()->show();
        it.value()->raise();
    }
}

void SonicPiScintilla::applyLoopScopeColours()
{
    QColor wave = theme->color("Scope");
    // Dormant grey: the theme foreground faded right down, so a silent loop's
    // box sits quietly in any theme until signal warms it to the accent.
    QColor quiet = theme->color("Foreground");
    quiet.setAlpha(120);
    QColor panel = theme->color("Background");
    panel.setAlpha(215);
    for (LiveLoopScopeWidget* s : m_loopScopes)
        s->setColours(wave, quiet, panel);
}

void SonicPiScintilla::flashLine(int line, bool codeWash, bool gutterDot)
{
    if (line < 0 || line >= lines() || !(codeWash || gutterDot))
        return;
    // Re-flashing within the hold window extends the pulse: bump this line's
    // generation and only clear when the matching timer is still the latest.
    int gen = ++m_flashGen[line];
    if (gutterDot)
    {
        markerDelete(line, kFlashGutterMarker);
        markerAdd(line, kFlashGutterMarker);
    }
    if (codeWash)
    {
        // Wash just the code: first non-blank character to end of text.
        long start = SendScintilla(SCI_GETLINEINDENTPOSITION, (unsigned long)line);
        long end = SendScintilla(SCI_GETLINEENDPOSITION, (unsigned long)line);
        if (end > start)
        {
            SendScintilla(SCI_SETINDICATORCURRENT, (unsigned long)kFlashIndicator);
            SendScintilla(SCI_INDICATORFILLRANGE, (unsigned long)start, end - start);
        }
    }
    QTimer::singleShot(kFlashHoldMs, this, [this, line, gen]() {
        if (m_flashGen.value(line) == gen)
        {
            m_flashGen.remove(line);
            markerDelete(line, kFlashGutterMarker);
            clearFlashWash(line);
        }
    });
}

void SonicPiScintilla::setFlashBrightness(int percent)
{
    m_flashAlpha = qBound(0, percent * 255 / 100, 255);
    applyFlashMarkerColours();
}

void SonicPiScintilla::clearFlashWash(int line)
{
    SendScintilla(SCI_SETINDICATORCURRENT, (unsigned long)kFlashIndicator);
    if (m_flashGen.isEmpty())
    {
        // No pulse pending anywhere: sweep the whole document, so a wash whose
        // text was moved to another line mid-pulse can't linger.
        long len = SendScintilla(SCI_GETLENGTH);
        if (len > 0)
            SendScintilla(SCI_INDICATORCLEARRANGE, (unsigned long)0, len);
        return;
    }
    if (line < 0 || line >= lines())
        return;
    long start = SendScintilla(SCI_POSITIONFROMLINE, (unsigned long)line);
    long end = SendScintilla(SCI_GETLINEENDPOSITION, (unsigned long)line);
    if (end > start)
        SendScintilla(SCI_INDICATORCLEARRANGE, (unsigned long)start, end - start);
}

void SonicPiScintilla::hideLineNumbers()
{
    mutex->lock();
    setMarginLineNumbers(0, false);
    setMarginWidth(0, "0");
    updateErrorMarginWidth();
    SendScintilla(SCI_HIDELINES);
    mutex->unlock();
}

void SonicPiScintilla::showLineNumbers()
{
    mutex->lock();
    setMarginLineNumbers(0, true);
    setMarginWidth(0, "1000");
    updateErrorMarginWidth();
    SendScintilla(SCI_SHOWLINES);
    mutex->unlock();
}

void SonicPiScintilla::addOtherKeyBinding(QSettings& qs, int cmd, int key)
{
    mutex->lock();
    QString skey;
    QTextStream(&skey) << "/Scintilla/keymap/c" << cmd << "/alt";
    qs.setValue(skey, key);
    mutex->unlock();
}

void SonicPiScintilla::addKeyBinding(QSettings& qs, int cmd, int key)
{
    mutex->lock();
    QString skey;
    QTextStream(&skey) << "/Scintilla/keymap/c" << cmd << "/key";
    qs.setValue(skey, key);
    mutex->unlock();
}

void SonicPiScintilla::cutLineFromPoint()
{
    mutex->lock();
    int linenum, index;
    getCursorPosition(&linenum, &index);

    if (text(linenum).mid(index).contains(QRegularExpression("^\\s*\\n")))
    {
        setSelection(linenum, index, linenum + 1, 0);
        SendScintilla(SCI_CUT);
    }
    else
    {
        //  SendScintilla(SCI_CLEARSELECTIONS);
        int pos = SendScintilla(SCI_GETCURRENTPOS);

        SendScintilla(SCI_LINEEND);
        SendScintilla(SCI_SETANCHOR, pos);
        SendScintilla(SCI_CUT);
    }
    mutex->unlock();
}

void SonicPiScintilla::tabCompleteifList()
{
    mutex->lock();
    if (isListActive())
    {
        SendScintilla(QsciCommand::Tab);
    }
    mutex->unlock();
}

void SonicPiScintilla::transposeChars()
{
    mutex->lock();
    int linenum, index;
    getCursorPosition(&linenum, &index);
    setSelection(linenum, 0, linenum + 1, 0);
    int lineLength = selectedText().size();

    // transpose chars
    if (index > 0)
    {
        if (index < (lineLength - 1))
        {
            index = index + 1;
        }
        setSelection(linenum, index - 2, linenum, index);
        QString text = selectedText();
        QChar a, b;
        a = text.at(0);
        b = text.at(1);
        QString replacement = "";
        replacement.append(b);
        replacement.append(a);
        replaceSelectedText(replacement);
    }

    setCursorPosition(linenum, index);
    mutex->unlock();
}

void SonicPiScintilla::setMark()
{
    mutex->lock();
    int pos = SendScintilla(SCI_GETCURRENTPOS);
    SendScintilla(SCI_SETEMPTYSELECTION, pos);
    SendScintilla(SCI_SETSELECTIONMODE, 0);
    this->selectionMode = true;
    mutex->unlock();
}

void SonicPiScintilla::escapeAndCancelSelection()
{
    // Escape (a QShortcut, so it never reaches event()) first dismisses the
    // completion popup if it's open, reverting the preview: a slider goes back to
    // its original value, a list back to the text typed before the popup opened.
    if (m_completion && m_completion->isShowing())
    {
        if (m_pvSlider) restoreOriginal();
        else clearPreview();
        endPreview();
        m_completion->hidePopup();
        return;
    }
    mutex->lock();
    int pos = SendScintilla(SCI_GETCURRENTPOS);
    SendScintilla(SCI_SETEMPTYSELECTION, pos);
    SendScintilla(SCI_CANCEL);
    this->selectionMode = false;
    mutex->unlock();
}

void SonicPiScintilla::deselect()
{
    mutex->lock();
    int pos = SendScintilla(SCI_GETCURRENTPOS);
    SendScintilla(SCI_SETEMPTYSELECTION, pos);
    this->selectionMode = false;
    mutex->unlock();
}

void SonicPiScintilla::copyClear()
{
    mutex->lock();
    QsciScintilla::copy();
    deselect();
    mutex->unlock();
}

void SonicPiScintilla::replaceLine(int lineNumber, QString newLine)
{
    mutex->lock();
    setSelection(lineNumber, 0, lineNumber + 1, 0);
    replaceSelectedText(newLine);
    mutex->unlock();
}

void SonicPiScintilla::replaceLines(int lineStart, int lineFinish, QString newLines)
{
    mutex->lock();
    setSelection(lineStart, 0, lineFinish + 1, 0);
    replaceSelectedText(newLines);
    mutex->unlock();
}

void SonicPiScintilla::forwardLines(int numLines)
{
    mutex->lock();
    int idx;
    if (numLines > 0)
    {
        for (idx = 0; idx < numLines; idx++)
        {
            if (selectionMode)
            {
                SendScintilla(SCI_LINEDOWNEXTEND);
            }
            else
            {
                SendScintilla(SCI_LINEDOWN);
            }
        }
    }
    else
    {
        for (idx = 0; idx > numLines; idx--)
        {
            if (selectionMode)
            {
                SendScintilla(SCI_LINEUPEXTEND);
            }
            else
            {
                SendScintilla(SCI_LINEUP);
            }
        }
    }
    mutex->unlock();
}

void SonicPiScintilla::forwardOneLine()
{
    // While the completion popup is open, the "move down" shortcut (whatever it
    // is bound to) moves the highlight instead of the caret.
    if (m_completion && m_completion->isShowing()) { m_completion->moveSelection(+1); return; }
    forwardLines(1);
}

void SonicPiScintilla::backOneLine()
{
    if (m_completion && m_completion->isShowing()) { m_completion->moveSelection(-1); return; }
    forwardLines(-1);
}

void SonicPiScintilla::forwardTenLines()
{
    if (m_completion && m_completion->isShowing()) { m_completion->moveSelection(+10); return; }
    mutex->lock();
    forwardLines(10);
    mutex->unlock();
}

void SonicPiScintilla::backTenLines()
{
    if (m_completion && m_completion->isShowing()) { m_completion->moveSelection(-10); return; }
    mutex->lock();
    forwardLines(-10);
    mutex->unlock();
}

void SonicPiScintilla::moveLineOrSelectionUp()
{
    mutex->lock();
    moveLineOrSelection(-1);
    mutex->unlock();
}

void SonicPiScintilla::moveLineOrSelectionDown()
{
    mutex->lock();
    moveLineOrSelection(1);
    mutex->unlock();
}

void SonicPiScintilla::moveLineOrSelection(int numLines)
{
    mutex->lock();
    beginUndoAction();

    int linenum, cursor, origLinenum, origCursor;
    getCursorPosition(&linenum, &cursor);
    origLinenum = linenum;
    origCursor = cursor;

    bool hadSelectedText = hasSelectedText();

    if (!hadSelectedText)
    {
        setSelection(linenum, 0, linenum + 1, 0);
    }

    int lineFrom, indexFrom, lineTo, indexTo, lineOffset;
    getSelection(&lineFrom, &indexFrom, &lineTo, &indexTo);
    lineOffset = lineTo - origLinenum;
    linenum = lineFrom;

    QString selection = selectedText();

    if (selection[selection.length() - 1] != '\n')
    {
        selection = selection + "\n";
        lineTo += 1;
        lineOffset += 1;
        indexTo = 0;
        replaceSelectedText("");
        setCursorPosition(linenum, 0);
        SendScintilla(SCI_DELETEBACK);
    }
    else
    {
        replaceSelectedText("");
    }
    setCursorPosition(linenum, 0);

    moveLines(numLines);

    getCursorPosition(&linenum, &cursor);
    setCursorPosition(linenum, 0);
    insert(selection);

    setCursorPosition(linenum + lineOffset, origCursor);

    int diffLine = lineTo - lineFrom;
    int diffIndex = indexTo - indexFrom;

    setSelection(linenum + diffLine, diffIndex, linenum, 0);

    endUndoAction();
    mutex->unlock();
}

QStringList SonicPiScintilla::apiContext(int pos, int& context_start,
    int& last_word_start)
{
    // sampl|  /  sample |  /  chord :E3,|
    int linenum, cursor;
    getCursorPosition(&linenum, &cursor);

    context_start = 0;
    last_word_start = pos;

    // The token reduction is a pure function (utils/completion_context) so the
    // completion detection it drives can be tested from hardcoded text + cursor.
    return SonicPi::lineToContext(text(linenum), cursor);
}

int SonicPiScintilla::tokenEndForCaret(int pos)
{
    const int len = SendScintilla(SCI_GETLENGTH);
    int end = pos;
    while (end < len)
    {
        const char c = (char)SendScintilla(SCI_GETCHARAT, end);
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == ',' ||
            c == '(' || c == ')' || c == '{' || c == '}' ||
            c == '[' || c == ']' || c == '"' || c == '\'' || c == '#')
            break;
        ++end;
    }
    return end;
}

int SonicPiScintilla::incLineNumWithinBounds(int linenum, int inc)
{
    mutex->lock();
    linenum += inc;
    int maxBufferIndex = lines() - 1;

    if (linenum < 0)
    {
        linenum = 0;
    }

    if (linenum > maxBufferIndex)
    {
        linenum = maxBufferIndex;
    }

    return linenum;
    mutex->unlock();
}

void SonicPiScintilla::moveLines(int numLines)
{
    mutex->lock();
    if (numLines > 0)
    {
        for (int i = 0; i < numLines; i++)
        {
            SendScintilla(SCI_LINEDOWN);
        }
    }
    else
    {
        for (int i = 0; i > numLines; i--)
        {
            SendScintilla(SCI_LINEUP);
        }
    }
    mutex->unlock();
}

void SonicPiScintilla::upcaseWordOrSelection()
{
    mutex->lock();
    if (hasSelectedText())
    {
        SendScintilla(SCI_UPPERCASE);
    }
    else
    {
        setMark();
        SendScintilla(SCI_WORDRIGHT);
        SendScintilla(SCI_UPPERCASE);
        deselect();
    }
    mutex->unlock();
}

void SonicPiScintilla::downcaseWordOrSelection()
{
    mutex->lock();
    if (hasSelectedText())
    {
        SendScintilla(SCI_LOWERCASE);
    }
    else
    {
        setMark();
        SendScintilla(SCI_WORDRIGHT);
        SendScintilla(SCI_LOWERCASE);
        deselect();
    }
    mutex->unlock();
}

void SonicPiScintilla::setLineErrorMarker(int lineNumber, bool isSyntaxError, const QString& errorToken, int colStart, int colEnd)
{
    mutex->lock();
    m_errorLine = lineNumber;
    m_errorIsSyntax = isSyntaxError;
    m_errorToken = errorToken;
    m_errorColStart = colStart;
    m_errorColEnd = colEnd;
    applyErrorMarkers(lineNumber);

    // Perhaps consider a more manual way of returning this functionality:
    // int currlinenum, index;
    // getCursorPosition(&currlinenum, &index);
    // if (lineNumber != currlinenum) {
    //   setCursorPosition(lineNumber, 0);
    // }

    mutex->unlock();
}

// Rebuild the error line's markers at the current zoom, so the image-based
// margin washes and dot track the line height like the dynamic code wash does.
void SonicPiScintilla::refreshErrorMarkers()
{
    mutex->lock();
    if (m_errorLine >= 0)
        applyErrorMarkers(m_errorLine);
    mutex->unlock();
}

void SonicPiScintilla::applyErrorMarkers(int lineNumber)
{
    markerDeleteAll(-1);

    // Runtime errors are pink, syntax errors blue; the dot, washes and underline
    // all take this colour. Marker 9 is the translucent line wash — its alpha
    // comes from the colour (setMarkerBackgroundColor also sets the marker alpha),
    // so pass a 40-alpha colour, not the opaque one.
    QColor errCol = theme->color(m_errorIsSyntax ? "MarkerBackgroundSyntax" : "MarkerBackground");
    QColor errWash = errCol;
    errWash.setAlpha(40);
    setMarkerBackgroundColor(errWash, 9);

    int errH = SendScintilla(SCI_TEXTHEIGHT, (unsigned long)0);
    // The arrowhead fills the symbol-margin gap between the number and the code.
    // The gap is kept at this same char-proportional width in every state, so
    // showing or dismissing an error never shifts the code horizontally.
    int gapW = updateErrorMarginWidth();

    qreal errDpr = devicePixelRatioF();
    if (errDpr < 1.0) errDpr = 1.0;

    // symbol margin: left at its default background; only the coloured dot sits
    // there (no solid fill).

    // number margin (10): solid fill + black right-justified line number, drawn
    // ourselves since the opaque fill covers Scintilla's own (grey) number.
    int numW = SendScintilla(SCI_GETMARGINWIDTHN, (unsigned long)0);
    if (numW > 0 && errH > 0) {
        QImage img(qRound(numW * errDpr), qRound(errH * errDpr), QImage::Format_ARGB32);
        img.setDevicePixelRatio(errDpr);
        img.fill(errCol);
        {
            QPainter p(&img);
            p.setRenderHint(QPainter::TextAntialiasing, true);
            int zoom = SendScintilla(SCI_GETZOOM);
            QFont f("Hack", qMax(1, 15 + zoom), -1, true);
            p.setFont(f);
            p.setPen(Qt::black);
            p.drawText(QRect(0, 0, numW - 3, errH), Qt::AlignRight | Qt::AlignVCenter,
                       QString::number(lineNumber + 1));
        }
        SendScintilla(SCI_RGBAIMAGESETSCALE, (unsigned long)qRound(errDpr * 100));
        markerDefine(img, 10);
    }

    // gutter marker: full-line-height arrowhead pointing right toward the code
    // (error colour), sized to the one-character gap so it scales with the font.
    if (errH >= 8) {
        int hPx = qRound(errH * errDpr);
        int wPx = qRound(gapW * errDpr);
        QImage dot(wPx, hPx, QImage::Format_ARGB32);
        dot.fill(Qt::transparent);
        {
            QPainter dp(&dot);
            dp.setRenderHint(QPainter::Antialiasing, true);
            dp.setPen(Qt::NoPen);
            dp.setBrush(errCol);
            // Inset the base a little so there's a small gap between the triangle
            // and the highlighted number margin to its left.
            int leftPx = qRound((gapW / 6.0) * errDpr);
            QPolygonF tri;
            tri << QPointF(leftPx, 0) << QPointF(wPx, hPx / 2.0) << QPointF(leftPx, hPx);
            dp.drawPolygon(tri);
        }
        dot.setDevicePixelRatio(errDpr);
        SendScintilla(SCI_RGBAIMAGESETSCALE, (unsigned long)qRound(errDpr * 100));
        markerDefine(dot, 8);
    }

    // Dashed underline in the error colour. Underline just the offending
    // identifier when the exception named one (found whole-word in the line's
    // byte range so multi-byte characters don't shift it); otherwise underline
    // the whole line's code, skipping the leading indentation.
    int lineStart = SendScintilla(SCI_POSITIONFROMLINE, (unsigned long)lineNumber);
    int lineEnd = SendScintilla(SCI_GETLINEENDPOSITION, (unsigned long)lineNumber);
    int errFrom = -1;
    int errLen = 0;
    // 1. Exact byte-column span from error_highlight (clamped to the line).
    if (m_errorColStart >= 0 && m_errorColEnd > m_errorColStart) {
        int from = lineStart + m_errorColStart;
        int to = lineStart + m_errorColEnd;
        if (to > lineEnd) to = lineEnd;
        if (from < lineEnd) {
            errFrom = from;
            errLen = to - from;
        }
    }
    // 2. Fall back to the identifier named in the message (whole-word search).
    if (errFrom < 0 && !m_errorToken.isEmpty()) {
        QByteArray tok = m_errorToken.toUtf8();
        SendScintilla(SCI_SETSEARCHFLAGS, (unsigned long)SCFIND_WHOLEWORD);
        SendScintilla(SCI_SETTARGETSTART, (unsigned long)lineStart);
        SendScintilla(SCI_SETTARGETEND, (unsigned long)lineEnd);
        int found = SendScintilla(SCI_SEARCHINTARGET, static_cast<uintptr_t>(tok.length()), tok.constData());
        if (found >= 0) {
            errFrom = found;
            errLen = tok.length();
        }
    }
    // 3. Otherwise underline the whole line's code.
    if (errFrom < 0) {
        errFrom = SendScintilla(SCI_GETLINEINDENTPOSITION, (unsigned long)lineNumber);
        errLen = lineEnd - errFrom;
    }
    SendScintilla(SCI_INDICSETFORE, (unsigned long)kErrorIndicator,
                  (long)((errCol.blue() << 16) | (errCol.green() << 8) | errCol.red()));
    SendScintilla(SCI_SETINDICATORCURRENT, (unsigned long)kErrorIndicator);
    SendScintilla(SCI_INDICATORCLEARRANGE, (unsigned long)0, (long)SendScintilla(SCI_GETLENGTH));
    if (errLen > 0)
        SendScintilla(SCI_INDICATORFILLRANGE, (unsigned long)errFrom, (long)errLen);

    markerAdd(lineNumber, 8);
    markerAdd(lineNumber, 9);
    markerAdd(lineNumber, 10);
}

void SonicPiScintilla::clearLineMarkers()
{
    mutex->lock();
    m_errorLine = -1;
    markerDeleteAll(-1);
    SendScintilla(SCI_SETINDICATORCURRENT, (unsigned long)kErrorIndicator);
    SendScintilla(SCI_INDICATORCLEARRANGE, (unsigned long)0, (long)SendScintilla(SCI_GETLENGTH));
    mutex->unlock();
}

int SonicPiScintilla::updateErrorMarginWidth()
{
    int gapW = (SendScintilla(SCI_TEXTWIDTH, static_cast<uintptr_t>(STYLE_DEFAULT), "0") * 13) / 10;
    if (gapW < 4)
        gapW = (SendScintilla(SCI_TEXTHEIGHT, (unsigned long)0) * 4) / 5;
    setMarginWidth(1, gapW);
    // The flash gutter dot is an image sized to this gap, so rebuild it here
    // (covers zoom changes and the initial sizing).
    applyFlashMarkerColours();
    return gapW;
}

void SonicPiScintilla::zoomFontIn()
{
    mutex->lock();
    int zoom = property("zoom").toInt();
    zoom++;
    if (zoom > 20)
        zoom = 20;
    setProperty("zoom", QVariant(zoom));
    zoomTo(zoom);
    updateErrorMarginWidth();
    mutex->unlock();
    refreshErrorMarkers();
    emit zoomLevelChanged();
}

void SonicPiScintilla::zoomFontOut()
{
    mutex->lock();
    int zoom = property("zoom").toInt();
    zoom--;
    if (zoom < -10)
        zoom = -10;
    setProperty("zoom", QVariant(zoom));
    zoomTo(zoom);
    updateErrorMarginWidth();
    mutex->unlock();
    refreshErrorMarkers();
    emit zoomLevelChanged();
}

int SonicPiScintilla::currentZoom()
{
    return (int)SendScintilla(SCI_GETZOOM);
}

void SonicPiScintilla::wheelEvent(QWheelEvent* event)
{
    QsciScintilla::wheelEvent(event);
    // Ctrl+wheel zooms the code; keep the gap and error markers at the new line height.
    if (event->modifiers() & Qt::ControlModifier)
    {
        mutex->lock();
        updateErrorMarginWidth();
        mutex->unlock();
        refreshErrorMarkers();
        emit zoomLevelChanged();
    }
}

void SonicPiScintilla::newLine()
{
    mutex->lock();
    SendScintilla(QsciCommand::Newline);
    mutex->unlock();
}

void SonicPiScintilla::replaceBuffer(QString content, int line, int index, int first_line)
{
    mutex->lock();
    // A Return with auto-indent, and beautify-on-run, both arrive here as a
    // whole-buffer replace. Capture where the flash anchors sit (line +
    // de-indented text) so we can remap them across the replace afterwards.
    const bool remap = !m_runLinePos.isEmpty();
    QVector<int> anchorLines;
    QStringList oldStripped;
    if (remap)
    {
        for (int p : m_runLinePos)
            anchorLines.append((int)SendScintilla(SCI_LINEFROMPOSITION, (unsigned long)p));
        for (int i = 0, n = lines(); i < n; i++)
            oldStripped.append(text(i).trimmed());
    }

    m_inReplaceBuffer = true;
    beginUndoAction();
    insert(" ");
    SendScintilla(QsciCommand::Delete);
    selectAll();
    replaceSelectedText(content);
    setCursorPosition(line, index);
    setFirstVisibleLine(first_line);
    endUndoAction();
    m_inReplaceBuffer = false;

    if (remap)
    {
        QStringList newStripped;
        for (int i = 0, n = lines(); i < n; i++)
            newStripped.append(text(i).trimmed());
        remapFlashAnchorsAcrossReplace(anchorLines, oldStripped, newStripped);
    }
    mutex->unlock();
}

void SonicPiScintilla::completeListOrNewlineAndIndent()
{
    // Return accepts the popup's highlight. A slider's value is already typed
    // into the buffer, so there Return closes the popup and newlines in one
    // press.
    if (m_completion && m_completion->isShowing())
    {
        if (!m_completion->isSliderMode())
        {
            acceptCompletion();
            return;
        }
        endPreview();
        m_completion->hidePopup();
        // fall through to newline + indent
    }
    mutex->lock();
    if (isListActive())
    {
        tabCompleteifList();
    }
    else
    {
        if (autoIndent)
        {
            newlineAndIndent();
        }
        else
        {
            newLine();
        }
    }
    mutex->unlock();
}

void SonicPiScintilla::newlineAndIndent()
{
    mutex->lock();
    int point_line, point_index, first_line;
    getCursorPosition(&point_line, &point_index);
    first_line = firstVisibleLine();

    std::string code = text().toStdString();

    emit bufferNewlineAndIndent(point_line, point_index, first_line, code, fileName.toStdString());
    mutex->unlock();
}

// Indicator recolouring the drop preview grey, so it reads as provisional
// until the drop commits (indicators can't italicise, only recolour).
static const int kDropPreviewIndicator = 22;

namespace
{
// One rounded accent border around the whole previewed block: a card-shaped
// outline Scintilla's per-line-run indicators can't draw.
class DropPreviewBox : public QWidget
{
public:
    QColor colour;
    QColor titleBg;  // editor background, to break the border behind the title
    QString title;   // card title, drawn straddling the top border
    QFont titleFont;

    explicit DropPreviewBox(QWidget* parent)
        : QWidget(parent)
    {
        setAttribute(Qt::WA_TransparentForMouseEvents, true);
        setAttribute(Qt::WA_NoSystemBackground, true);
        setAttribute(Qt::WA_TranslucentBackground, true);
        hide();
    }

    // Reserve a full title height above the code so the legend straddles the
    // top border clear of the first code line.
    int titleReserve() const
    {
        return title.isEmpty() ? 0 : QFontMetrics(titleFont).height();
    }

    // Minimum box width so the whole title fits (drawn at kTitleX with a small
    // background pad each side and a right margin).
    int minWidthForTitle() const
    {
        if (title.isEmpty())
            return 0;
        return 12 + QFontMetrics(titleFont).horizontalAdvance(title) + 6 + 8;
    }

protected:
    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing);
        const int titleH = title.isEmpty() ? 0 : QFontMetrics(titleFont).height();
        const qreal borderTop = titleH / 2.0;
        QColor fill = colour;
        fill.setAlpha(14);
        p.setPen(QPen(colour, 2.0));
        p.setBrush(fill);
        p.drawRoundedRect(QRectF(rect()).adjusted(1, borderTop + 1, -1, -1), 6, 6);
        if (title.isEmpty())
            return;
        // Title straddling the top border, legend style: clear the border
        // behind the text, then draw it in the accent colour.
        p.setFont(titleFont);
        const int tw = QFontMetrics(titleFont).horizontalAdvance(title);
        const int tx = 12;
        const int pad = 6;
        p.setPen(Qt::NoPen);
        p.setBrush(titleBg);
        p.drawRect(QRectF(tx - pad, 0, tw + 2 * pad, titleH));
        p.setPen(colour);
        p.drawText(QRectF(tx, 0, tw, titleH), Qt::AlignVCenter | Qt::AlignLeft, title);
    }
};
} // namespace

void SonicPiScintilla::setPlaceholderText(const QString& text)
{
    m_placeholderText = text;
    if (!m_placeholder)
    {
        m_placeholder = new QLabel(viewport());
        m_placeholder->setAttribute(Qt::WA_TransparentForMouseEvents, true);
        m_placeholder->setTextFormat(Qt::PlainText);
    }
    m_placeholder->setText(text);
    // Re-evaluate whenever the buffer changes or the editor is zoomed.
    connect(this, &QsciScintilla::textChanged, this, &SonicPiScintilla::updatePlaceholder,
            Qt::UniqueConnection);
    connect(this, &SonicPiScintilla::zoomLevelChanged, this, &SonicPiScintilla::updatePlaceholder,
            Qt::UniqueConnection);
    updatePlaceholder();
}

void SonicPiScintilla::updatePlaceholder()
{
    if (!m_placeholder)
        return;
    // SCI_GETLENGTH, not text().isEmpty(): the latter copies the whole document
    // out of Scintilla on every textChanged just to test emptiness.
    const bool show = SendScintilla(SCI_GETLENGTH) == 0 && !m_placeholderText.isEmpty();
    m_placeholder->setVisible(show);
    if (!show)
        return;
    // Same font as the line-number margin (Hack, 15 + the live zoom, italic).
    // The size has to go in the stylesheet: the app-wide QLabel font-size rule
    // overrides setFont on styled labels. setFont still runs so adjustSize
    // measures the right font.
    const unsigned long commentStyle = QsciLexerRuby::Comment;
    const int ptSize = qMax(6, 15 + (int)SendScintilla(SCI_GETZOOM));
    m_placeholder->setFont(QFont("Hack", ptSize, -1, true));
    // 38% opacity so it reads as a placeholder hint rather than real code.
    const long fore = SendScintilla(SCI_STYLEGETFORE, commentStyle);
    const QColor c((int)(fore & 0xFF), (int)((fore >> 8) & 0xFF), (int)((fore >> 16) & 0xFF));
    m_placeholder->setStyleSheet(
        QString("color: rgba(%1,%2,%3,0.38); background: transparent;"
                " font-family: 'Hack'; font-size: %4pt; font-style: italic;")
            .arg(c.red())
            .arg(c.green())
            .arg(c.blue())
            .arg(ptSize));
    m_placeholder->adjustSize();
    const int x = (int)SendScintilla(SCI_POINTXFROMPOSITION, (unsigned long)0, (long)0);
    const int y = (int)SendScintilla(SCI_POINTYFROMPOSITION, (unsigned long)0, (long)0);
    m_placeholder->move(x, y);
}

void SonicPiScintilla::placeDropPreview(int bytePos, const QString& text, const QString& title)
{
    // Pointer sitting over the preview itself: already in place. Same payload
    // only; a different card's code replaces it.
    if (m_dropPreviewPos >= 0 && text == m_dropPreviewText && bytePos >= m_dropPreviewPos
        && bytePos <= m_dropPreviewPos + m_dropPreviewLen)
        return;
    SendScintilla(SCI_SETUNDOCOLLECTION, (long)0);
    if (m_dropPreviewPos >= 0)
    {
        if (bytePos > m_dropPreviewPos)
            bytePos = qMax(m_dropPreviewPos, bytePos - m_dropPreviewLen);
        SendScintilla(SCI_DELETERANGE, (unsigned long)m_dropPreviewPos, (long)m_dropPreviewLen);
    }
    const QByteArray utf8 = text.toUtf8();
    // uintptr_t selects the (uintptr_t, const char*) overload unambiguously
    // (unsigned long is 32-bit on MSVC, leaving the call ambiguous there).
    SendScintilla(SCI_INSERTTEXT, (uintptr_t)bytePos, utf8.constData());
    SendScintilla(SCI_SETUNDOCOLLECTION, (long)1);
    m_dropPreviewPos = bytePos;
    m_dropPreviewLen = utf8.length();
    m_dropPreviewText = text;

    SendScintilla(SCI_INDICSETSTYLE, (unsigned long)kDropPreviewIndicator, (long)INDIC_TEXTFORE);
    SendScintilla(SCI_INDICSETFORE, (unsigned long)kDropPreviewIndicator, (long)0x909090);
    SendScintilla(SCI_SETINDICATORCURRENT, (unsigned long)kDropPreviewIndicator);
    SendScintilla(SCI_INDICATORFILLRANGE, (unsigned long)m_dropPreviewPos, (long)m_dropPreviewLen);

    // Card-shaped border around the block's content lines (the payload's
    // leading/trailing blank lines stay outside the box).
    if (!m_dropPreviewBox)
        m_dropPreviewBox = new DropPreviewBox(viewport());
    DropPreviewBox* box = static_cast<DropPreviewBox*>(m_dropPreviewBox);
    box->colour = theme->color("HighlightedBackground");
    box->title = title;
    box->titleBg = theme->color("Background");
    QFont titleFont = font();
    titleFont.setBold(true);
    box->titleFont = titleFont;
    const long firstLine = SendScintilla(SCI_LINEFROMPOSITION, (unsigned long)m_dropPreviewPos) + 1;
    const long endLine = SendScintilla(SCI_LINEFROMPOSITION,
                                       (unsigned long)(m_dropPreviewPos + m_dropPreviewLen - 1));
    const long lastLine = qMax(firstLine, endLine - 1);
    const long topPos = SendScintilla(SCI_POSITIONFROMLINE, (unsigned long)firstLine);
    const int top = (int)SendScintilla(SCI_POINTYFROMPOSITION, (unsigned long)0, topPos);
    const int bottom = (int)SendScintilla(SCI_POINTYFROMPOSITION, (unsigned long)0,
                                          SendScintilla(SCI_POSITIONFROMLINE, (unsigned long)lastLine))
                       + (int)SendScintilla(SCI_TEXTHEIGHT, (unsigned long)lastLine);
    const int left = (int)SendScintilla(SCI_POINTXFROMPOSITION, (unsigned long)0, topPos);
    int right = left;
    for (long ln = firstLine; ln <= lastLine; ++ln)
        right = qMax(right, (int)SendScintilla(SCI_POINTXFROMPOSITION, (unsigned long)0,
                                               SendScintilla(SCI_GETLINEENDPOSITION,
                                                             (unsigned long)ln)));
    const int padX = 8;
    const int padY = 4;
    const int titleTop = box->titleReserve(); // room above the border for the title
    // Stretch the box so the full card name is visible even when it is wider
    // than the code lines it wraps.
    const int boxW = qMax((right - left) + 2 * padX, box->minWidthForTitle());
    box->setGeometry(left - padX, top - padY - titleTop, boxW,
                     (bottom - top) + 2 * padY + titleTop);
    box->show();
    box->raise();
    box->update();
}

void SonicPiScintilla::clearDropPreview()
{
    if (m_dropPreviewPos < 0)
        return;
    SendScintilla(SCI_SETUNDOCOLLECTION, (long)0);
    SendScintilla(SCI_DELETERANGE, (unsigned long)m_dropPreviewPos, (long)m_dropPreviewLen);
    SendScintilla(SCI_SETUNDOCOLLECTION, (long)1);
    m_dropPreviewPos = -1;
    m_dropPreviewLen = 0;
    if (m_dropPreviewBox)
        m_dropPreviewBox->hide();
}

// Byte position of the start of the line under the pointer: card code is
// line-based, so previews and drops always land on line boundaries.
static long dropLineStart(QsciScintillaBase* sci, const QPoint& pos)
{
    const long sciPos = sci->SendScintilla(QsciScintillaBase::SCI_POSITIONFROMPOINT,
                                           (unsigned long)pos.x(), (long)pos.y());
    const long line = sci->SendScintilla(QsciScintillaBase::SCI_LINEFROMPOSITION, sciPos);
    return sci->SendScintilla(QsciScintillaBase::SCI_POSITIONFROMLINE, line);
}

// Card drags carry this mime tag, and their lifecycle (dragEnded /
// insertPreviewCleared) cleans the preview up. Other text drags (other apps,
// the editor's own selection) never get preview text written into the buffer;
// nothing would clean it up.
static const char* kCardMime = "application/x-sonic-pi-card-title";

void SonicPiScintilla::dragEnterEvent(QDragEnterEvent* event)
{
    mutex->lock();
    if (event->mimeData()->hasFormat("text/uri-list"))
    {
        event->acceptProposedAction();
    }
    else if (event->mimeData()->hasFormat(kCardMime))
    {
        event->acceptProposedAction();
        placeDropPreview((int)dropLineStart(this, event->position().toPoint()),
                         event->mimeData()->text(),
                         QString::fromUtf8(event->mimeData()->data(kCardMime)));
    }
    mutex->unlock();
}

void SonicPiScintilla::dragMoveEvent(QDragMoveEvent* event)
{
    mutex->lock();
    if (event->mimeData()->hasFormat("text/uri-list"))
    {
        event->acceptProposedAction();
    }
    else if (event->mimeData()->hasFormat(kCardMime))
    {
        event->acceptProposedAction();
        placeDropPreview((int)dropLineStart(this, event->position().toPoint()),
                         event->mimeData()->text(),
                         QString::fromUtf8(event->mimeData()->data(kCardMime)));
    }
    mutex->unlock();
}

void SonicPiScintilla::dragLeaveEvent(QDragLeaveEvent* event)
{
    // Keep the preview while the pointer is off the editor so dragging back on
    // restores it in place. If the card is released out here, dragEnded cancels
    // it (cancelInsertPreview) rather than committing a fumbled drag.
    QsciScintilla::dragLeaveEvent(event);
}

void SonicPiScintilla::finaliseDropPreview()
{
    mutex->lock();
    if (m_dropPreviewPos < 0)
    {
        mutex->unlock();
        return;
    }
    const int pos = m_dropPreviewPos;
    const QString text = m_dropPreviewText;
    clearDropPreview();
    int line = 0, index = 0;
    lineIndexFromPosition(pos, &line, &index);
    insertAt(text, line, index);
    setCursorPosition(line, index);
    mutex->unlock();
}

void SonicPiScintilla::previewInsertAtCursor(const QString& text, const QString& title)
{
    mutex->lock();
    int line = 0, index = 0;
    getCursorPosition(&line, &index);
    const long bytePos = SendScintilla(SCI_POSITIONFROMLINE, (unsigned long)line);
    placeDropPreview((int)bytePos, text, title);
    mutex->unlock();
}

void SonicPiScintilla::cancelInsertPreview()
{
    mutex->lock();
    clearDropPreview();
    mutex->unlock();
}

void SonicPiScintilla::focusOutEvent(QFocusEvent* e)
{
    // Dismiss the completion popup when the editor loses focus (clicking away,
    // switching apps) — but not when the cursor is over the popup itself (e.g.
    // clicking a piano key), which would cancel the click.
    if (m_completion && m_completion->isShowing() && !m_completion->underMouse())
    {
        clearPreview();   // drop a list preview (keeps a committed slider value)
        endPreview();
        m_completion->hidePopup();
    }
    QsciScintilla::focusOutEvent(e);
}

void SonicPiScintilla::contextMenuEvent(QContextMenuEvent* event)
{
    // Move the caret under the click (unless there's a selection) so word/line
    // actions like "Show Docs for Current Word" act on what was right-clicked.
    if (!hasSelectedText())
    {
        const int pos = (int)SendScintilla(SCI_POSITIONFROMPOINT,
                                           (unsigned long)event->pos().x(),
                                           (long)event->pos().y());
        if (pos >= 0) SendScintilla(SCI_SETEMPTYSELECTION, pos);
    }
    // Standard edit menu (cut/copy/paste/…); MainWindow appends the idiomatic
    // code actions (Show Docs for word, Comment/Uncomment, Align) via the signal.
    QMenu* menu = createStandardContextMenu();
    if (!menu) menu = new QMenu(this);
    emit extendContextMenu(menu);
    menu->exec(event->globalPos());
    delete menu;
}

bool SonicPiScintilla::event(QEvent* evt)
{
    if (evt->type() == QEvent::KeyPress)
    {
        QKeyEvent* key = static_cast<QKeyEvent*>(evt);
        const int k = key->key();

        // Any keystroke dismisses a live hover projection first: its text is
        // undo-invisible, so an edit or a Scintilla-keymap undo landing while
        // it sits in the buffer would apply at shifted positions.
        if (m_dropPreviewPos >= 0)
            clearDropPreview();

        // Raw arrow / page / escape keys (from Scintilla's keymap) drive the
        // popup directly. The configured nav/accept shortcuts (Ctrl+n, Tab,
        // Return, …) are handled by their existing slots, which are popup-aware,
        // so the popup automatically honours whatever the user has bound.
        if (m_completion && m_completion->isShowing())
        {
            switch (k)
            {
            case Qt::Key_Up:       m_completion->moveSelection(-1);  return true;
            case Qt::Key_Down:     m_completion->moveSelection(+1);  return true;
            case Qt::Key_PageUp:   m_completion->moveSelection(-10); return true;
            case Qt::Key_PageDown: m_completion->moveSelection(+10); return true;
            // Left/Right move the caret (a slider still steps via Up/Down or
            // the mouse); the post-key block below re-derives the popup
            // context after the move.
            case Qt::Key_Space:
                // Space commits a previewed entry, then types the space; a plain
                // space otherwise (no preview shown yet).
                if (m_pvLive)
                {
                    SendScintilla(SCI_BEGINUNDOACTION);
                    acceptCompletion();
                    replaceSelectedText(" ");
                    SendScintilla(SCI_ENDUNDOACTION);
                    return true;
                }
                break;
            case Qt::Key_Escape:
                if (m_pvSlider) restoreOriginal(); else clearPreview();
                endPreview(); m_completion->hidePopup();             return true;
            default: break;
            }
        }

        // Default Return: accept the popup (handled inside) or newline+indent.
        if (k == Qt::Key_Return || k == Qt::Key_Enter)
        {
            completeListOrNewlineAndIndent();
            return true;
        }

        // Coalesce preview-restore + the edit + re-preview into one undo step, so
        // a single Cmd-Z removes the whole keystroke (not each preview edit).
        SendScintilla(SCI_BEGINUNDOACTION);
        // Drop the preview first so the keystroke edits the user's typed text (or,
        // for a slider, leaves its committed value), then let the editor process it.
        if (m_pvStart >= 0) clearPreview();

        // Let the editor insert/delete the character, then refresh the popup. This
        // path runs after the edit fully settles (cursor advanced), so the context
        // is correct — unlike textChanged, which fires mid-edit before the caret
        // moves (so e.g. `play ` would still read as `play`).
        bool res = QsciScintilla::event(evt);
        const QString t = key->text();
        const bool printable = !t.isEmpty() && t[0].isPrint();
        if (printable || k == Qt::Key_Backspace)
        {
            updateCompletion();
        }
        else if (m_completion && m_completion->isShowing())
        {
            // A bare modifier press (Ctrl/Shift/Alt/Meta) is the start of a chord
            // such as C-n — keep the popup open so the chord's shortcut can drive
            // it. Only genuinely dismiss on other non-text keys.
            switch (k)
            {
            case Qt::Key_Control:
            case Qt::Key_Shift:
            case Qt::Key_Alt:
            case Qt::Key_Meta:
            case Qt::Key_AltGr:
            case Qt::Key_CapsLock:
                break;
            case Qt::Key_Left:
            case Qt::Key_Right:
                // The caret moved: re-derive the context at its new position.
                // The popup follows while the caret stays within the completable
                // area and dismisses once it walks out of it.
                updateCompletion();
                break;
            default:
                endPreview();
                m_completion->hidePopup();
                break;
            }
        }
        SendScintilla(SCI_ENDUNDOACTION);
        return res;
    }

    return QsciScintilla::event(evt);
}

// Append names the user has defined in this buffer (define/live_loop/cue/set) to
// the completion list — the static API can't know about them. `symbol` prefixes a
// ':' (for sync/cue value positions). Dedups against the existing items.
static void addBufferDefs(QList<CompletionItem>& items, const QString& buffer,
                          const QRegularExpression& re, bool symbol)
{
    if (items.isEmpty()) return;
    const QString kind = items.first().kind;
    QSet<QString> have;
    for (const CompletionItem& it : items) have.insert(it.text);
    QRegularExpressionMatchIterator mi = re.globalMatch(buffer);
    while (mi.hasNext())
    {
        const QString name = mi.next().captured(1);
        if (name.isEmpty()) continue;
        const QString t = symbol ? (QStringLiteral(":") + name) : name;
        if (have.contains(t)) continue;
        have.insert(t);
        CompletionItem it;
        it.text = t;
        it.kind = kind;
        it.summary = QObject::tr("defined in this buffer");
        items.append(it);
    }
}

void SonicPiScintilla::triggerCompletion()
{
    // Explicit invocation (menu/shortcut): show suggestions at the caret even when
    // automatic completion is turned off, so it can be used purely on demand.
    updateCompletion(true);
}

void SonicPiScintilla::showCompletionDocs()
{
    // Surface the autocomplete docs for the current context: always (re)build the
    // popup for the CURRENT cursor position — like the trigger shortcut — so it
    // swaps when the cursor has moved to a different context, not just when hidden.
    // Then read the highlighted item's docstring for a screen reader.
    triggerCompletion();
    announceCompletionDetails();
}

void SonicPiScintilla::announceCompletionDetails()
{
    // Speak the highlighted item's full docstring on demand — the docs pane is
    // pruned from the accessibility tree, so this is how a screen reader hears it.
    if (!m_completion || !m_completion->isShowing()) return;
    const QString doc = m_completion->currentDoc();
    if (!doc.isEmpty()) emit announceRequested(doc);
}

void SonicPiScintilla::updateCompletion(bool force)
{
    if (!m_completion) return;
    if (!m_completionEnabled && !force)
    {
        clearPreview(); endPreview();
        m_completion->hidePopup();
        return;
    }

    // Work against the user's typed text / current value, not a stale preview.
    clearPreview();

    // Don't pop up inside a comment or a string literal: scan the line up to the
    // caret tracking quote state; bail on an unquoted '#' (comment) or if the
    // caret sits inside an unterminated string.
    {
        int gl, gc;
        getCursorPosition(&gl, &gc);
        const QString upto = text(gl).left(gc);
        QChar q;
        bool suppress = false;
        for (int i = 0; i < upto.length(); ++i)
        {
            const QChar c = upto[i];
            if (!q.isNull())
            {
                if (c == '\\') { ++i; continue; }   // skip escaped char in string
                if (c == q) q = QChar();
                continue;
            }
            if (c == '"' || c == '\'') { q = c; continue; }
            if (c == '#') { suppress = true; break; }   // comment to end of line
        }
        if (suppress || !q.isNull())
        {
            endPreview();
            m_completion->hidePopup();
            return;
        }
    }

    int pos = SendScintilla(SCI_GETCURRENTPOS);
    int context_start, last_word_start;
    QStringList context = apiContext(pos, context_start, last_word_start);
    QString partial = context.isEmpty() ? QString() : context.last();

    // Ruby structural keywords are complete words, not identifiers to complete:
    // typing `end` (or `do`, `if`, ...) must not fuzz-match every name that
    // merely contains those letters (osc_send, midi_pitch_bend, ...).
    static const QSet<QString> kRubyKeywords = {
        QStringLiteral("end"),    QStringLiteral("do"),     QStringLiteral("then"),
        QStringLiteral("begin"),  QStringLiteral("rescue"), QStringLiteral("ensure"),
        QStringLiteral("else"),   QStringLiteral("elsif"),  QStringLiteral("when"),
        QStringLiteral("while"),  QStringLiteral("until"),  QStringLiteral("for"),
        QStringLiteral("if"),     QStringLiteral("unless"), QStringLiteral("case"),
        QStringLiteral("def"),    QStringLiteral("class"),  QStringLiteral("module"),
        QStringLiteral("return"), QStringLiteral("next"),   QStringLiteral("break"),
        QStringLiteral("yield"),  QStringLiteral("and"),    QStringLiteral("or"),
        QStringLiteral("not"),    QStringLiteral("in")
    };
    if (kRubyKeywords.contains(partial))
    {
        endPreview();
        m_completion->hidePopup();
        return;
    }

    auto* api = dynamic_cast<ScintillaAPI*>(lexer() ? lexer()->apis() : nullptr);
    if (!api)
    {
        endPreview();
        m_completion->hidePopup();
        return;
    }

    // Remaining text on the line after the caret, so a chord/scale root
    // completion can look ahead to the name argument that follows it.
    int curLine, curCol;
    getCursorPosition(&curLine, &curCol);
    const QString afterCursor = text(curLine).mid(curCol);

    QList<CompletionItem> items = api->completionsFor(context, afterCursor);

    // Offer names defined in this buffer: user functions at a call position, and
    // live_loop/cue/set names where a cue symbol is expected (sync/cue/get/set).
    if (!items.isEmpty())
    {
        const QString k = items.first().kind;
        if (k == "fn")
        {
            static const QRegularExpression reDef(
                QStringLiteral("\\b(?:define|defonce)\\s+:([A-Za-z_][A-Za-z0-9_]*[?!]?)"));
            addBufferDefs(items, text(), reDef, false);
        }
        else if (k == "cue")
        {
            static const QRegularExpression reCue(
                QStringLiteral("\\b(?:live_loop|cue|set)\\s+:([A-Za-z_][A-Za-z0-9_]*)"));
            addBufferDefs(items, text(), reCue, true);
        }
    }

    // With no partial typed yet (e.g. just after a space), only pop up in a
    // "value" position — args/opts, synth/fx/sample/cue names — so `play :e3, `
    // shows the opts immediately. A bare space at the top level (the huge
    // function list, kind "fn") stays quiet.
    if (partial.isEmpty() && (items.isEmpty() || items.first().kind == "fn"))
    {
        endPreview();
        m_completion->hidePopup();
        return;
    }

    // Notes are an ordered instrument, never fuzzy-matched. Digits pick a number
    // directly; a note-name partial (c, cs, e3 …) resolves to the matching pitch
    // nearest middle C. Either way we show the full numeric list, ordered low→high,
    // and navigate it a semitone at a time — never a spray of name matches.
    const bool noteMode = !items.isEmpty() && items.first().kind == "note";
    bool useNumbers = noteMode;   // the numeric instrument, unless a name partial resolves nothing
    int preferNote = -1;
    if (noteMode && !partial.isEmpty())
    {
        // Note symbols carry a leading ':' (":c4"); match on the bare token so a
        // typed ":c" resolves the same as "c".
        const QString pat = partial.startsWith(':') ? partial.mid(1) : partial;
        const bool allDigits = !pat.isEmpty() &&
            std::all_of(pat.begin(), pat.end(), [](QChar c) { return c.isDigit(); });
        if (allDigits)
            preferNote = pat.toInt();
        else if (!pat.isEmpty())
        {
            // Resolve the typed note name to the matching pitch nearest middle C.
            int bestDist = 1000;
            for (const CompletionItem& it : items)
            {
                if (it.note < 0) continue;
                QString name = it.text.startsWith(':') ? it.text.mid(1) : it.text;
                if (name.startsWith(pat, Qt::CaseInsensitive))
                {
                    const int d = qAbs(it.note - 60);
                    if (d < bestDist) { bestDist = d; preferNote = it.note; }
                }
            }
            useNumbers = (preferNote >= 0);   // unresolved name → fall back to fuzzy
        }
    }
    if (useNumbers)
    {
        QList<CompletionItem> nums;
        for (const CompletionItem& it : items)
            if (!it.text.isEmpty() && it.text[0].isDigit()) nums.append(it);
        std::stable_sort(nums.begin(), nums.end(),
                         [](const CompletionItem& a, const CompletionItem& b) {
                             return a.note < b.note;
                         });
        items = nums;
    }

    QList<CompletionItem> filtered;
    if (items.size() == 1 && items.first().slider)
    {
        // A bounded opt offers a single slider value-picker — show it directly;
        // the partial (if any) is the value being typed and is replaced on accept.
        filtered = items;
    }
    else if (useNumbers)
    {
        // No fuzzy filtering — the full instrument stays, Up/Down walk it.
        filtered = items;
    }
    else
    {
        // Fuzzy match + rank: the partial's chars must appear in order, best first.
        QList<QPair<int, int>> ranked; // (score, index into items)
        for (int i = 0; i < items.size(); ++i)
        {
            int sc;
            if (SonicPi::fuzzyMatch(partial, items[i].text, sc))
                ranked.append(qMakePair(sc, i));
        }
        std::stable_sort(ranked.begin(), ranked.end(),
                         [](const QPair<int, int>& a, const QPair<int, int>& b) {
                             return a.first > b.first;
                         });
        for (const QPair<int, int>& r : ranked)
            filtered.append(items[r.second]);
    }

    if (filtered.isEmpty())
    {
        endPreview();
        m_completion->hidePopup();
        return;
    }

    // Track the editor's code font + live zoom. The list sits a touch below the
    // editor text size and is clamped; the docstring reads like prose, so it
    // tracks the editor's effective (zoomed) size directly — derived from the
    // same zoomed value, NOT the clamped list size, so it keeps following zoom
    // even once the list font saturates at its max. Notched down by a fixed
    // offset so the prose sits comfortably below the editor text size.
    constexpr double kDocFontOffset = 3.0;
    QFont codeFont = lexer() ? lexer()->defaultFont() : font();
    const double zoomed = codeFont.pointSize() + SendScintilla(SCI_GETZOOM);
    codeFont.setPointSizeF(qBound(8.0, zoomed * 0.82, 15.0));
    m_completion->setItemFont(codeFont, zoomed - kDocFontOffset);

    const int tokenEnd = tokenEndForCaret(pos);
    int wordStart = tokenEnd - partial.length();
    int x = SendScintilla(SCI_POINTXFROMPOSITION, 0, wordStart);
    int y = SendScintilla(SCI_POINTYFROMPOSITION, 0, pos);
    int line = SendScintilla(SCI_LINEFROMPOSITION, pos);
    int lh = SendScintilla(SCI_TEXTHEIGHT, line);
    // SCI coords are viewport-relative → map to global for the top-level popup.
    QPoint globalTop = viewport() ? viewport()->mapToGlobal(QPoint(x, y))
                                  : mapToGlobal(QPoint(x, y));
    m_completion->showItems(filtered, globalTop, lh, preferNote);

    // Arm the live preview but don't write anything yet: the buffer keeps the
    // user's typed text until they choose an entry (nav/click/drag), at which
    // point previewChanged → applyPreview() swaps it in. The managed span starts
    // at sepStart (eating whitespace a separating comma replaces) so the preview
    // shows the comma in place. m_pvOriginal is the Escape-restore text.
    const bool nowSlider = m_completion->isSliderMode();
    int sepStart;
    m_pvPrefix = nowSlider ? QString() : argSeparatorBefore(context, wordStart, sepStart);
    if (m_pvPrefix.isEmpty()) sepStart = wordStart;
    const QString original = text(sepStart, tokenEnd);   // whitespace + typed partial
    if (m_pvStart < 0 || nowSlider != m_pvSlider) m_pvOriginal = original;
    m_pvStart = sepStart;
    m_pvLen = original.length();
    m_pvSlider = nowSlider;
    m_pvRestore = original;
}

// A new argument (positional value or opt) typed after a preceding argument
// needs a separating comma: `scale 60 :a` → `scale 60, :aeolian`,
// `play 43 amp:` → `play 43, amp:`. Add it only when there's an argument before
// this one (not just the function name) and they aren't already comma/bracket
// separated — and not when the value belongs to a preceding opt (`note: 60`).
QString SonicPiScintilla::argSeparatorBefore(const QStringList& context,
                                             int wordStart, int& replaceStart)
{
    replaceStart = wordStart;

    // Locate the current call's argument run: skip a leading `lvalue =` (or any
    // operator token), then the function name. Tokens after that are existing
    // arguments — only then does a freshly-chosen one need a ", " to join them
    // (so `a = scale` completes the function, not `a =, scale`).
    QStringList words;
    for (int i = 0; i < context.size() - 1; ++i)
        if (!context[i].isEmpty()) words << context[i];
    int fnIdx = 0;
    for (int i = 0; i < words.size(); ++i) {
        const QChar c0 = words[i][0];
        const bool valueLike = c0.isLetterOrNumber() || c0 == ':' || c0 == '_'
                               || c0 == '\'' || c0 == '"';
        if (!valueLike) fnIdx = i + 1;   // an operator/assignment resets the call
    }
    if (words.size() - (fnIdx + 1) < 1)   // completing the function or its first arg
        return QString();

    int j = wordStart - 1;
    while (j >= 0) {
        const char c = (char)SendScintilla(SCI_GETCHARAT, j);
        if (c == ' ' || c == '\t') { --j; continue; }
        break;
    }
    if (j >= 0) {
        const char c = (char)SendScintilla(SCI_GETCHARAT, j);
        // ':' = the value belongs to a preceding opt (`note: 60`), not a new arg.
        if (c != ',' && c != '(' && c != '[' && c != '{' && c != ':') {
            replaceStart = j + 1;      // eat the whitespace after the value
            return QStringLiteral(", ");
        }
    }
    return QString();
}

void SonicPiScintilla::acceptCompletion()
{
    if (!m_completion) return;
    QString chosen = m_completion->currentText();
    clearPreview();   // restore the typed text, then do the real insert below
    endPreview();
    m_completion->hidePopup();
    if (chosen.isEmpty()) return;

    int pos = SendScintilla(SCI_GETCURRENTPOS);
    int context_start, last_word_start;
    QStringList context = apiContext(pos, context_start, last_word_start);
    QString partial = context.isEmpty() ? QString() : context.last();
    const int tokenEnd = tokenEndForCaret(pos);
    int wordStart = tokenEnd - partial.length();

    int selStart;
    QString insert = argSeparatorBefore(context, wordStart, selStart) + chosen;
    SendScintilla(SCI_SETSEL, selStart, tokenEnd);
    replaceSelectedText(insert);
}

bool SonicPiScintilla::completionActive() const
{
    return m_completion && m_completion->isShowing();
}

void SonicPiScintilla::acceptCompletionPopup()
{
    acceptCompletion();
}

// Replace the previewed span [m_pvStart, m_pvStart+m_pvLen) with `text`.
void SonicPiScintilla::replacePreviewSpan(const QString& text)
{
    m_pvGuard = true;
    SendScintilla(SCI_SETSEL, m_pvStart, m_pvStart + m_pvLen);
    replaceSelectedText(text);
    const int end = m_pvStart + (int)text.length();
    SendScintilla(SCI_SETSEL, end, end);
    m_pvLen = text.length();
    m_pvGuard = false;
}

void SonicPiScintilla::applyPreview(const QString& sel)
{
    if (m_pvStart < 0) return;
    // With a screen reader active, a list preview's buffer edit gets spoken on top
    // of the popup's own announcement (doubled/garbled speech). Skip the visual
    // preview for lists; the announcement conveys the selection and Enter still
    // commits. Slider values are committed live, so keep those.
    if (!m_pvSlider && QAccessible::isActive()) return;
    // Prepend any separating comma so the preview reads exactly as the commit will.
    replacePreviewSpan(m_pvPrefix + sel);
    m_pvLive = true;   // a selection is now shown in the buffer (Space can commit it)
    // A slider value is committed live, so the restore target tracks it (typing a
    // comma keeps the value); a list preview restores to the typed filter instead.
    if (m_pvSlider) m_pvRestore = sel;
}

void SonicPiScintilla::clearPreview()
{
    if (m_pvStart < 0 || !m_pvLive) return;   // nothing written to restore
    m_pvLive = false;
    replacePreviewSpan(m_pvRestore);
}

void SonicPiScintilla::restoreOriginal()
{
    m_pvLive = false;
    if (m_pvStart < 0) return;
    replacePreviewSpan(m_pvOriginal);
}

void SonicPiScintilla::endPreview()
{
    m_pvStart = -1;
    m_pvLen = 0;
    m_pvSlider = false;
    m_pvLive = false;
    m_pvRestore.clear();
    m_pvOriginal.clear();
}

void SonicPiScintilla::popCompletionOnClick()
{
    if (!m_completion || !m_completionEnabled || m_completion->isShowing()) return;
    auto* api = dynamic_cast<ScintillaAPI*>(lexer() ? lexer()->apis() : nullptr);
    if (!api) return;
    int pos = SendScintilla(SCI_GETCURRENTPOS);
    int cs, lws;
    QStringList context = apiContext(pos, cs, lws);
    if (context.isEmpty()) return;

    QList<CompletionItem> items = api->completionsFor(context);
    if (items.isEmpty()) return;

    // Only pop where the click lands in a tangible value slot — a note/chord/scale
    // (keyboard preview) or a bounded opt (`pan: 0.5` slider). Never the plain
    // function list, which would be intrusive on every click.
    const QString k = items.first().kind;
    const bool slider = (items.size() == 1 && items.first().slider);
    if (slider || k == "note" || k == "chord" || k == "scale" || k == "tuning")
        updateCompletion();
}

void SonicPiScintilla::mouseReleaseEvent(QMouseEvent* e)
{
    // A click in the editor dismisses an open popup (clicks on the popup never
    // reach the editor). clearPreview() keeps typed text: it drops only an
    // un-committed list preview and leaves a committed slider value in place.
    const bool dismissed = m_completion && m_completion->isShowing();
    if (dismissed)
        dismissCompletionKeepTyped();
    QsciScintilla::mouseReleaseEvent(e);
    // Don't reopen on the very click that dismissed it; a fresh click onto a
    // note/chord/scale or `pan: 0.5` value still opens its preview.
    if (!dismissed)
        popCompletionOnClick();
}

void SonicPiScintilla::dropEvent(QDropEvent* dropEvent)
{
    mutex->lock();
    if (dropEvent->mimeData()->hasFormat("text/uri-list"))
    {
        dropEvent->acceptProposedAction();
        QList<QUrl> urlList = dropEvent->mimeData()->urls();
        QString text;
        for (int i = 0; i < urlList.size(); ++i)
        {
            text += "\"" + urlList.at(i).toLocalFile() + "\"" + QLatin1Char('\n');
        }
        insert(text);
    }
    else if (dropEvent->mimeData()->hasFormat(kCardMime))
    {
        // Commit at the preview box's position (m_dropPreviewPos), not the
        // pointer's line: within a multi-line block the pointer can sit lines
        // below the box. finaliseDropPreview re-inserts as one undoable step.
        dropEvent->acceptProposedAction();
        if (m_dropPreviewPos < 0) // drop with no live preview: place it first
            placeDropPreview((int)dropLineStart(this, dropEvent->position().toPoint()),
                             dropEvent->mimeData()->text(),
                             QString::fromUtf8(dropEvent->mimeData()->data(kCardMime)));
        mutex->unlock();
        finaliseDropPreview();
        return;
    }
    mutex->unlock();
}

void SonicPiScintilla::sp_paste()
{
    mutex->lock();
    SendScintilla(QsciCommand::Paste);
    deselect();
    mutex->unlock();
}

void SonicPiScintilla::sp_cut()
{
    mutex->lock();
    SendScintilla(QsciCommand::SelectionCut);
    deselect();
    mutex->unlock();
}

void SonicPiScintilla::showAutoCompletion(bool val)
{
    // Toggles our custom popup; the native Scintilla list stays disabled.
    m_completionEnabled = val;
    if (!val && m_completion)
    {
        m_completion->hidePopup();
    }
}

void SonicPiScintilla::setCompletionHelp(bool val)
{
    // When off, the popup is a plain word list (no docstring/piano/slider panes).
    if (m_completion) m_completion->setShowHelp(val);
}

void SonicPiScintilla::setText(const QString& text)
{
    SendScintilla(SCI_CLEARALL);
    QByteArray bytes = textAsBytes(text);
    SendScintilla(SCI_ADDTEXT, bytes.size(), bytes.constData());
}

void SonicPiScintilla::setAutoIndentEnabled(bool enabled)
{
    this->autoIndent = enabled;
}

// Single-char moves and deletes arrive via the shortcut path (Ctrl+b/f/h/d
// etc.), bypassing event(); re-derive the popup context after each, as the
// raw keys do.
void SonicPiScintilla::charRight()
{
    mutex->lock();
    SendScintilla(QsciCommand::CharRight);
    mutex->unlock();
    if (m_completion && m_completion->isShowing())
        updateCompletion();
}

void SonicPiScintilla::charLeft()
{
    mutex->lock();
    SendScintilla(QsciCommand::CharLeft);
    mutex->unlock();
    if (m_completion && m_completion->isShowing())
        updateCompletion();
}

void SonicPiScintilla::deleteForward()
{
    mutex->lock();
    SendScintilla(QsciCommand::Delete);
    mutex->unlock();
    if (m_completion && m_completion->isShowing())
        updateCompletion();
}

void SonicPiScintilla::deleteBack()
{
    mutex->lock();
    SendScintilla(QsciCommand::DeleteBack);
    mutex->unlock();
    if (m_completion && m_completion->isShowing())
        updateCompletion();
}

void SonicPiScintilla::dismissCompletionKeepTyped()
{
    if (!m_completion || !m_completion->isShowing())
        return;
    clearPreview();
    endPreview();
    m_completion->hidePopup();
}

void SonicPiScintilla::lineStart()
{
    dismissCompletionKeepTyped();
    mutex->lock();
    SendScintilla(QsciCommand::Home);
    mutex->unlock();
}

void SonicPiScintilla::lineEnd()
{
    dismissCompletionKeepTyped();
    mutex->lock();
    SendScintilla(QsciCommand::LineEnd);
    mutex->unlock();
}

void SonicPiScintilla::documentStart()
{
    dismissCompletionKeepTyped();
    mutex->lock();
    SendScintilla(QsciCommand::DocumentStart);
    mutex->unlock();
}

void SonicPiScintilla::documentEnd()
{
    dismissCompletionKeepTyped();
    mutex->lock();
    SendScintilla(QsciCommand::DocumentEnd);
    mutex->unlock();
}

void SonicPiScintilla::wordRight()
{
    dismissCompletionKeepTyped();
    mutex->lock();
    SendScintilla(QsciCommand::WordRight);
    mutex->unlock();
}

void SonicPiScintilla::wordLeft()
{
    dismissCompletionKeepTyped();
    mutex->lock();
    SendScintilla(QsciCommand::WordLeft);
    mutex->unlock();
}

void SonicPiScintilla::selectLineStart()
{
    dismissCompletionKeepTyped();
    mutex->lock();
    SendScintilla(QsciCommand::HomeExtend);
    mutex->unlock();
}

void SonicPiScintilla::selectLineEnd()
{
    dismissCompletionKeepTyped();
    mutex->lock();
    SendScintilla(QsciCommand::LineEndExtend);
    mutex->unlock();
}

void SonicPiScintilla::selectWordRight()
{
    dismissCompletionKeepTyped();
    mutex->lock();
    SendScintilla(QsciCommand::WordRightExtend);
    mutex->unlock();
}

void SonicPiScintilla::selectWordLeft()
{
    dismissCompletionKeepTyped();
    mutex->lock();
    SendScintilla(QsciCommand::WordLeftExtend);
    mutex->unlock();
}

void SonicPiScintilla::selectDocStart()
{
    mutex->lock();
    SendScintilla(QsciCommand::DocumentStartExtend);
    mutex->unlock();
}

void SonicPiScintilla::selectDocEnd()
{
    mutex->lock();
    SendScintilla(QsciCommand::DocumentEndExtend);
    mutex->unlock();
}

void SonicPiScintilla::centerCaret()
{
    mutex->lock();
    SendScintilla(QsciCommand::VerticalCentreCaret);
    mutex->unlock();
}

// Undo positions are recorded without any live preview text (previews stay
// out of undo collection), so drop the preview before replaying.
void SonicPiScintilla::undo()
{
    mutex->lock();
    clearDropPreview();
    SendScintilla(QsciCommand::Undo);
    mutex->unlock();
}

void SonicPiScintilla::redo()
{
    mutex->lock();
    clearDropPreview();
    SendScintilla(QsciCommand::Redo);
    mutex->unlock();
}

void SonicPiScintilla::selectAll()
{
    mutex->lock();
    SendScintilla(QsciCommand::SelectAll);
    mutex->unlock();
}

void SonicPiScintilla::deleteWordRight()
{
    mutex->lock();
    SendScintilla(QsciCommand::DeleteWordRight);
    mutex->unlock();
}

void SonicPiScintilla::deleteWordLeft()
{
    mutex->lock();
    SendScintilla(QsciCommand::DeleteWordLeft);
    mutex->unlock();
}
