//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include "sonicpitooltip.h"
#include "dpi.h"
#include "model/sonicpitheme.h"

#include <QAbstractButton>
#include <QAbstractItemView>
#include <QAbstractTextDocumentLayout>
#include <QAction>
#include <QApplication>
#include <QGroupBox>
#include <QFontMetrics>
#include <QHelpEvent>
#include <QKeySequence>
#include <QMouseEvent>
#include <QPainter>
#include <QPainterPath>
#include <QRegularExpression>
#include <QScreen>
#include <QTabBar>
#include <QTextBlockFormat>
#include <QTextCursor>
#include <QToolButton>
#include <QToolTip>
#include <QtMath>

#ifdef Q_OS_MAC
#include "platform/macos.h"
#endif

namespace
{
// Vertical gap between the control and the caret tip.
constexpr int ANCHOR_GAP = 4;
// Spacing between title and body, and between body and the shortcut chip.
constexpr int TITLE_GAP = 5;
constexpr int CHIP_GAP = 8;

// Qt's stock tooltip font is ~11px — too small for multi-line help text.
QFont bodyFontForTip()
{
    QFont f = QToolTip::font();
    f.setPointSizeF(f.pointSizeF() + 1.5);
    return f;
}

QFont chipFont()
{
    QFont f = bodyFontForTip();
    f.setFamily("Hack");
    f.setPointSizeF(f.pointSizeF() - 1.0);
    return f;
}

// Comfortable leading for wrapped help text; single-spaced lines at
// tooltip sizes read cramped.
void applyLineHeight(QTextDocument& doc, int percent)
{
    QTextCursor cursor(&doc);
    cursor.select(QTextCursor::Document);
    QTextBlockFormat bf;
    bf.setLineHeight(percent, QTextBlockFormat::ProportionalHeight);
    cursor.mergeBlockFormat(bf);
}

// Widget label → tooltip heading: drop the mnemonic marker (& — && is a
// literal ampersand) and any trailing colon. Symbol-only labels ("↑", "+")
// make junk headings, so they yield no title.
QString cleanTitle(QString s)
{
    s.replace(QStringLiteral("&&"), QString(QChar(0x1F)));
    s.remove(QLatin1Char('&'));
    s.replace(QChar(0x1F), QLatin1Char('&'));
    s = s.trimmed();
    if (s.endsWith(QLatin1Char(':')))
        s.chop(1);
    static const QRegularExpression wordChar(QStringLiteral("[\\p{L}\\p{N}]"));
    if (!s.contains(wordChar))
        return QString();
    return s;
}

// Opaque blend of a→b. Borders must be opaque: a semi-transparent border
// lets the drop shadow bleed through, which reads as a smudged edge
// (especially over light themes).
QColor mix(const QColor& a, const QColor& b, qreal t)
{
    return QColor(int(a.red() + (b.red() - a.red()) * t),
                  int(a.green() + (b.green() - a.green()) * t),
                  int(a.blue() + (b.blue() - a.blue()) * t));
}
}

SonicPiToolTip::SonicPiToolTip(SonicPiTheme* theme)
    : QWidget(nullptr)
    , m_theme(theme)
{
    // NoDropShadowWindowHint: the OS would otherwise draw its own shadow
    // around the (mostly transparent) window rect, which shows up as a dark
    // outline floating clear of the bubble. The shadow is painted by us.
    setWindowFlags(Qt::ToolTip | Qt::FramelessWindowHint | Qt::WindowDoesNotAcceptFocus
                   | Qt::NoDropShadowWindowHint);
    setAttribute(Qt::WA_TranslucentBackground);
    // Never intercept the pointer — the tip must not swallow hover or
    // clicks meant for what's underneath it.
    setAttribute(Qt::WA_TransparentForMouseEvents);
    setAttribute(Qt::WA_ShowWithoutActivating);
    setFocusPolicy(Qt::NoFocus);

    m_titleDoc.setDocumentMargin(0);
    m_bodyDoc.setDocumentMargin(0);

    m_fade.setDuration(140);
    m_fade.setStartValue(0.0);
    m_fade.setEndValue(1.0);
    m_fade.setEasingCurve(QEasingCurve::OutCubic);
    connect(&m_fade, &QVariantAnimation::valueChanged, this,
            [this](const QVariant& v) { setWindowOpacity(v.toReal()); });
}

void SonicPiToolTip::showTip(const QRect& anchorGlobal, const QString& title,
                             const QString& body, const QString& shortcut)
{
    if (body.isEmpty())
    {
        hideTip();
        return;
    }

    const bool alreadyVisible = isVisible();

    m_title = title;
    m_body = body;
    m_shortcut = shortcut;

    layoutContent();
    place(anchorGlobal);
    update();

    if (!alreadyVisible)
    {
        m_fade.stop();
        setWindowOpacity(0.0);
        show();
#ifdef Q_OS_MAC
        // Qt::ToolTip windows sit above the Cmd-Tab switcher; drop to
        // floating level. Also keep the window itself out of the AX tree —
        // screen readers get tip text from the widget's accessible
        // description, not from this transient window.
        SonicPi::setPopupBelowSwitcher(reinterpret_cast<void*>(winId()));
        SonicPi::setWindowAccessibilityIgnored(reinterpret_cast<void*>(winId()));
#endif
        m_fade.start();
    }
}

void SonicPiToolTip::hideTip()
{
    m_fade.stop();
    hide();
}

void SonicPiToolTip::layoutContent()
{
    m_padX = ScaleWidthForDPI(13);
    m_padY = ScaleHeightForDPI(10);
    m_radius = ScaleHeightForDPI(7);
    m_caretW = ScaleWidthForDPI(14);
    m_caretH = ScaleHeightForDPI(7);
    m_shadow = ScaleHeightForDPI(14);

    const QFont bodyFont = bodyFontForTip();
    QFont titleFont = bodyFont;
    titleFont.setPointSizeF(bodyFont.pointSizeF() + 2.0);
    titleFont.setWeight(QFont::Bold);

    m_bodyDoc.setDefaultFont(bodyFont);
    if (Qt::mightBeRichText(m_body))
        m_bodyDoc.setHtml(m_body);
    else
        m_bodyDoc.setPlainText(m_body);
    applyLineHeight(m_bodyDoc, 118);

    m_titleDoc.setDefaultFont(titleFont);
    m_titleDoc.setPlainText(m_title);

    m_chipW = m_chipH = 0;
    if (!m_shortcut.isEmpty())
    {
        const QFontMetrics fm(chipFont());
        m_chipW = fm.horizontalAdvance(m_shortcut) + ScaleWidthForDPI(14);
        m_chipH = fm.height() + ScaleHeightForDPI(8);
    }

    // Natural (unwrapped) width, capped for readability; both docs then
    // wrap at the same width so the bubble hugs its content.
    const int maxTextW = ScaleWidthForDPI(330);
    m_titleDoc.setTextWidth(-1);
    m_bodyDoc.setTextWidth(-1);
    int contentW = qCeil(m_bodyDoc.idealWidth());
    if (!m_title.isEmpty())
        contentW = qMax(contentW, qCeil(m_titleDoc.idealWidth()));
    contentW = qMax(contentW, m_chipW);
    contentW = qMin(contentW, maxTextW);
    m_titleDoc.setTextWidth(contentW);
    m_bodyDoc.setTextWidth(contentW);

    int h = m_padY;
    if (!m_title.isEmpty())
        h += qCeil(m_titleDoc.size().height()) + ScaleHeightForDPI(TITLE_GAP);
    h += qCeil(m_bodyDoc.size().height());
    if (m_chipH > 0)
        h += ScaleHeightForDPI(CHIP_GAP) + m_chipH;
    h += m_padY;

    m_bubble = QRect(0, 0, contentW + 2 * m_padX, h);
}

void SonicPiToolTip::place(const QRect& anchor)
{
    QScreen* screen = QGuiApplication::screenAt(anchor.center());
    if (!screen)
        screen = QGuiApplication::primaryScreen();
    const QRect scr = screen->availableGeometry();
    const int margin = ScaleWidthForDPI(6);
    const int gap = ScaleHeightForDPI(ANCHOR_GAP);

    const int bw = m_bubble.width();
    const int bh = m_bubble.height();

    // Prefer below the control; flip above when there's no room.
    const int belowTop = anchor.bottom() + gap + m_caretH;
    const int aboveTop = anchor.top() - gap - m_caretH - bh;
    m_below = (belowTop + bh + margin <= scr.bottom()) || (aboveTop < scr.top() + margin);

    int bx = anchor.center().x() - bw / 2;
    bx = qBound(scr.left() + margin, bx, qMax(scr.left() + margin, scr.right() - margin - bw));
    int by = m_below ? belowTop : aboveTop;
    by = qBound(scr.top() + margin, by, qMax(scr.top() + margin, scr.bottom() - margin - bh));

    // Widget frame = bubble + shadow margin all round + caret on one edge.
    const int wx = bx - m_shadow;
    const int wy = by - m_shadow - (m_below ? m_caretH : 0);
    const int ww = bw + 2 * m_shadow;
    const int wh = bh + m_caretH + 2 * m_shadow;

    m_bubble.moveTo(m_shadow, m_shadow + (m_below ? m_caretH : 0));

    // Caret tracks the control's centre but stays clear of the corners.
    const int minCaretX = m_bubble.left() + m_radius + m_caretW / 2 + 2;
    const int maxCaretX = m_bubble.right() - m_radius - m_caretW / 2 - 2;
    m_caretX = qBound(minCaretX, anchor.center().x() - wx, qMax(minCaretX, maxCaretX));

    setGeometry(wx, wy, ww, wh);
}

void SonicPiToolTip::paintEvent(QPaintEvent* event)
{
    Q_UNUSED(event);

    QPainter p(this);
    p.setRenderHint(QPainter::Antialiasing);

    const QColor bg = m_theme->color("ToolTipBase");
    const QColor fg = m_theme->color("ToolTipText");
    const bool highContrast = m_theme->getStyle() == SonicPiTheme::HighContrastMode;

    // Bubble + caret as one outline.
    const QRectF bubble(m_bubble);
    QPainterPath path;
    path.addRoundedRect(bubble, m_radius, m_radius);
    const qreal baseY = m_below ? bubble.top() + 1.0 : bubble.bottom() - 1.0;
    const qreal tipY = m_below ? bubble.top() - m_caretH : bubble.bottom() + m_caretH;
    QPolygonF tri;
    tri << QPointF(m_caretX - m_caretW / 2.0, baseY)
        << QPointF(m_caretX, tipY)
        << QPointF(m_caretX + m_caretW / 2.0, baseY);
    QPainterPath caret;
    caret.addPolygon(tri);
    caret.closeSubpath();
    path = path.united(caret);

    // Soft shadow: concentric strokes fading outward (cheap, reliable
    // fake blur — no compositor dependency). High contrast mode swaps it
    // for a strong border instead.
    if (!highContrast)
    {
        const QPainterPath shadowPath = path.translated(0, ScaleHeightForDPI(2));
        const int steps = 8;
        p.setBrush(Qt::NoBrush);
        for (int i = steps; i >= 1; --i)
        {
            const qreal t = i / qreal(steps);
            const int alpha = int(26 * (1.0 - t) * (1.0 - t));
            if (alpha <= 0)
                continue;
            QPen pen(QColor(0, 0, 0, alpha), (m_shadow * 2.0 * i) / steps);
            pen.setJoinStyle(Qt::RoundJoin);
            p.setPen(pen);
            p.drawPath(shadowPath);
        }
    }

    const QColor border = highContrast ? fg : mix(bg, fg, 0.4);
    p.setPen(QPen(border, highContrast ? qMax(2, ScaleHeightForDPI(2)) : 1.0));
    p.setBrush(bg);
    p.drawPath(path);

    QAbstractTextDocumentLayout::PaintContext ctx;
    ctx.palette.setColor(QPalette::Text, fg);

    const qreal x = bubble.left() + m_padX;
    qreal y = bubble.top() + m_padY;

    if (!m_title.isEmpty())
    {
        p.save();
        p.translate(x, y);
        m_titleDoc.documentLayout()->draw(&p, ctx);
        p.restore();
        y += m_titleDoc.size().height() + ScaleHeightForDPI(TITLE_GAP);
    }

    p.save();
    p.translate(x, y);
    m_bodyDoc.documentLayout()->draw(&p, ctx);
    p.restore();
    y += m_bodyDoc.size().height();

    if (!m_shortcut.isEmpty())
    {
        y += ScaleHeightForDPI(CHIP_GAP);
        const QRectF chip(x, y, m_chipW, m_chipH);
        const QColor chipBg = mix(bg, fg, 0.08);
        const QColor chipBorder = highContrast ? fg : mix(bg, fg, 0.5);
        p.setPen(QPen(chipBorder, 1.0));
        p.setBrush(chipBg);
        const qreal chipRadius = ScaleHeightForDPI(4);
        p.drawRoundedRect(chip, chipRadius, chipRadius);
        p.setFont(chipFont());
        p.setPen(fg);
        p.drawText(chip, Qt::AlignCenter, m_shortcut);
    }
}

//
// ---------------------------------------------------------------------
//

SonicPiToolTipManager::SonicPiToolTipManager(SonicPiTheme* theme, QObject* parent)
    : QObject(parent)
    , m_tip(new SonicPiToolTip(theme))
{
    m_focusTipTimer.setSingleShot(true);
    m_focusTipTimer.setInterval(650);
    connect(&m_focusTipTimer, &QTimer::timeout, this, &SonicPiToolTipManager::onFocusTipTimer);
    qApp->installEventFilter(this);
}

SonicPiToolTipManager::~SonicPiToolTipManager()
{
    delete m_tip;
}

QWidget* SonicPiToolTipManager::resolveTip(QWidget* w, Tip& tip)
{
    // Mirror Qt's own QEvent::ToolTip propagation: walk up the parent
    // chain (stopping at the window) to the first widget with a tip.
    for (QWidget* t = w; t; t = t->isWindow() ? nullptr : t->parentWidget())
    {
        if (t->toolTip().isEmpty())
            continue;
        tip.body = t->toolTip();
        tip.title = t->property("tipTitle").toString();
        tip.shortcut = t->property("tipShortcut").toString();
        if (tip.shortcut.isEmpty())
        {
            // Toolbar buttons: the key-cap chip comes straight from the
            // action's shortcut, so it never drifts out of sync.
            if (QToolButton* tb = qobject_cast<QToolButton*>(t))
            {
                if (QAction* a = tb->defaultAction())
                    tip.shortcut = a->shortcut().toString(QKeySequence::NativeText);
            }
        }
        if (tip.title.isEmpty())
        {
            // Uniform title + body: when no explicit tipTitle is set, use
            // the control's own name as the heading so every tip has the
            // same shape.
            if (QToolButton* tb = qobject_cast<QToolButton*>(t))
            {
                if (QAction* a = tb->defaultAction())
                    tip.title = cleanTitle(a->iconText());
            }
            else if (QAbstractButton* b = qobject_cast<QAbstractButton*>(t))
                tip.title = cleanTitle(b->text());
            else if (QGroupBox* g = qobject_cast<QGroupBox*>(t))
                tip.title = cleanTitle(g->title());
            // A heading that just repeats the body adds nothing.
            if (tip.title.compare(tip.body, Qt::CaseInsensitive) == 0)
                tip.title.clear();
        }
        return t;
    }
    return nullptr;
}

bool SonicPiToolTipManager::eventFilter(QObject* obj, QEvent* event)
{
    if (obj == m_tip)
        return false;

    switch (event->type())
    {
    case QEvent::ToolTip:
    {
        QWidget* w = qobject_cast<QWidget*>(obj);
        if (!w)
            return false;
        QHelpEvent* he = static_cast<QHelpEvent*>(event);

        // Item views: per-item tips anchored to the item's rect.
        if (QAbstractItemView* view = qobject_cast<QAbstractItemView*>(w->parentWidget()))
        {
            if (w == view->viewport())
            {
                const QModelIndex idx = view->indexAt(he->pos());
                const QString itemTip = idx.isValid() ? idx.data(Qt::ToolTipRole).toString() : QString();
                if (!itemTip.isEmpty())
                {
                    Tip tip;
                    tip.body = itemTip;
                    QRect r = view->visualRect(idx);
                    r.moveTopLeft(w->mapToGlobal(r.topLeft()));
                    showTip(w, tip, r, false);
                    return true;
                }
            }
        }

        // Tab bars: per-tab tips anchored to the tab.
        if (QTabBar* tabs = qobject_cast<QTabBar*>(w))
        {
            const int i = tabs->tabAt(he->pos());
            const QString tabTip = i >= 0 ? tabs->tabToolTip(i) : QString();
            if (!tabTip.isEmpty())
            {
                Tip tip;
                tip.body = tabTip;
                QRect r = tabs->tabRect(i);
                r.moveTopLeft(tabs->mapToGlobal(r.topLeft()));
                showTip(tabs, tip, r, false);
                return true;
            }
        }

        Tip tip;
        QWidget* t = resolveTip(w, tip);
        if (!t)
        {
            if (m_tip->isVisible())
                hideTip();
            return false;
        }

        QRect anchor(t->mapToGlobal(QPoint(0, 0)), t->size());
        // For large widgets (group boxes, lists) an edge-anchored bubble
        // floats far from the pointer and reads as unrelated — anchor to
        // the pointer instead.
        const bool cursorAnchored = anchor.height() > ScaleHeightForDPI(120)
            || anchor.width() > ScaleWidthForDPI(440);
        if (cursorAnchored)
        {
            const QPoint gp = w->mapToGlobal(he->pos());
            anchor = QRect(gp - QPoint(ScaleWidthForDPI(8), ScaleHeightForDPI(10)),
                           ScaleForDPI(16, 20));
        }
        showTip(t, tip, anchor, cursorAnchored);
        return true; // handled — stock QToolTip must not also fire
    }

    case QEvent::Leave:
        if (m_tip->isVisible() && obj == m_anchorWidget)
            hideTip();
        return false;

    case QEvent::MouseMove:
    {
        if (!m_tip->isVisible())
            return false;
        QWidget* w = qobject_cast<QWidget*>(obj);
        if (!w)
            return false;
        if (m_anchorWidget && (w == m_anchorWidget || m_anchorWidget->isAncestorOf(w)))
        {
            // Pointer-anchored tips go stale once the pointer wanders off.
            if (m_cursorAnchored)
            {
                const QPoint gp = static_cast<QMouseEvent*>(event)->globalPosition().toPoint();
                if ((gp - m_cursorAnchor).manhattanLength() > ScaleWidthForDPI(28))
                    hideTip();
            }
        }
        else
        {
            hideTip();
        }
        return false;
    }

    case QEvent::MouseButtonPress:
    case QEvent::MouseButtonDblClick:
    case QEvent::Wheel:
        hideTip();
        m_focusTipTimer.stop();
        return false;

    case QEvent::KeyPress:
        // Esc dismisses (WCAG 1.4.13); any other key also hides so tips
        // don't linger while typing. Never consumed — Esc keeps its
        // app-level meaning.
        hideTip();
        m_focusTipTimer.stop();
        return false;

    case QEvent::FocusIn:
    {
        // Keyboard accessibility: tabbing onto a control surfaces its tip
        // after a moment, mirroring what hover gives pointer users.
        QWidget* w = qobject_cast<QWidget*>(obj);
        if (!w)
            return false;
        const Qt::FocusReason reason = static_cast<QFocusEvent*>(event)->reason();
        if (reason == Qt::TabFocusReason || reason == Qt::BacktabFocusReason)
        {
            m_focusCandidate = w;
            m_focusTipTimer.start();
        }
        return false;
    }

    case QEvent::FocusOut:
        if (obj == m_focusCandidate)
            m_focusTipTimer.stop();
        if (m_tip->isVisible() && obj == m_anchorWidget)
            hideTip();
        return false;

    case QEvent::Move:
    case QEvent::Resize:
        if (m_tip->isVisible() && m_anchorWidget && obj == m_anchorWidget->window())
            hideTip();
        return false;

    case QEvent::Hide:
    case QEvent::Destroy:
        if (m_tip->isVisible() && (!m_anchorWidget || obj == m_anchorWidget))
            hideTip();
        return false;

    case QEvent::ApplicationDeactivate:
        hideTip();
        m_focusTipTimer.stop();
        return false;

    default:
        return false;
    }
}

void SonicPiToolTipManager::showTip(QWidget* anchorWidget, const Tip& tip,
                                    const QRect& anchorGlobal, bool cursorAnchored)
{
    // ToolTip events re-fire as the pointer pauses; don't restart the
    // fade when the same tip is already up.
    if (m_tip->isShowing(tip.body) && anchorWidget == m_anchorWidget)
        return;
    m_anchorWidget = anchorWidget;
    m_cursorAnchored = cursorAnchored;
    m_cursorAnchor = anchorGlobal.center();
    m_tip->showTip(anchorGlobal, tip.title, tip.body, tip.shortcut);
}

void SonicPiToolTipManager::hideTip()
{
    m_tip->hideTip();
    m_anchorWidget.clear();
    m_cursorAnchored = false;
}

void SonicPiToolTipManager::onFocusTipTimer()
{
    QWidget* w = m_focusCandidate.data();
    if (!w || !w->hasFocus() || !w->isVisible())
        return;
    Tip tip;
    QWidget* t = resolveTip(w, tip);
    if (!t)
        return;
    QRect anchor(t->mapToGlobal(QPoint(0, 0)), t->size());
    // If the tip lives on a large ancestor, anchor to the focused control
    // itself so the bubble appears where the user is looking.
    if (anchor.height() > ScaleHeightForDPI(120) || anchor.width() > ScaleWidthForDPI(440))
        anchor = QRect(w->mapToGlobal(QPoint(0, 0)), w->size());
    showTip(t, tip, anchor, false);
}
