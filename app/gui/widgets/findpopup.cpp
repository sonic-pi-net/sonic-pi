//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#include "findpopup.h"

#include <QAbstractScrollArea>
#include <QEvent>
#include <QHBoxLayout>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QPainter>
#include <QPropertyAnimation>
#include <QToolButton>

#include "dpi.h"
#include "model/sonicpitheme.h"
#include "utils/tablericons.h"

bool FindPopup::s_forceCase = false;

FindPopup::FindPopup(QWidget* parent)
    : QWidget(parent)
{
    setObjectName("findPopup");
    hide();

    m_icon = new QLabel(this);
    m_icon->setObjectName("findIcon");

    m_edit = new QLineEdit(this);
    m_edit->setObjectName("findLineEdit");
    m_edit->setPlaceholderText(tr("Find"));
    m_edit->setAccessibleName(tr("Find in buffer"));
    m_edit->setClearButtonEnabled(false);
    m_edit->installEventFilter(this);

    m_count = new QLabel(this);
    m_count->setObjectName("findCount");
    m_count->setAlignment(Qt::AlignRight | Qt::AlignVCenter);

    m_case = new QToolButton(this);
    m_case->setObjectName("findCaseBtn");
    m_case->setText(tr("Aa"));
    m_case->setCheckable(true);
    m_case->setChecked(s_forceCase);
    m_case->setToolTip(tr("Match case exactly (otherwise a lowercase search matches any case)"));
    m_case->setFocusPolicy(Qt::NoFocus);

    m_prev = new QToolButton(this);
    m_prev->setObjectName("findPrevBtn");
    m_prev->setToolTip(tr("Previous match (Up, Shift+F3 or Ctrl+R)"));
    m_prev->setFocusPolicy(Qt::NoFocus);

    m_next = new QToolButton(this);
    m_next->setObjectName("findNextBtn");
    m_next->setToolTip(tr("Next match (Down, F3 or Ctrl+S)"));
    m_next->setFocusPolicy(Qt::NoFocus);

    m_close = new QToolButton(this);
    m_close->setObjectName("findCloseBtn");
    m_close->setToolTip(tr("Close find (Escape)"));
    m_close->setFocusPolicy(Qt::NoFocus);

    QHBoxLayout* row = new QHBoxLayout(this);
    row->setContentsMargins(ScaleWidthForDPI(12), ScaleHeightForDPI(4),
                            ScaleWidthForDPI(8), ScaleHeightForDPI(4));
    row->setSpacing(ScaleWidthForDPI(4));
    row->addWidget(m_icon);
    row->addWidget(m_edit, 1);
    row->addWidget(m_count);
    row->addWidget(m_case);
    row->addWidget(m_prev);
    row->addWidget(m_next);
    row->addWidget(m_close);

    applyZoomMetrics();

    connect(m_edit, &QLineEdit::textChanged, this,
            [this](const QString& q) { emit queryChanged(q); });
    connect(m_case, &QToolButton::toggled, this, [this](bool on) {
        s_forceCase = on;
        emit queryChanged(m_edit->text());   // re-search with the new case rule
    });
    connect(m_prev, &QToolButton::clicked, this, [this] { emit prevRequested(); });
    connect(m_next, &QToolButton::clicked, this, [this] { emit nextRequested(); });
    connect(m_close, &QToolButton::clicked, this, [this] { emit closeRequested(false); });

    m_slide = new QPropertyAnimation(this, "pos", this);
    m_slide->setDuration(150);
    m_slide->setEasingCurve(QEasingCurve::OutCubic);

    // Reposition when the editor is resized (the bar hugs its top-right corner).
    parent->installEventFilter(this);
}

bool FindPopup::editHasFocus() const
{
    return m_edit->hasFocus();
}

// ~8% per zoom step tracks the editor's per-point font growth closely
// enough that the bar and the code enlarge in step.
qreal FindPopup::zoomScale() const
{
    return qBound(0.8, 1.0 + m_zoom * 0.08, 2.2);
}

void FindPopup::applyZoomMetrics()
{
    const qreal s = zoomScale();
    setFixedSize(ScaleWidthForDPI(qRound(390 * s)), ScaleHeightForDPI(qRound(44 * s)));
}

void FindPopup::setZoom(int delta)
{
    if (delta == m_zoom)
        return;
    m_zoom = delta;
    applyZoomMetrics();
    // Re-derive the icon pixmaps and stylesheet font sizes at the new scale.
    applyTheme(m_bg, m_fg, m_border, m_accent, m_accentText);
    if (isVisible())
        reposition();
}

QString FindPopup::query() const
{
    return m_edit->text();
}

bool FindPopup::forceCase() const
{
    return m_case->isChecked();
}

void FindPopup::open(const QString& seed)
{
    m_case->setChecked(s_forceCase);   // pick up a toggle made in another buffer
    if (!seed.isNull())
        m_edit->setText(seed);
    const bool wasOpen = isOpen();
    show();
    raise();
    reposition();
    if (!wasOpen)
    {
        // Slide in from just above the editor's top edge.
        const QPoint target = pos();
        m_slide->stop();
        m_slide->setStartValue(QPoint(target.x(), target.y() - height() - ScaleHeightForDPI(8)));
        m_slide->setEndValue(target);
        m_slide->start();
    }
    m_edit->setFocus();
    m_edit->selectAll();
    // setText doesn't emit textChanged when the seed matches the previous
    // query, so always kick a refresh: the highlights need rebuilding.
    emit queryChanged(m_edit->text());
}

void FindPopup::closePopup()
{
    m_slide->stop();
    hide();
}

void FindPopup::setMatchStatus(int current, int total)
{
    const bool none = (total == 0);
    if (none != m_noMatches)
    {
        m_noMatches = none;
        restyle();
        update();
    }
    if (total < 0)
        m_count->clear();
    else if (total == 0)
        m_count->setText(tr("No results"));
    else
        m_count->setText(QString("%1/%2").arg(current).arg(total));
    m_prev->setEnabled(total > 0);
    m_next->setEnabled(total > 0);
}

void FindPopup::reposition()
{
    QWidget* par = parentWidget();
    if (!par)
        return;
    QRect area = par->rect();
    if (auto* scroll = qobject_cast<QAbstractScrollArea*>(par))
        area = scroll->viewport()->geometry();   // clear of the vertical scrollbar
    m_slide->stop();
    move(area.right() - width() - ScaleWidthForDPI(12), area.top() + ScaleHeightForDPI(8));
}

void FindPopup::applyTheme(const QColor& surface, const QColor& surfaceText,
                           const QColor& border, const QColor& accent,
                           const QColor& accentText)
{
    m_bg = surface;
    m_fg = surfaceText;
    m_border = border;
    m_accent = accent;
    m_accentText = accentText;

    QPalette pal = m_edit->palette();
    pal.setColor(QPalette::PlaceholderText, SonicPiTheme::blend(m_fg, m_bg, 0.55));
    m_edit->setPalette(pal);

    const int px = ScaleWidthForDPI(qRound(16 * zoomScale()));
    const qreal dpr = devicePixelRatioF();
    const QColor muted = SonicPiTheme::blend(m_fg, m_bg, 0.45);
    m_icon->setPixmap(TablerIcons::pixmap(TablerIcons::Glyph::Search, muted, px, dpr));
    m_prev->setIconSize(QSize(px, px));
    m_next->setIconSize(QSize(px, px));
    m_close->setIconSize(QSize(px, px));
    m_prev->setIcon(TablerIcons::icon(TablerIcons::Glyph::ChevronUp, m_fg, px, dpr));
    m_next->setIcon(TablerIcons::icon(TablerIcons::Glyph::ChevronDown, m_fg, px, dpr));
    m_close->setIcon(TablerIcons::icon(TablerIcons::Glyph::X, muted, px, dpr));

    restyle();
    update();
}

void FindPopup::restyle()
{
    const QColor muted = SonicPiTheme::blend(m_fg, m_bg, 0.45);
    const QColor hover = SonicPiTheme::blend(m_fg, m_bg, 0.85);
    const int editPx = qRound(14 * zoomScale());
    const int smallPx = qRound(12 * zoomScale());
    // No-results state speaks in the theme accent (the house attention
    // colour) — nothing outside the palette.
    // margin/padding zeroed explicitly: the app-wide QToolButton rule's 6dx
    // margins + 4dx padding would otherwise consume these tiny buttons' boxes.
    setStyleSheet(QString(
        "#findPopup { background: transparent; }"
        "#findIcon { background: transparent; margin: 0; padding: 0; }"
        "#findLineEdit { background: transparent; border: none; margin: 0; padding: 0;"
        " color: %1; font-size: %7px;"
        " selection-background-color: %4; selection-color: %5; }"
        "#findCount { background: transparent; margin: 0; padding: 0 2px; color: %2;"
        " font-size: %8px; }"
        "#findPrevBtn, #findNextBtn, #findCloseBtn, #findCaseBtn { margin: 0; padding: 3px;"
        " background: transparent; border: none; border-radius: 4px; }"
        "#findPrevBtn:hover, #findNextBtn:hover, #findCloseBtn:hover, #findCaseBtn:hover"
        " { background: %3; }"
        "#findPrevBtn:disabled, #findNextBtn:disabled { background: transparent; }"
        "#findCaseBtn { color: %6; font-weight: 600; font-size: %8px; }"
        "#findCaseBtn:checked { background: %4; color: %5; }")
        .arg(m_fg.name(),
             (m_noMatches ? m_accent : muted).name(),
             hover.name(),
             m_accent.name(),
             m_accentText.name(),
             muted.name())
        .arg(editPx)
        .arg(smallPx));
}

void FindPopup::paintEvent(QPaintEvent*)
{
    QPainter p(this);
    p.setRenderHint(QPainter::Antialiasing, true);
    // Accent ring while the field is focused (docsFilter's focus language);
    // the no-results state colours the counter, not the ring.
    const QColor pen = m_editFocused ? m_accent : m_border;
    const qreal penW = 2.0;
    const QRectF r = QRectF(rect()).adjusted(penW / 2, penW / 2, -penW / 2, -penW / 2);
    // The shared nav-chip radius (docsFilter, chips — see dpi.h): one visual
    // identity, not a bespoke curve.
    const qreal radius = ScaleHeightForDPI(kRadiusLargeDx);
    p.setPen(QPen(pen, penW));
    p.setBrush(m_bg);
    p.drawRoundedRect(r, radius, radius);
}

bool FindPopup::eventFilter(QObject* obj, QEvent* ev)
{
    if (obj == parentWidget())
    {
        if (ev->type() == QEvent::Resize && isVisible())
            reposition();
        return false;
    }

    if (obj != m_edit)
        return false;

    if (ev->type() == QEvent::FocusIn || ev->type() == QEvent::FocusOut)
    {
        m_editFocused = (ev->type() == QEvent::FocusIn);
        update();
        return false;
    }

    // The app binds Escape and Ctrl+G as window QShortcuts (escapeWorkspaces)
    // and Shift+Return to Link tap-tempo, so claim them here or the field
    // never sees the key (QLineEdit already claims its own editing keys).
    // Qt on macOS maps the Command key to ControlModifier and Control to
    // MetaModifier, so accept either as "ctrl-ish" — emacs muscle memory uses
    // the real Control key on every platform.
    if (ev->type() == QEvent::ShortcutOverride)
    {
        auto* ke = static_cast<QKeyEvent*>(ev);
        const bool ctrlish = ke->modifiers() & (Qt::ControlModifier | Qt::MetaModifier);
        const int k = ke->key();
        if (k == Qt::Key_Escape
            || ((k == Qt::Key_Return || k == Qt::Key_Enter)
                && !(ke->modifiers() & ~Qt::ShiftModifier))
            || (ctrlish && (k == Qt::Key_S || k == Qt::Key_R || k == Qt::Key_G)))
        {
            ev->accept();
            return true;
        }
        return false;
    }

    if (ev->type() == QEvent::KeyPress)
    {
        auto* ke = static_cast<QKeyEvent*>(ev);
        const bool ctrlish = ke->modifiers() & (Qt::ControlModifier | Qt::MetaModifier);
        const bool shift = ke->modifiers() & Qt::ShiftModifier;
        switch (ke->key())
        {
        case Qt::Key_Return:
        case Qt::Key_Enter:
            if (ke->modifiers() & ~Qt::ShiftModifier)
                break;   // Meta+Return (Run) etc. stay global
            // Enter accepts (isearch RET): close, landing the caret on the
            // current match. Cycling lives on ↑/↓, F3 and Ctrl+S/Ctrl+R.
            emit closeRequested(false);
            return true;
        case Qt::Key_F3:
            if (shift) emit prevRequested(); else emit nextRequested();
            return true;
        case Qt::Key_Down:
            emit nextRequested();
            return true;
        case Qt::Key_Up:
            emit prevRequested();
            return true;
        case Qt::Key_Escape:
            emit closeRequested(false);
            return true;
        case Qt::Key_S:
            if (ctrlish) { emit nextRequested(); return true; }   // isearch repeat
            break;
        case Qt::Key_R:
            if (ctrlish) { emit prevRequested(); return true; }   // isearch reverse
            break;
        case Qt::Key_G:
            if (ctrlish) { emit closeRequested(true); return true; }   // isearch abort
            break;
        default:
            break;
        }
        return false;
    }

    return false;
}
