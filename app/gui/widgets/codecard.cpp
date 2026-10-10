//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include "codecard.h"

#include <QAccessible>
#include <QAccessibleWidget>
#include <QApplication>
#include <QDrag>
#include <QGridLayout>
#include <QHBoxLayout>
#include <QKeyEvent>
#include <QLabel>
#include <QMenu>
#include <QMimeData>
#include <QMouseEvent>
#include <QPainter>
#include <QPainterPath>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QRegularExpression>
#include <QScrollArea>
#include <QScrollBar>
#include <QSettings>
#include <QStyle>
#include <QSyntaxHighlighter>
#include <QTextDocumentFragment>
#include <QTextOption>
#include <QTimer>
#include <QtMath>
#include <QVBoxLayout>

#include "dpi.h"
#include "model/sonicpitheme.h"
#include "utils/code_colours.h"
#include "utils/flash_style.h"
#include "utils/gui_settings.h"
#include "utils/tutorialdocs.h"
#include "widgets/cardscope.h"
#include "widgets/tutorialwidgets.h" // TutHeading, TutProseText

namespace
{

void repolish(QWidget* w)
{
    w->style()->unpolish(w);
    w->style()->polish(w);
    w->update();
}

// Card frames read as a named group ("Play a note card, 1 of 8"). A QFrame's
// default accessible role is Border, which the platform bridges prune from
// the tree — the name and the Space/I keyboard hint would never be spoken.
class CodeCardAccessible : public QAccessibleWidget
{
public:
    explicit CodeCardAccessible(QWidget* w)
        : QAccessibleWidget(w, QAccessible::Grouping)
    {
    }
};

// Pointer-only affordances (the drag handle) are pruned from the
// accessibility tree: to a screen reader they are dead stops with no
// keyboard equivalent of their own. Same idiom as the completion popup's
// IgnoredAccessible.
class IgnoredAccessible : public QAccessibleWidget
{
public:
    explicit IgnoredAccessible(QWidget* w)
        : QAccessibleWidget(w, QAccessible::NoRole)
    {
    }
    QAccessible::State state() const override
    {
        QAccessible::State st = QAccessibleWidget::state();
        st.invisible = true;
        st.offscreen = true;
        return st;
    }
    int childCount() const override { return 0; }
    QAccessibleInterface* child(int) const override { return nullptr; }
};

QAccessibleInterface* codeCardAccessibleFactory(const QString& className, QObject* object)
{
    Q_UNUSED(className);
    QWidget* widget = qobject_cast<QWidget*>(object);
    if (!widget)
        return nullptr;
    if (widget->property("a11yIgnored").toBool())
        return new IgnoredAccessible(widget);
    if (widget->objectName() == QLatin1String("qsCard"))
        return new CodeCardAccessible(widget);
    return nullptr;
}

void registerCodeCardAccessibility()
{
    static bool installed = false;
    if (installed)
        return;
    installed = true;
    QAccessible::installFactory(codeCardAccessibleFactory);
    registerTutorialWidgetAccessibility(); // titles are TutHeadings
}

// The editor's colours, from the one tokenizer the card's still lines use
// (TutorialDocs::tokenizeLine): editing a line changes nothing about how it
// is coloured.
class CodeHighlighter : public QSyntaxHighlighter
{
public:
    CodeHighlighter(QTextDocument* doc, const SonicPi::CodeColours& colours)
        : QSyntaxHighlighter(doc), m_colours(colours)
    {
    }

protected:
    void highlightBlock(const QString& text) override
    {
        for (const SonicPi::CodeToken& t : SonicPi::TutorialDocs::tokenizeLine(text))
        {
            const QString& c = colourOf(t.kind);
            if (c.isEmpty())
                continue;
            QTextCharFormat f;
            f.setForeground(QColor(c));
            if (t.kind == SonicPi::CodeTokenKind::Comment)
                f.setFontItalic(true);
            setFormat(t.start, t.length, f);
        }
    }

private:
    const QString& colourOf(SonicPi::CodeTokenKind k) const
    {
        switch (k)
        {
        case SonicPi::CodeTokenKind::Keyword:  return m_colours.keyword;
        case SonicPi::CodeTokenKind::Symbol:   return m_colours.symbol;
        case SonicPi::CodeTokenKind::Number:   return m_colours.number;
        case SonicPi::CodeTokenKind::String:   return m_colours.string;
        case SonicPi::CodeTokenKind::Regex:    return m_colours.regex;
        case SonicPi::CodeTokenKind::Comment:  return m_colours.comment;
        case SonicPi::CodeTokenKind::Def:      return m_colours.def;
        case SonicPi::CodeTokenKind::Ivar:     return m_colours.ivar;
        case SonicPi::CodeTokenKind::Constant: return m_colours.constant;
        }
        return m_colours.keyword;
    }
    SonicPi::CodeColours m_colours;
};

QString rememberedKey(const QString& key) { return QStringLiteral("cards/") + key; }

} // namespace

CodeCard::CodeCard(const Spec& spec, const Metrics& metrics, SonicPiTheme* theme, QWidget* parent)
    : QFrame(parent), m_spec(spec), m_m(metrics), m_theme(theme), m_code(spec.code)
{
    registerCodeCardAccessibility();
    // An edit is kept until it is reset, as the web keeps it (card.js remember).
    if (!m_spec.key.isEmpty())
    {
        const QVariant kept = SonicPi::guiSettings().value(rememberedKey(m_spec.key));
        if (kept.isValid())
            m_code = kept.toString();
    }

    // The card: an accent bar, a tinted body, a deeper-tinted foot, as the
    // printed cheat sheets and the web's card. Chrome is styled in app.qss
    // (#qsCard and friends); only the zoomed font sizes are set here.
    setObjectName(QStringLiteral("qsCard"));
    setProperty("still", !m_spec.runnable);
    if (m_m.width > 0)
        setFixedWidth(m_m.width);
    m_layout = new QVBoxLayout(this);
    m_layout->setContentsMargins(0, 0, 0, 0);
    m_layout->setSpacing(0);

    // Header bar: title on the left, the actions on the right. The buttons are
    // created LAST — after the code and the description — and only PLACED
    // here: assistive technology reads widgets in creation order, so the card
    // must read content first and actions after, even though the buttons
    // render at the top. The grid makes that possible (where a widget lands is
    // independent of when it was made). No vertical margin so the icon buttons
    // fill the bar top to bottom; the outer button's corner is rounded to
    // match the card.
    QGridLayout* headerGrid = new QGridLayout();
    headerGrid->setContentsMargins(0, 0, 0, 0);
    headerGrid->setSpacing(0);
    QWidget* header = new QWidget(this);
    header->setObjectName(QStringLiteral("qsCardHeader"));
    headerGrid->addWidget(header, 0, 0, 1, 9);
    headerGrid->setColumnMinimumWidth(0, uiScale().y(14)); // title padded on the left
    headerGrid->setColumnStretch(2, 1);
    // TutHeading: a real heading for screen-reader rotor navigation from card
    // to card.
    QLabel* heading = new TutHeading(m_spec.title, 2, this);
    heading->setObjectName(QStringLiteral("qsCardTitle"));
    heading->setStyleSheet(QString("font-size: %1px;").arg(m_m.titlePx));
    // Match the widget font to the stylesheet size so the bar height (and thus
    // the button height) is measured correctly; sizeHint uses the widget font.
    {
        QFont hf = heading->font();
        hf.setPixelSize(m_m.titlePx);
        hf.setBold(true);
        heading->setFont(hf);
    }
    headerGrid->addWidget(heading, 0, 1, Qt::AlignVCenter);
    m_layout->addLayout(headerGrid);

    // The body scrolls on both axes rather than clipping a snippet longer or
    // wider than the host's budget; with no budget it is as tall as the code,
    // and a long line scrolls sideways. The body *is* the scroll area rather
    // than holding one, so this adds no node to the accessibility tree.
    m_body = new QScrollArea(this);
    m_body->setObjectName(QStringLiteral("qsCardBody"));
    m_body->setAccessibleName(tr("Code"));
    m_body->setFrameShape(QFrame::NoFrame);
    m_body->setWidgetResizable(true);
    // In a deck no bar ever shows — a bar steals viewport space and can crush
    // the text over a 1px rounding surprise; overflow stays reachable by wheel
    // and flashLine. A card as tall as its code shows a bar for a long line.
    m_body->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    m_body->setHorizontalScrollBarPolicy(m_m.codeBodyHeight > 0 ? Qt::ScrollBarAlwaysOff
                                                                : Qt::ScrollBarAsNeeded);
    // The card owns focus, so the body must not become a tab stop of its own.
    m_body->setFocusPolicy(Qt::NoFocus);
    // Let the body's accent tint show through: a viewport fills its own
    // background by default, which would paint over it.
    m_body->viewport()->setAutoFillBackground(false);
    m_layout->addWidget(m_body);
    renderLines();

    // Footer: the description, an error and what the program puts down the
    // left, Play and Stop on the right.
    const int pad = uiScale().y(14);
    m_footer = new QWidget(this);
    m_footer->setObjectName(QStringLiteral("qsCardFooter"));
    if (m_m.footerHeight > 0)
        m_footer->setFixedHeight(m_m.footerHeight);
    QHBoxLayout* footerLayout = new QHBoxLayout(m_footer);
    footerLayout->setContentsMargins(pad, uiScale().y(8), pad, uiScale().y(8));
    footerLayout->setSpacing(uiScale().y(12));
    QVBoxLayout* words = new QVBoxLayout;
    words->setContentsMargins(0, 0, 0, 0);
    words->setSpacing(uiScale().y(3));

    // Italic description at the code's size. Top-aligned so every card's
    // description starts at the same spot however many lines it wraps to.
    m_blurb = new QLabel(m_spec.blurb, m_footer);
    m_blurb->setObjectName(QStringLiteral("qsCardBlurb"));
    m_blurb->setTextFormat(Qt::RichText);
    // Plain prose for assistive technology (rich-text labels expose markup).
    m_blurb->setAccessibleName(
        QTextDocumentFragment::fromHtml(m_spec.blurb).toPlainText().simplified());
    m_blurb->setWordWrap(true);
    if (m_m.blurbWidth > 0)
        m_blurb->setMaximumWidth(m_m.blurbWidth);
    m_blurb->setStyleSheet(QString("font-size: %1px;").arg(m_m.codeFontPx));
    words->addWidget(m_blurb);

    // The program's error: the line it names and what went wrong, upright,
    // marked in the accent down its left edge.
    m_errorLabel = new QLabel(m_footer);
    m_errorLabel->setObjectName(QStringLiteral("qsCardError"));
    m_errorLabel->setWordWrap(true);
    m_errorLabel->setTextInteractionFlags(Qt::TextSelectableByMouse);
    m_errorLabel->setStyleSheet(QString("font-size: %1px;").arg(m_m.codeFontPx));
    words->addWidget(m_errorLabel);

    // What the program puts: its last few lines, the newest strongest.
    m_outputLabel = new QLabel(m_footer);
    m_outputLabel->setObjectName(QStringLiteral("qsCardOutput"));
    m_outputLabel->setTextFormat(Qt::RichText);
    m_outputLabel->setWordWrap(true);
    m_outputLabel->setAccessibleName(tr("Output"));
    m_outputLabel->setStyleSheet(QString("font-size: %1px;").arg(qMax(1, qRound(m_m.codeFontPx * 0.85))));
    words->addWidget(m_outputLabel);
    words->addStretch(1);
    footerLayout->addLayout(words, 1);

    // The transport: two scope boxes side by side, Play and Stop, each disc
    // with a ring round it — the left channel round Play, the right round
    // Stop. The whole square is the hit box: the rings are inset so they never
    // clip, but the region reads as the control.
    if (m_spec.runnable)
    {
        const int side = m_m.scopeSide > 0 ? m_m.scopeSide : uiScale().y(66);
        QWidget* transport = new QWidget(m_footer);
        QHBoxLayout* tl = new QHBoxLayout(transport);
        tl->setContentsMargins(0, 0, 0, 0);
        tl->setSpacing(0);
        tl->addWidget(makeScopeBox(transport, side, false));
        tl->addWidget(makeScopeBox(transport, side, true));
        footerLayout->addWidget(transport, 0, Qt::AlignVCenter);
        connect(m_play, &QPushButton::clicked, this, [this] {
            // Playing a card also selects it (the click would otherwise leave
            // focus — and the selection ring — on the button, not the card).
            setFocus(Qt::OtherFocusReason);
            emit playRequested();
        });
        connect(m_stop, &QPushButton::clicked, this, [this] {
            setFocus(Qt::OtherFocusReason);
            emit stopRequested();
        });
        refreshDiscs();
        refreshRings();
    }
    m_layout->addWidget(m_footer);
    refreshFooter();

    // The header's action buttons, created after the content so a screen
    // reader meets the card as title → code → description → transport →
    // actions, then placed into the header bar's grid (they render at the top).
    const int btnH = heading->sizeHint().height() + uiScale().y(22);
    const int iconPx = uiScale().y(24);
    const int btnW = iconPx + uiScale().y(14);
    int column = 3;

    if (m_spec.actions & Edit)
    {
        // Edit: the code becomes an editor in place; the pencil stays lit
        // while it is one. Press it again (or Escape) to read it again.
        m_edit = makeButton(TablerIcons::Glyph::Pencil, iconPx, btnW, btnH);
        m_edit->setAccessibleName(tr("Edit %1").arg(m_spec.title));
        m_edit->setToolTip(tr("Change this card's code. Play it to hear your change."));
        connect(m_edit, &QPushButton::clicked, this, [this] { setEditing(!isEditing()); });
        headerGrid->addWidget(m_edit, 0, column++, Qt::AlignVCenter);
    }

    if (m_spec.actions & Reset)
    {
        // Reset: back to the code as written. Only there once it was changed.
        m_reset = makeButton(TablerIcons::Glyph::Reset, iconPx, btnW, btnH);
        m_reset->setAccessibleName(tr("Reset %1 to its code as written").arg(m_spec.title));
        m_reset->setToolTip(tr("Put this card's code back as it was written."));
        m_reset->setVisible(isEdited());
        connect(m_reset, &QPushButton::clicked, this, [this] {
            setCode(m_spec.code);
            if (m_editor)
                setEditing(false);
            emit announceRequested(tr("%1 is back as it was written.").arg(m_spec.title));
            // Heard as well as seen: a playing card plays the code as written.
            if (m_playing)
                emit playRequested();
        });
        headerGrid->addWidget(m_reset, 0, column++, Qt::AlignVCenter);
    }

    if (m_spec.actions & Copy)
    {
        // Copy: the card's code onto the clipboard. The glyph flashes to a
        // tick so the copy visibly registered where it was clicked.
        QPushButton* copy = makeButton(TablerIcons::Glyph::Copy, iconPx, btnW, btnH);
        copy->setAccessibleName(tr("Copy %1 to the clipboard").arg(m_spec.title));
        copy->setToolTip(tr("Copy this card's code to the clipboard."));
        connect(copy, &QPushButton::clicked, this, [this, copy] {
            emit copyRequested(m_spec.title, m_code);
            setButtonGlyph(copy, TablerIcons::Glyph::Check);
            QPointer<QPushButton> alive(copy);
            QTimer::singleShot(1400, this, [this, alive] {
                if (alive) setButtonGlyph(alive, TablerIcons::Glyph::Copy);
            });
        });
        headerGrid->addWidget(copy, 0, column++, Qt::AlignVCenter);
    }

    if (m_spec.actions & Add)
    {
        // Add: drops the card's code into the editor. Hovering projects a live
        // preview into the editor (see hoverAt); the click commits it.
        m_add = makeButton(TablerIcons::Glyph::SquareChevronsUp, iconPx, btnW, btnH);
        m_add->setAccessibleName(tr("Add %1 to the editor at the cursor").arg(m_spec.title));
        m_add->setToolTip(tr("Add this card to your code at the cursor."));
        connect(m_add, &QPushButton::clicked, this,
                [this] { emit insertRequested(m_spec.title, payload()); });
        headerGrid->addWidget(m_add, 0, column++, Qt::AlignVCenter);
    }

    if (m_spec.actions & Open)
    {
        QPushButton* open = makeButton(TablerIcons::Glyph::ExternalLink, iconPx, btnW, btnH);
        open->setAccessibleName(tr("Open %1 in the editor").arg(m_spec.title));
        open->setToolTip(tr("Open this card's code in the editor."));
        connect(open, &QPushButton::clicked, this,
                [this] { emit openRequested(m_spec.title, m_code); });
        headerGrid->addWidget(open, 0, column++, Qt::AlignVCenter);
    }

    if (m_spec.actions & Drag)
    {
        // Drag: an explicit handle at the end of the bar. The whole card is
        // draggable, but this makes it obvious.
        QPushButton* drag = makeButton(TablerIcons::Glyph::Texture, iconPx, btnW, btnH);
        drag->setCursor(Qt::OpenHandCursor);
        drag->setToolTip(tr("Drag me into your editor."));
        // A drag handle only means something to a pointer: keep it out of the
        // Tab ring (it does nothing on Enter) and out of the accessibility tree
        // (the I shortcut, and the Add button where there is one, are its
        // keyboard equivalents).
        drag->setFocusPolicy(Qt::NoFocus);
        drag->setProperty("a11yIgnored", true);
        // Buttons accept the press, so the card's filter never sees it; the
        // handle needs its own filter to start drags.
        drag->installEventFilter(this);
        m_dragHandles.insert(drag);
        headerGrid->addWidget(drag, 0, column++, Qt::AlignVCenter);
        setCursor(Qt::OpenHandCursor);
        header->setCursor(Qt::OpenHandCursor);
    }
    // The bar's last button has the card's rounded corner.
    for (int c = column - 1; c >= 3; --c)
        if (QLayoutItem* item = headerGrid->itemAtPosition(0, c))
            if (QWidget* w = item->widget())
            {
                w->setProperty("corner", "tr");
                break;
            }

    for (QObject* handle : { (QObject*)header, (QObject*)heading, (QObject*)this })
    {
        handle->installEventFilter(this);
        m_dragHandles.insert(handle);
    }

    // Keyboard/screen-reader path: the card itself is focusable; Space plays
    // or stops it, I inserts the code at the editor's cursor, C reads it out,
    // and the context menu offers the same. The name is the host's, which
    // knows the card's place among the others; the code is read from the
    // child widgets.
    setFocusPolicy(Qt::TabFocus);
    if (m_spec.runnable)
        setAccessibleDescription(insertable()
            ? tr("Press Space to play or stop, C to hear the code, I to insert it into the editor.")
            : tr("Press Space to play or stop, C to hear the code."));
    else
        setAccessibleDescription(insertable()
            ? tr("Press C to hear the code, I to insert it into the editor.")
            : tr("Press C to hear the code."));
    setContextMenuPolicy(Qt::CustomContextMenu);
    connect(this, &QWidget::customContextMenuRequested, this, [this](const QPoint& pos) {
        QMenu menu(this);
        QAction* playAct = m_spec.runnable ? menu.addAction(tr("Play")) : nullptr;
        QAction* stopAct = m_spec.runnable && m_playing ? menu.addAction(tr("Stop")) : nullptr;
        QAction* insertAct = insertable() ? menu.addAction(tr("Insert at Cursor in Editor")) : nullptr;
        QAction* editAct = (m_spec.actions & Edit)
            ? menu.addAction(isEditing() ? tr("Done Editing") : tr("Edit")) : nullptr;
        QAction* resetAct = (m_spec.actions & Reset) && isEdited() ? menu.addAction(tr("Reset")) : nullptr;
        QAction* chosen = menu.exec(mapToGlobal(pos));
        if (!chosen)
            return;
        if (chosen == playAct) m_play->click();
        else if (chosen == stopAct) m_stop->click();
        else if (chosen == insertAct) emit insertRequested(m_spec.title, payload());
        else if (chosen == editAct) m_edit->click();
        else if (chosen == resetAct) m_reset->click();
    });
    restyle();
}

QPushButton* CodeCard::makeButton(TablerIcons::Glyph glyph, int iconPx, int w, int h)
{
    QPushButton* b = new QPushButton(this);
    b->setObjectName(QStringLiteral("qsCardBtn"));
    b->setCursor(Qt::PointingHandCursor);
    b->setFixedSize(w, h);
    b->setIconSize(QSize(iconPx, iconPx));
    // Tab reaches the button (the :focus wash is for keyboard users), but a
    // click must not take focus; the wash would linger after the click.
    b->setFocusPolicy(Qt::TabFocus);
    setButtonGlyph(b, glyph);
    return b;
}

// Icon buttons: the accent-contrast glyph on the header bar. Under the
// pointer the button fills with the code-body tint and the glyph flips to a
// dark contrasting ink — strong, legible contrast. Edit while editing is
// armed, as a device's Configure is: the bar's colours turned inside out, so
// a mode the card is in reads from across the room.
void CodeCard::setButtonGlyph(QPushButton* b, TablerIcons::Glyph glyph)
{
    const int px = b->iconSize().width();
    const qreal dpr = devicePixelRatioF();
    const QColor ink = m_theme->contrastingText(m_theme->accentTint());
    const bool lit = b->property("armed").toBool();
    const QColor rest = lit ? m_theme->color("HighlightedBackground") : m_theme->accentContrastText();
    m_iconNormal[b] = TablerIcons::icon(glyph, rest, px, dpr);
    m_iconHover[b] = TablerIcons::icon(glyph, lit ? rest : ink, px, dpr);
    b->setIcon(b == m_hoverButton ? m_iconHover.value(b) : m_iconNormal.value(b));
}

QWidget* CodeCard::makeScopeBox(QWidget* parent, int side, bool stop)
{
    QWidget* box = new QWidget(parent);
    box->setFixedSize(side, side);
    QGridLayout* lay = new QGridLayout(box);
    lay->setContentsMargins(0, 0, 0, 0);
    CardScope* scope = new CardScope(box);
    scope->setFixedSize(side, side);
    scope->setSlot(m_spec.scopeSlot);
    scope->setChannel(stop ? CardScope::Channel::Right : CardScope::Channel::Left);
    lay->addWidget(scope, 0, 0, Qt::AlignCenter);
    QPushButton* b = new QPushButton(box);
    b->setObjectName(QStringLiteral("qsCardRun"));
    b->setCursor(Qt::PointingHandCursor);
    // A QPushButton's vertical size policy is Fixed, so a layout never
    // stretches it to the cell — pin it to the box or the clickable area
    // collapses to a button-height band across the middle.
    b->setFixedSize(side, side);
    const int d = int(side * 0.52); // the disc, inside the ring with a clear gap
    b->setIconSize(QSize(d, d));
    lay->addWidget(b, 0, 0); // overlays the scope and takes the mouse
    if (stop)
    {
        m_stopScope = scope;
        m_stop = b;
        b->setAccessibleName(tr("Stop %1").arg(m_spec.title));
        b->setToolTip(tr("Stop this card."));
        b->setEnabled(false);
    }
    else
    {
        m_playScope = scope;
        m_play = b;
        b->setAccessibleName(tr("Play %1").arg(m_spec.title));
        b->setToolTip(tr("Play this card. Play again while it plays and your change takes over."));
    }
    return box;
}

// A solid accent disc with the glyph cut out in the pane's background; under
// the pointer the disc flips to the foreground. Stop while nothing plays is
// the muted foreground, faint over the footer (the ring's colours are opaque,
// so the fade is mixed in rather than painted at an alpha).
QPixmap CodeCard::disc(bool stop, bool hover, bool enabled, int d) const
{
    const QColor bg = m_theme->color("PaneBackground");
    QColor fill = hover ? m_theme->color("Foreground") : m_theme->color("HighlightedBackground");
    QColor glyph = bg;
    if (!enabled)
    {
        fill = SonicPiTheme::blend(m_theme->accentTintStrong(), m_theme->mutedForeground(), 0.35);
        glyph = SonicPiTheme::blend(fill, bg, 0.7);
    }
    return TablerIcons::transportRing(stop, fill, glyph, d, devicePixelRatioF());
}

void CodeCard::refreshDiscs()
{
    if (!m_play)
        return;
    const int d = m_play->iconSize().width();
    m_iconNormal[m_play] = QIcon(disc(false, false, true, d));
    m_iconHover[m_play] = QIcon(disc(false, true, true, d));
    m_iconNormal[m_stop] = QIcon(disc(true, false, m_stop->isEnabled(), d));
    m_iconHover[m_stop] = QIcon(disc(true, m_stop->isEnabled(), m_stop->isEnabled(), d));
    for (QPushButton* b : { m_play, m_stop })
        b->setIcon(b == m_hoverButton ? m_iconHover.value(b) : m_iconNormal.value(b));
}

// The rings rest faint in the foreground and take the accent once they are
// live, or while the pointer is over their disc.
void CodeCard::refreshRings()
{
    if (!m_playScope)
        return;
    const QColor accent = m_theme->color("HighlightedBackground");
    const QColor fg = m_theme->color("Foreground");
    for (CardScope* s : { m_playScope, m_stopScope })
    {
        const bool lit = (s == m_playScope && m_hoverButton == m_play)
            || (s == m_stopScope && m_hoverButton == m_stop && m_stop->isEnabled());
        if (m_playing || lit)
            s->setColours(accent, accent);
        else
            s->setColours(fg, fg);
        s->setLit(lit);
    }
    m_playScope->setBusy(m_booting, accent);
}

void CodeCard::renderLines()
{
    // The code as it reads: one block of highlighted text, lines unwrapped (a
    // long one scrolls sideways). The block carries the body's padding, the
    // scroll area itself having none.
    if (!m_text)
    {
        const int pad = uiScale().y(14);
        m_codeBlock = new QWidget(m_body);
        QVBoxLayout* codeLayout = new QVBoxLayout(m_codeBlock);
        codeLayout->setContentsMargins(pad, uiScale().y(10), pad, uiScale().y(10));
        codeLayout->setSpacing(0);
        m_text = new TutProseText(m_codeBlock);
        dressCodeText(m_text, m_m.codeFontPx);
        m_text->setAccessibleName(tr("%1, code").arg(m_spec.title));
        if (!m_spec.readable)
        {
            // One stop a card, and its whole face a drag surface.
            m_text->setFocusPolicy(Qt::NoFocus);
            m_text->setAttribute(Qt::WA_TransparentForMouseEvents);
        }
        codeLayout->addWidget(m_text);
        // widgetResizable stretches the block to fill the viewport, so a tail
        // stretch keeps a short snippet packed at the top.
        codeLayout->addStretch(1);
        m_body->setWidget(m_codeBlock);
    }
    m_text->setHtml(SonicPi::TutorialDocs::highlightCode(m_code, SonicPi::codeColours(m_theme)));
    m_text->setMinimumWidth(qCeil(m_text->document()->idealWidth()));
    fitBody();
}

void CodeCard::dressCodeText(TutProseText* text, int codeFontPx)
{
    text->setObjectName(QStringLiteral("qsCode"));
    text->setStyleSheet(QString("font-size: %1px;").arg(codeFontPx));
    QTextOption unwrapped = text->document()->defaultTextOption();
    unwrapped.setWrapMode(QTextOption::NoWrap);
    text->document()->setDefaultTextOption(unwrapped);
}

int CodeCard::codeLinesHeight(int lines, int codeFontPx)
{
    TutProseText probe(nullptr);
    dressCodeText(&probe, codeFontPx);
    probe.ensurePolished();
    QStringList blank;
    for (int i = 0; i < qMax(1, lines); ++i)
        blank << QStringLiteral("&nbsp;");
    probe.setHtml(blank.join(QStringLiteral("<br>")));
    return probe.heightForWidth(QWIDGETSIZE_MAX);
}

// The body's height: the host's budget, or the code's own plus a bar for a
// line too long to show. The editor, when there is one, is fitted the same.
void CodeCard::fitBody()
{
    if (m_m.codeBodyHeight > 0)
    {
        m_body->setFixedHeight(m_m.codeBodyHeight);
        if (m_editor)
            m_editor->setFixedHeight(m_m.codeBodyHeight);
        return;
    }
    if (m_codeBlock)
    {
        const QSize content = m_codeBlock->sizeHint();
        const bool tooWide = content.width() > m_body->viewport()->width() && m_body->viewport()->width() > 0;
        const int bar = tooWide ? m_body->horizontalScrollBar()->sizeHint().height() : 0;
        m_body->setFixedHeight(content.height() + bar);
    }
    if (m_editor)
    {
        const QFontMetrics fm(m_editor->font());
        const int lines = qMax(1, m_editor->document()->blockCount());
        const int frame = 2 * m_editor->frameWidth() + int(2 * m_editor->document()->documentMargin());
        m_editor->setFixedHeight(lines * fm.lineSpacing() + frame + uiScale().y(20));
    }
}

void CodeCard::setCodeBodyHeight(int px)
{
    m_m.codeBodyHeight = px;
    m_body->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    m_body->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    fitBody();
}

void CodeCard::resizeEvent(QResizeEvent* event)
{
    QFrame::resizeEvent(event);
    fitBody();
}

void CodeCard::setEditing(bool editing)
{
    if (editing == isEditing())
        return;
    if (editing)
    {
        m_editor = new QPlainTextEdit(this);
        m_editor->setObjectName(QStringLiteral("qsCardEditor"));
        m_editor->setAccessibleName(tr("%1, code").arg(m_spec.title));
        m_editor->setLineWrapMode(QPlainTextEdit::NoWrap);
        m_editor->setFrameShape(QFrame::NoFrame);
        m_editor->setTabChangesFocus(false);
        QFont f(QStringLiteral("Hack"));
        f.setPixelSize(m_m.codeFontPx);
        m_editor->setFont(f);
        m_editor->setStyleSheet(QString("font-size: %1px;").arg(m_m.codeFontPx));
        m_editor->setPlainText(m_code);
        new CodeHighlighter(m_editor->document(), SonicPi::codeColours(m_theme));
        m_editor->installEventFilter(this);
        connect(m_editor, &QPlainTextEdit::textChanged, this, [this] {
            const bool wasEdited = isEdited();
            m_code = m_editor->toPlainText();
            remember();
            fitBody();
            if (m_reset)
                m_reset->setVisible(isEdited());
            if (wasEdited != isEdited())
                emit editedChanged(isEdited());
        });
        m_layout->replaceWidget(m_body, m_editor);
        m_body->hide();
        fitBody();
        m_editor->setFocus(Qt::OtherFocusReason);
    }
    else
    {
        m_layout->replaceWidget(m_editor, m_body);
        m_editor->deleteLater();
        m_editor = nullptr;
        renderLines();
        m_body->show();
        setFocus(Qt::OtherFocusReason);
    }
    if (m_edit)
    {
        m_edit->setProperty("armed", editing);
        setButtonGlyph(m_edit, TablerIcons::Glyph::Pencil);
        repolish(m_edit);
    }
    setProperty("editing", editing);
    restyle();
}

void CodeCard::setCode(const QString& code)
{
    const bool wasEdited = isEdited();
    m_code = code;
    remember();
    if (m_editor)
    {
        const QSignalBlocker quiet(m_editor);
        m_editor->setPlainText(code);
    }
    else
        renderLines();
    if (m_reset)
        m_reset->setVisible(isEdited());
    if (wasEdited != isEdited())
        emit editedChanged(isEdited());
}

void CodeCard::remember()
{
    if (m_spec.key.isEmpty())
        return;
    QSettings s = SonicPi::guiSettings();
    if (isEdited())
        s.setValue(rememberedKey(m_spec.key), m_code);
    else
        s.remove(rememberedKey(m_spec.key));
}

QSet<QString> CodeCard::loopNames() const
{
    QSet<QString> loops;
    static const QRegularExpression llRe(QStringLiteral("live_loop\\s+[:\"]([A-Za-z0-9_]+)"));
    auto match = llRe.globalMatch(m_code);
    while (match.hasNext())
        loops.insert(match.next().captured(1));
    return loops;
}

void CodeCard::setPlaying(bool playing, SonicPi::SonicPiAPI* api)
{
    // A run again while one plays is still playing: the rings carry on
    // drawing rather than starting over.
    const bool changed = playing != m_playing;
    m_playing = playing;
    if (playing)
        m_booting = false;
    if (m_stop)
    {
        m_stop->setEnabled(playing);
        refreshDiscs();
        refreshRings();
        for (CardScope* s : { m_playScope, m_stopScope })
        {
            if (!changed)
                break;
            if (playing)
                s->start(api);
            else
                s->stop();
        }
    }
    setProperty("playing", playing);
    restyle();
}

void CodeCard::setBooting(bool booting)
{
    m_booting = booting;
    if (booting)
    {
        m_error.clear();
        if (!m_playing)
            m_output.clear();   // a run from rest starts a fresh page of output
        refreshFooter();
    }
    refreshRings();
}

void CodeCard::setError(const QString& message)
{
    m_error = message;
    if (!message.isEmpty())
        m_booting = false;
    setProperty("errored", !message.isEmpty());
    refreshFooter();
    refreshRings();
    restyle();
}

void CodeCard::appendOutput(const QString& line)
{
    m_output << line;
    while (m_output.size() > kOutputLines)
        m_output.removeFirst();
    refreshFooter();
}

void CodeCard::clearOutput()
{
    m_output.clear();
    refreshFooter();
}

void CodeCard::refreshFooter()
{
    m_blurb->setVisible(!m_spec.blurb.isEmpty());
    m_errorLabel->setText(m_error);
    m_errorLabel->setVisible(!m_error.isEmpty());
    QStringList html;
    for (int i = 0; i < m_output.size(); ++i)
    {
        const QString text = m_output[i].toHtmlEscaped();
        html << (i == m_output.size() - 1 ? QStringLiteral("<span style=\"color:%1;\">%2</span>")
                                                .arg(m_theme->color("Foreground").name(), text)
                                          : text);
    }
    m_outputLabel->setText(html.join("<br>"));
    m_outputLabel->setVisible(!m_output.isEmpty());
    // A card with nothing to say and nothing to play has no foot.
    m_footer->setVisible(m_spec.runnable || !m_spec.blurb.isEmpty() || !m_error.isEmpty()
                         || !m_output.isEmpty());
}

void CodeCard::flashLine(int runLine)
{
    const int line = runLine - 2;
    if (line < 0 || line >= m_code.count(QLatin1Char('\n')) + 1 || m_editor)
        return;
    // A snippet that overruns the card's budget scrolls, so follow the run and
    // keep the lit line in view.
    const int lineHeight = QFontMetrics(m_text->font()).lineSpacing();
    m_body->ensureVisible(0, m_text->y() + line * lineHeight + lineHeight / 2, 0, lineHeight);
    QColor wash = m_theme->color("HighlightedBackground");
    wash.setAlpha(SonicPi::kFlashWashAlpha);
    m_text->setWash(line, wash);
    // Each flash holds its own line: a later one on another line isn't cut
    // short by this one's clearing.
    QPointer<TutProseText> text(m_text);
    QTimer::singleShot(SonicPi::kFlashHoldMs, this, [text, line] {
        if (text && text->washedLine() == line)
            text->setWash(-1, QColor());
    });
}

void CodeCard::showHoverIcon(QPushButton* b)
{
    if (b == m_hoverButton)
        return;
    QPushButton* was = m_hoverButton;
    m_hoverButton = b;
    if (was)
    {
        was->setIcon(m_iconNormal.value(was));
        if (was == m_add) // leaving Add: revert its preview
            emit insertPreviewCleared();
    }
    if (b)
    {
        b->setIcon(m_iconHover.value(b));
        if (b == m_add) // entering Add: project the code
            emit insertPreviewRequested(m_spec.title, payload());
    }
    if (was == m_play || was == m_stop || b == m_play || b == m_stop)
        refreshRings();
}

void CodeCard::hoverAt(const QPoint& gp, bool eligible)
{
    // The border lights while the pointer is anywhere over a card that plays.
    // Only a meaningfully-visible card lights: layout rounding can leave a
    // sliver of an off-page card at a viewport edge, and its border would
    // paint there.
    bool over = false;
    if (eligible && isVisible())
    {
        const QPoint lp = mapFromGlobal(gp);
        if (rect().contains(lp)) // cheap test before the region maths
        {
            const QRect vis = visibleRegion().boundingRect();
            over = vis.width() > uiScale().y(8) && vis.contains(lp);
        }
    }
    if (over != m_hover)
    {
        m_hover = over;
        setProperty("cardHover", over && m_spec.runnable);
        restyle();
    }

    QPushButton* under = nullptr;
    if (over)
    {
        for (auto it = m_iconHover.constBegin(); it != m_iconHover.constEnd(); ++it)
        {
            QPushButton* b = it.key();
            if (!b->isVisible() || !b->isEnabled())
                continue;
            const QPoint lp = b->mapFromGlobal(gp);
            if (b->rect().contains(lp) && b->visibleRegion().contains(lp))
            {
                under = b;
                break;
            }
        }
    }
    showHoverIcon(under);
}

void CodeCard::restyle()
{
    repolish(this);
}

QPixmap CodeCard::dragPixmap() const
{
    // Paint in device pixels; set the DPR at the end.
    const qreal dpr = devicePixelRatioF();
    const QPixmap code = m_body->grab(); // highlighted code area
    const int cwD = code.width();
    const int chD = code.height();

    const QColor accent = m_theme->color("HighlightedBackground");
    const QColor cardBg = m_theme->accentTint();

    QFont titleFont(QStringLiteral("Hack"));
    titleFont.setPixelSize(qRound(uiScale().font(FontRole::Base) * dpr));
    titleFont.setBold(true);
    const QFontMetrics fm(titleFont);
    const int titleHD = fm.height();
    const int textWD = fm.horizontalAdvance(m_spec.title);

    const int strokeD = qMax(1, qRound(2 * dpr));
    const int radiusD = qRound(uiScale().y(8) * dpr);
    const int cpadD = qRound(uiScale().y(12) * dpr);
    const int titlePadD = qRound(uiScale().y(6) * dpr);
    const int borderTopD = titleHD / 2;
    const int titleX = strokeD + cpadD;
    const int WD = strokeD + cpadD + qMax(cwD, textWD + 2 * titlePadD) + cpadD + strokeD;
    const int HD = borderTopD + cpadD + chD + cpadD + strokeD;

    QPixmap pm(WD, HD);
    pm.fill(Qt::transparent);
    QPainter p(&pm);
    p.setRenderHint(QPainter::Antialiasing);
    p.setRenderHint(QPainter::SmoothPixmapTransform);

    // Bordered card body.
    QPainterPath path;
    path.addRoundedRect(QRectF(strokeD / 2.0, borderTopD, WD - strokeD,
                               HD - borderTopD - strokeD / 2.0),
                        radiusD, radiusD);
    p.fillPath(path, cardBg);
    p.setPen(QPen(accent, strokeD));
    p.drawPath(path);

    p.drawPixmap(QRect(titleX, borderTopD + cpadD, cwD, chD), code);

    // Title straddling the top border: clear the border behind it, then draw.
    p.fillRect(QRectF(titleX - titlePadD, 0, textWD + 2 * titlePadD, titleHD), cardBg);
    p.setPen(accent);
    p.setFont(titleFont);
    p.drawText(QRectF(titleX, 0, textWD, titleHD), Qt::AlignVCenter | Qt::AlignLeft, m_spec.title);
    p.end();
    pm.setDevicePixelRatio(dpr);
    return pm;
}

void CodeCard::startDrag()
{
    QDrag* drag = new QDrag(this);
    QMimeData* mime = new QMimeData;
    mime->setText(payload());
    mime->setData(QStringLiteral("application/x-sonic-pi-card-title"), m_spec.title.toUtf8());
    drag->setMimeData(mime);
    QPixmap flat = dragPixmap().scaledToWidth(
        qRound(ScaleHeightForDPI(240) * devicePixelRatioF()), Qt::SmoothTransformation);
    QTransform tilt;
    tilt.rotate(-5);
    QPixmap pm = flat.transformed(tilt, Qt::SmoothTransformation);
    drag->setPixmap(pm);
    drag->setHotSpot(QPoint(pm.width() / 2, ScaleHeightForDPI(16)));
    drag->exec(Qt::CopyAction);
    emit dragEnded();
}

bool CodeCard::eventFilter(QObject* obj, QEvent* event)
{
    // The editor: Mod-Enter plays, Mod-. stops, Escape reads the code again.
    if (obj == m_editor && event->type() == QEvent::KeyPress)
    {
        QKeyEvent* ke = static_cast<QKeyEvent*>(event);
        const bool mod = ke->modifiers() & Qt::ControlModifier;   // Cmd on macOS
        if (mod && (ke->key() == Qt::Key_Return || ke->key() == Qt::Key_Enter) && m_play)
        {
            emit playRequested();
            return true;
        }
        if (mod && ke->key() == Qt::Key_Period && m_stop)
        {
            emit stopRequested();
            return true;
        }
        if (ke->key() == Qt::Key_Escape)
        {
            setEditing(false);
            return true;
        }
        return false;
    }

    if (!m_dragHandles.contains(obj))
        return QFrame::eventFilter(obj, event);

    if (event->type() == QEvent::KeyPress && obj == this)
    {
        QKeyEvent* ke = static_cast<QKeyEvent*>(event);
        if ((ke->key() == Qt::Key_Space || ke->key() == Qt::Key_Return) && m_play)
        {
            if (m_playing)
                emit stopRequested();
            else
                emit playRequested();
            return true;
        }
        if (ke->key() == Qt::Key_I && insertable())
        {
            emit insertRequested(m_spec.title, payload());
            return true;
        }
        if (ke->key() == Qt::Key_C && ke->modifiers() == Qt::NoModifier)
        {
            emit announceRequested(tr("Code: %1").arg(m_code.trimmed()));
            return true;
        }
        if (ke->key() == Qt::Key_Left || ke->key() == Qt::Key_Right)
        {
            emit stepRequested(ke->key() == Qt::Key_Right ? 1 : -1);
            return true;
        }
    }

    if (event->type() == QEvent::MouseButtonPress)
    {
        QMouseEvent* me = static_cast<QMouseEvent*>(event);
        if (me->button() == Qt::LeftButton)
        {
            m_pressPos = me->pos();
            m_pressGlobal = me->globalPosition().toPoint();
            m_dragSource = obj;
        }
    }
    else if (event->type() == QEvent::MouseMove && obj == m_dragSource && (m_spec.actions & Drag))
    {
        QMouseEvent* me = static_cast<QMouseEvent*>(event);
        if ((me->buttons() & Qt::LeftButton)
            && (me->pos() - m_pressPos).manhattanLength() >= QApplication::startDragDistance())
        {
            m_dragSource = nullptr; // it's a drag, not a click
            startDrag();
            return true;
        }
    }
    else if (event->type() == QEvent::MouseButtonRelease && obj == m_dragSource)
    {
        m_dragSource = nullptr;
        const QPoint rel = static_cast<QMouseEvent*>(event)->globalPosition().toPoint();
        if ((rel - m_pressGlobal).manhattanLength() < QApplication::startDragDistance())
        {
            setFocus(Qt::OtherFocusReason);
            emit clicked();
        }
    }
    return QFrame::eventFilter(obj, event);
}
