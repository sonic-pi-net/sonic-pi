//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#include "quickstartpane.h"

#include <QApplication>
#include <QCursor>
#include <QDateTime>
#include <QFile>
#include <QFileInfo>
#include <QRegularExpression>
#include <QDrag>
#include <QFrame>
#include <QGridLayout>
#include <QIcon>
#include <QFontMetrics>
#include <QFontMetricsF>
#include <QPainter>
#include <QPainterPath>
#include <QPen>
#include <QTransform>
#include <QHBoxLayout>
#include <QKeyEvent>
#include <QLabel>
#include <QMenu>
#include <QMimeData>
#include <QMouseEvent>
#include <QWheelEvent>
#include <QPropertyAnimation>
#include <QVariantAnimation>
#include <QAccessible>
#include <QAccessibleWidget>
#include <QPushButton>
#include <QTextDocumentFragment>
#include <QScrollArea>
#include <QScrollBar>
#include <QStyle>
#include <QTimer>
#include <QVBoxLayout>

#include <QPointer>

#include <cmath>

#include <QSvgRenderer>

#include "dpi.h"
#include "utils/fontroles.h"
#include "model/sonicpitheme.h"
#include "utils/tablericons.h"
#include "widgets/tutorialwidgets.h" // TutHeading (accessible section titles)
#include "widgets/tutscope.h" // ScopeSampler: the shared SHM reader/poll base
#include "widgets/zoombar.h"
#include "utils/flash_style.h"
#include "utils/reducedmotion.h"
#include "utils/tutorialdocs.h"

#include "api/sonicpi_api.h"

namespace
{
// Re-evaluate a widget's stylesheet after a dynamic property change so
// property-driven rules (e.g. [cardHover="true"]) take effect.
void repolish(QWidget* w)
{
    w->style()->unpolish(w);
    w->style()->polish(w);
    w->update();
}
} // namespace

// Circular stereo mini scope for a playing card: the waveform wrapped
// around a ring (left channel on the outer ring, right on the inner)
// radius modulated by amplitude. No panel or box: just the rings, so an
// idle card shows nothing at all. Decorative (mouse-transparent, no
// focus, no accessible role; the Run/Stop button conveys running state).
class CardScope : public ScopeSampler
{
public:
    explicit CardScope(QWidget* parent = nullptr)
        : ScopeSampler(parent)
    {
        // The scope is the play/stop control (an overlaid button fills it), so
        // it must accept mouse events (no WA_TransparentForMouseEvents here).
        setFocusPolicy(Qt::NoFocus);
    }

    void setColours(const QColor& outer, const QColor& inner)
    {
        m_outer = outer;
        m_inner = inner;
        update();
    }

    // Hover lights idle rings at full strength (the resting fade says "not
    // playing"; hover says "click me", matching the glyph's hover colour).
    void setLit(bool lit)
    {
        m_lit = lit;
        update();
    }

    // The scope-buffer slot this card taps (its own, isolated from others).
    void setSlot(unsigned int slot) { m_slot = slot; }

    void start(SonicPi::SonicPiAPI* api)
    {
        m_left.clear();
        m_right.clear();
        m_active = true;
        startSampling(api, m_slot);
        update();
    }

    // Stays visible when stopped: the rings settle back to faint circles.
    void stop()
    {
        stopSampling();
        m_left.clear();
        m_right.clear();
        m_active = false;
        update();
    }

protected:
    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing);
        const qreal side = qMin(width(), height());
        const QPointF centre(width() / 2.0, height() / 2.0);

        auto ring = [&](const std::vector<float>& samples, qreal baseR, QColor colour) {
            const qreal radius = baseR * side;
            const qreal amp = 0.04 * side; // rings kept clear of the centre play icon
            if (!m_active && !m_lit)
                colour.setAlphaF(0.22);
            QPen pen(colour, side * 0.028, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);
            p.setPen(pen);
            p.setBrush(Qt::NoBrush);
            if (samples.size() < 8)
            {
                p.drawEllipse(centre, radius, radius);
                return;
            }
            QPainterPath path;
            const size_t n = samples.size();
            for (size_t i = 0; i <= n; i++)
            {
                const qreal theta = (qreal)(i % n) / n * 2.0 * M_PI;
                const qreal v = qBound(-1.0, (double)samples[i % n], 1.0);
                const qreal r = radius + v * amp;
                const QPointF pt(centre.x() + r * std::cos(theta),
                                 centre.y() + r * std::sin(theta));
                if (i == 0)
                    path.moveTo(pt);
                else
                    path.lineTo(pt);
            }
            p.drawPath(path);
        };
        ring(m_left, 0.42, m_outer);
        ring(m_right, 0.30, m_inner);
    }

    // One tap block's worth of the newest audible audio per revolution.
    unsigned int windowFrames() const override { return 1024; }

    void storeWindow(const float* interleaved, unsigned int frames,
                     unsigned int ch) override
    {
        m_left.resize(frames);
        m_right.resize(frames);
        for (unsigned int i = 0; i < frames; i++)
        {
            m_left[i] = interleaved[(size_t)i * ch];
            m_right[i] = interleaved[(size_t)i * ch + (ch - 1)];
        }
    }

private:
    QColor m_outer;
    QColor m_inner;
    std::vector<float> m_left;
    std::vector<float> m_right;
    bool m_active = false;
    bool m_lit = false;
    unsigned int m_slot = 0;
};


namespace
{
struct Deck
{
    QString title;
    QString description; // one-line intro shown at the top of the deck
    QVector<SonicPi::QuickstartCard> cards;
};
} // namespace

// Parse the quickstart cards file (etc/quickstart/cards.txt) into decks.
// Format: "# Deck: name" starts a deck; the text before its first card is the
// deck's description; "## title" starts a card, the lines after are its blurb,
// and a ``` fenced block is the code. Forgiving, so it is easy to hand-author.
static QVector<Deck> parseDecks(const QString& text)
{
    QVector<Deck> decks;
    SonicPi::QuickstartCard* card = nullptr;
    bool inCode = false;
    const QStringList lines = text.split('\n');
    for (const QString& line : lines)
    {
        const QString trimmed = line.trimmed();
        if (inCode)
        {
            if (trimmed == QLatin1String("```")) { inCode = false; continue; }
            if (card)
            {
                if (!card->code.isEmpty()) card->code += '\n';
                card->code += line; // keep indentation verbatim
            }
            continue;
        }
        if (line.startsWith(QLatin1String("# Deck:")))
        {
            Deck d;
            d.title = line.mid(7).trimmed();
            decks.append(d);
            card = nullptr;
            continue;
        }
        if (decks.isEmpty()) continue;
        if (line.startsWith(QLatin1String("## ")))
        {
            SonicPi::QuickstartCard c;
            c.title = line.mid(3).trimmed();
            decks.last().cards.append(c);
            card = &decks.last().cards.last();
            continue;
        }
        if (trimmed == QLatin1String("```")) { inCode = true; continue; }
        if (trimmed.startsWith(QLatin1Char('#')) || trimmed.isEmpty()) continue; // comment/blank
        if (card) // blurb line(s)
        {
            if (!card->blurb.isEmpty()) card->blurb += ' ';
            card->blurb += trimmed;
        }
        else // text before the first card is the deck description
        {
            if (!decks.last().description.isEmpty()) decks.last().description += ' ';
            decks.last().description += trimmed;
        }
    }
    return decks;
}

// A tiny built-in deck, used only if the cards file is missing or empty so
// the pane never comes up blank.
static QVector<Deck> fallbackDecks()
{
    Deck d;
    d.title = QStringLiteral("Play");
    d.description = QStringLiteral("A few first sounds to get you started.");
    d.cards.append({ QStringLiteral("Your First Note"),
                     QStringLiteral("One line, one note."), QStringLiteral("play 60") });
    d.cards.append({ QStringLiteral("A Beat"), QStringLiteral("A drum break in one line."),
                     QStringLiteral("sample :loop_amen") });
    return { d };
}

bool QuickstartPane::validateCardsFile(const QString& path, QString* error)
{
    QFile f(path);
    if (!f.open(QFile::ReadOnly | QFile::Text))
    {
        if (error)
            *error = tr("The file could not be opened for reading.");
        return false;
    }
    const QVector<Deck> decks = parseDecks(QString::fromUtf8(f.readAll()));
    if (decks.isEmpty())
    {
        if (error)
            *error = tr("No card decks were found. A card set needs at least one "
                        "\"# Deck: name\" line followed by \"## Card title\" cards.");
        return false;
    }
    return true;
}

// Load + cache the decks from `path`. Re-read when the path OR the file's
// modification time changes, so editing the file and reloading picks up edits.
static const QVector<Deck>& quickstartDecks(const QString& path)
{
    static QString cachedPath;
    static QDateTime cachedMtime;
    static QVector<Deck> cached;
    const QDateTime mtime = QFileInfo(path).lastModified();
    if (!cached.isEmpty() && path == cachedPath && mtime == cachedMtime)
        return cached;
    cachedPath = path;
    cachedMtime = mtime;
    cached.clear();
    QFile f(path);
    if (f.open(QFile::ReadOnly | QFile::Text))
        cached = parseDecks(QString::fromUtf8(f.readAll()));
    if (cached.isEmpty()) cached = fallbackDecks();
    return cached;
}
namespace
{

// Card frames read as a named group ("Play a note card, 1 of 8"). A QFrame's
// default accessible role is Border, which the platform bridges prune from
// the tree — the name and the Space/I keyboard hint would never be spoken.
class QsCardAccessible : public QAccessibleWidget
{
public:
    explicit QsCardAccessible(QWidget* w)
        : QAccessibleWidget(w, QAccessible::Grouping)
    {
    }
};

// Pointer-only affordances (the drag handle) are pruned from the
// accessibility tree: to a screen reader they are dead stops with no
// keyboard equivalent of their own. Same idiom as the completion popup's
// IgnoredAccessible.
class QsIgnoredAccessible : public QAccessibleWidget
{
public:
    explicit QsIgnoredAccessible(QWidget* w)
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

QAccessibleInterface* quickstartAccessibleFactory(const QString& className, QObject* object)
{
    Q_UNUSED(className);
    QWidget* widget = qobject_cast<QWidget*>(object);
    if (!widget)
        return nullptr;
    if (widget->property("a11yIgnored").toBool())
        return new QsIgnoredAccessible(widget);
    if (widget->objectName() == QLatin1String("qsCard"))
        return new QsCardAccessible(widget);
    return nullptr;
}

void registerQuickstartAccessibility()
{
    static bool installed = false;
    if (installed)
        return;
    installed = true;
    QAccessible::installFactory(quickstartAccessibleFactory);
}

} // namespace

QuickstartPane::QuickstartPane(SonicPiTheme* theme, QWidget* parent)
    : QWidget(parent), m_theme(theme)
{
    registerQuickstartAccessibility();
    registerTutorialWidgetAccessibility(); // card titles are TutHeadings
    setAttribute(Qt::WA_StyledBackground, true);
    // Deck switcher runs down the left as a side column, so it costs no
    // vertical space and more card rows are visible.
    QHBoxLayout* layout = new QHBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    // Poll the pointer for hover feedback instead of tracking Enter/Leave
    // across every child widget (which sticks and flickers). Runs only while
    // the pane is shown (see showEvent/hideEvent) so a hidden Cards tab costs
    // nothing.
    m_hoverTimer = new QTimer(this);
    m_hoverTimer->setInterval(50);
    connect(m_hoverTimer, &QTimer::timeout, this, &QuickstartPane::updateHover);

    rebuild();
}

void QuickstartPane::showEvent(QShowEvent* event)
{
    QWidget::showEvent(event);
    if (m_themeDirty)
    {
        m_themeDirty = false;
        rebuild();
    }
    m_hoverTimer->start();
}

void QuickstartPane::hideEvent(QHideEvent* event)
{
    QWidget::hideEvent(event);
    m_hoverTimer->stop();
    // Don't leave a hover state stuck while hidden.
    if (m_hoverCard)
        setCardHover(m_hoverCard, false);
    m_hoverCard = nullptr;
    if (m_hoverIcon && m_addCode.contains(m_hoverIcon))
        emit insertPreviewCleared();
    m_hoverIcon = nullptr;
}

void QuickstartPane::applyTheme()
{
    // The deck rebuild re-creates every card widget, so it's by far the
    // heaviest single step of a whole-app re-theme. A hidden Cards tab can
    // take it lazily on next show instead of on every theme/hue change.
    if (isVisible())
        rebuild();
    else
        m_themeDirty = true;
    if (m_zoomBar)
        m_zoomBar->applyTheme();
}

QWidget* QuickstartPane::zoomControls()
{
    if (!m_zoomBar)
    {
        m_zoomBar = new ZoomBar(m_theme, tr("quickstart"), this);
        connect(m_zoomBar, &ZoomBar::zoomStep, this,
                [this](int delta) { setUserZoom(m_userZoom + delta); });
    }
    return m_zoomBar;
}

int QuickstartPane::preferredDockHeight() const
{
    // Start tall enough for the deck bar, the description and one full card.
    int chrome = uiScale().y(24); // grid margins around the card
    if (m_topBar)
        chrome += m_topBar->sizeHint().height();
    if (m_navRow)
        chrome += m_navRow->sizeHint().height();
    if (m_deckDesc)
    {
        QFont descFont;
        descFont.setPixelSize(fontPx(15));
        chrome += 2 * QFontMetrics(descFont).height() + uiScale().y(12); // two lines
    }
    return m_cardHeight + chrome;
}

int QuickstartPane::cardWidth() const
{
    // Sized so kMaxCols monospace columns fit without shrinking the code font
    // (grown in step with kMaxCols: 41 cols needs ~441).
    return qRound(ScaleHeightForDPI(441) * m_zoomFactor);
}

void QuickstartPane::computeGlobalLayout()
{
    if (m_codeBodyH > 0 && m_layoutZoom == m_userZoom)
        return; // only the (width-independent) zoom invalidates these
    m_layoutZoom = m_userZoom;

    // The card is a fixed-size format; content is authored to fit these
    // budgets rather than the card growing to fit content.
    const int kMaxCols = 41;   // widest code line a card is designed to hold
    const int kCodeLines = 6;  // tallest snippet a card is designed to hold
    const int kBlurbLines = 2; // blurb runs full width, so it wraps in fewer lines

    const int pad = uiScale().y(14);
    const int codeAvail = cardWidth() - 2 * pad;
    m_blurbW = codeAvail; // the description now spans the full card width

    // Code font: the requested (zoomed) size, capped so kMaxCols monospace
    // columns fit the card width. An authored line wider than that is still
    // reachable (the body scrolls), but sizing to the budget is what keeps
    // scrolling the exception rather than the reading experience.
    int codePx = qMax(8, qRound(ScaleHeightForDPI(15) * m_zoomFactor));
    QFont hack(QStringLiteral("Hack"));
    hack.setPixelSize(codePx);
    const qreal colW = QFontMetricsF(hack).horizontalAdvance(QLatin1Char('m'));
    if (colW * kMaxCols > codeAvail)
        // Floor stays unscaled: it is the "fit an authored line" backstop, so
        // letting it rise with zoom would defeat the shrink-to-fit it guards.
        codePx = qMax(ScaleHeightForDPI(9), int(codePx * codeAvail / (colW * kMaxCols)));
    m_codeFontPx = codePx;

    hack.setPixelSize(m_codeFontPx);
    const int lineH = QFontMetrics(hack).height();
    m_codeBodyH = 2 * uiScale().y(10) + kCodeLines * lineH;

    // Footer: a full-height scope on the left, and to its right the
    // description (two lines) over the Run/Add buttons.
    QFont sans;
    sans.setPixelSize(m_codeFontPx); // the blurb renders at the code's size
    const int blurbLineH = QFontMetrics(sans).height();
    // The footer holds the description (two lines) and, on the right, the scope
    // which is the play/stop control. The scope is inset from the footer's
    // inner height so its rings never touch (get clipped by) the edges.
    const int innerH = kBlurbLines * blurbLineH + uiScale().y(58);
    m_footerH = 2 * uiScale().y(8) + innerH;
    m_scopeSide = innerH - uiScale().y(16);
    // The blurb yields the full hit-box width (scope + its surrounding
    // margin), not just the scope square — see the scopeHit box in addCard.
    m_blurbW = codeAvail - innerH - uiScale().y(12);
}

void QuickstartPane::setCardHover(QWidget* frame, bool on)
{
    if (!m_cardFx.contains(frame))
        return;
    frame->setProperty("cardHover", on);
    repolish(frame);
}

void QuickstartPane::updateHover()
{
    const QPoint gp = QCursor::pos();
    // Short-circuit the common case: pointer nowhere near the pane. The cheap
    // geometric test can't see occlusion (a completion popup or dialog over the
    // dock), so once it passes, confirm via widgetAt that the pane really is
    // what's under the pointer; hover must never fire beneath another window.
    bool onPane = isVisible() && window()->isActiveWindow()
        && rect().contains(mapFromGlobal(gp));
    if (onPane)
    {
        QWidget* under = QApplication::widgetAt(gp);
        onPane = under && (under == this || isAncestorOf(under));
    }

    QWidget* card = nullptr;
    if (onPane)
    {
        for (auto it = m_cardFx.constBegin(); it != m_cardFx.constEnd(); ++it)
        {
            QWidget* f = it.key();
            if (!f || !f->isVisible())
                continue;
            const QPoint lp = f->mapFromGlobal(gp);
            if (!f->rect().contains(lp)) // cheap test before the region maths
                continue;
            // Only a meaningfully-visible card can hover-activate: layout
            // rounding can leave a 1-2px sliver of an off-page card visible at
            // the viewport edge, and its hover border would paint there.
            const QRect vis = f->visibleRegion().boundingRect();
            if (vis.width() > uiScale().y(8) && vis.contains(lp))
            {
                card = f;
                break;
            }
        }
    }
    if (card != m_hoverCard)
    {
        if (m_hoverCard)
            setCardHover(m_hoverCard, false);
        m_hoverCard = card;
        if (m_hoverCard)
            setCardHover(m_hoverCard, true);
    }

    // Icon-button glyph swap for the card play/add/drag buttons. (The dock-row
    // zoom controls handle their own hover in eventFilter, being off the pane.)
    QPushButton* icon = nullptr;
    if (onPane)
    {
        for (auto it = m_iconHover.constBegin(); it != m_iconHover.constEnd(); ++it)
        {
            QPushButton* b = qobject_cast<QPushButton*>(it.key());
            if (!b || !b->isVisible())
                continue;
            const QPoint lp = b->mapFromGlobal(gp);
            if (!b->rect().contains(lp)) // cheap test before the region maths
                continue;
            if (b->visibleRegion().contains(lp))
            {
                icon = b;
                break;
            }
        }
    }
    if (icon != m_hoverIcon)
    {
        if (m_hoverIcon)
        {
            if (m_iconNormal.contains(m_hoverIcon))
                m_hoverIcon->setIcon(m_iconNormal.value(m_hoverIcon));
            if (m_addCode.contains(m_hoverIcon)) // leaving an add button: revert its preview
                emit insertPreviewCleared();
            setScopeHover(m_hoverIcon, false); // run button: settle its rings
        }
        m_hoverIcon = icon;
        if (m_hoverIcon)
        {
            if (m_iconHover.contains(m_hoverIcon))
                m_hoverIcon->setIcon(m_iconHover.value(m_hoverIcon));
            if (m_addCode.contains(m_hoverIcon)) // entering an add button: project the code
                emit insertPreviewRequested(m_addTitle.value(m_hoverIcon),
                                            m_addCode.value(m_hoverIcon));
            setScopeHover(m_hoverIcon, true); // run button: rings follow the glyph
        }
    }
}

bool QuickstartPane::eventFilter(QObject* obj, QEvent* event)
{
    // The viewport's Resize is the only reliable "the layout gave me my real
    // size" signal: rebuild() runs while a fresh scroll area is still at its
    // 100px default, and even a singleShot(0) can lose the race with the
    // posted layout pass.
    if (event->type() == QEvent::Resize && m_scroll && obj == m_scroll->viewport())
        updateCarousel();
    // The invisible left-margin strip pages back (see updateBackEdge for its
    // enabled/cursor state).
    if (obj == m_backEdge && event->type() == QEvent::MouseButtonPress
        && m_backEdge->isEnabled()
        && static_cast<QMouseEvent*>(event)->button() == Qt::LeftButton)
    {
        goToPage(m_pageIndex - 1);
        return true;
    }
    // A card's code body only claims the wheel along an axis it can actually
    // scroll; otherwise the gesture falls through to the carousel below, so a
    // snippet that fits (almost all of them) still pages the deck from
    // anywhere on the card.
    if (event->type() == QEvent::Wheel && m_scroll)
    {
        if (QWidget* vp = qobject_cast<QWidget*>(obj))
        {
            QScrollArea* sa = qobject_cast<QScrollArea*>(vp->parentWidget());
            if (sa && sa->objectName() == QLatin1String("qsCardBody")
                && vp == sa->viewport())
            {
                QWheelEvent* we = static_cast<QWheelEvent*>(event);
                const bool horizontal
                    = qAbs(we->angleDelta().x()) > qAbs(we->angleDelta().y());
                const QScrollBar* bar
                    = horizontal ? sa->horizontalScrollBar() : sa->verticalScrollBar();
                if (bar && bar->minimum() != bar->maximum())
                    return false; // the body scrolls it
                // Nothing to scroll along that axis: re-aim at the carousel.
                obj = m_scroll;
            }
        }
    }
    // Side-scrolling pages the carousel one card at a time (never free-scrolls).
    // A cooldown absorbs a trackpad swipe's momentum so one gesture = one card.
    if (event->type() == QEvent::Wheel && m_scroll
        && (obj == m_scroll || obj == m_scroll->viewport()))
    {
        QWheelEvent* we = static_cast<QWheelEvent*>(event);
        const int dx = we->angleDelta().x();
        const int dy = we->angleDelta().y();
        bool act = true, next = false;
        // angleDelta folds in the system's natural-scrolling setting, so read it
        // as a scrollbar delta (same as dy below): negative x scrolls the content
        // rightward; i.e. a macOS natural swipe left advances to the next cards.
        if (qAbs(dx) > qAbs(dy) && qAbs(dx) > 2)
            next = dx < 0;
        else if (qAbs(dy) > 2)
            next = dy < 0; // vertical wheel: down = forward
        else
            act = false;
        if (act && !m_wheelCooldown)
        {
            goToPage(m_pageIndex + (next ? 1 : -1));
            m_wheelCooldown = true;
            QTimer::singleShot(320, this, [this] { m_wheelCooldown = false; });
        }
        return true;
    }
    // Left/Right with focus on the scroll area drop the selection onto the
    // leading visible card; from there each press moves the selection one
    // card at a time (the selected-card branch below) and the deck scrolls
    // to keep it in view — the nav bar follows the selection, not the other
    // way round.
    if (event->type() == QEvent::KeyPress && m_scroll
        && (obj == m_scroll || obj == m_scroll->viewport()))
    {
        const int key = static_cast<QKeyEvent*>(event)->key();
        if (key == Qt::Key_Left || key == Qt::Key_Right)
        {
            if (QWidget* card = firstVisibleCard())
            {
                card->setFocus(Qt::OtherFocusReason);
                return true;
            }
            goToPage(m_pageIndex + (key == Qt::Key_Right ? 1 : -1));
            return true;
        }
    }
    // A plain click (not a drag, not a button) on a partly-visible card scrolls
    // it fully into view; the same as pressing the arrow toward it.
    if (event->type() == QEvent::MouseButtonPress
        && static_cast<QMouseEvent*>(event)->button() == Qt::LeftButton
        && !qobject_cast<QPushButton*>(obj))
    {
        QWidget* f = qobject_cast<QWidget*>(obj);
        while (f && !m_frameWs.contains(f))
            f = f->parentWidget();
        // Only when it's actually a card: the press propagates child->parent, so
        // a later delivery to a non-card ancestor must not clear a found frame.
        if (f)
        {
            m_clickFrame = f;
            m_clickPos = static_cast<QMouseEvent*>(event)->globalPosition().toPoint();
        }
    }
    else if (event->type() == QEvent::MouseButtonRelease && m_clickFrame)
    {
        QWidget* f = m_clickFrame;
        const QPoint rel = static_cast<QMouseEvent*>(event)->globalPosition().toPoint();
        const int move = (rel - m_clickPos).manhattanLength();
        m_clickFrame = nullptr;
        if (move < QApplication::startDragDistance())
        {
            scrollCardIntoView(f);
            // Clicking a card selects it, so the arrow keys walk on from here.
            f->setFocus(Qt::OtherFocusReason);
        }
    }

    if (!m_dragCode.contains(obj))
        return QWidget::eventFilter(obj, event);
    if (event->type() == QEvent::KeyPress && m_frameWs.contains(obj))
    {
        QKeyEvent* ke = static_cast<QKeyEvent*>(event);
        const QString ws = m_frameWs.value(obj);
        if (ke->key() == Qt::Key_Space || ke->key() == Qt::Key_Return)
        {
            if (m_runButtons.contains(ws))
                m_runButtons.value(ws)->click();
            return true;
        }
        if (ke->key() == Qt::Key_I)
        {
            emit insertRequested(QString(), m_dragCode.value(obj));
            return true;
        }
        // Speak the card's code outright — the same on-demand pattern as the
        // autocomplete's read-details key. No group-diving required.
        if (ke->key() == Qt::Key_C && ke->modifiers() == Qt::NoModifier)
        {
            emit announceRequested(tr("Code: %1").arg(m_dragCode.value(obj).trimmed()));
            return true;
        }
        // Left/Right move the selection one card at a time — the focused
        // card wears the selection ring and announces itself to a screen
        // reader — and the deck scrolls whenever the selection walks past
        // the visible edge, so every card (first and last included) is
        // reachable. The deck is linear: the ends are hard stops.
        if (ke->key() == Qt::Key_Left || ke->key() == Qt::Key_Right)
        {
            const int idx = m_cardFrames.indexOf(qobject_cast<QWidget*>(obj));
            if (idx < 0)
                return true;
            const int next = idx + (ke->key() == Qt::Key_Right ? 1 : -1);
            if (next < 0 || next >= m_cardFrames.size())
            {
                emit announceRequested(next < 0 ? tr("First card.") : tr("Last card."));
                return true;
            }
            QWidget* card = m_cardFrames.value(next);
            if (!card)
                return true;
            scrollCardIntoView(card);
            card->setFocus(Qt::OtherFocusReason);
            return true;
        }
    }
    if (event->type() == QEvent::MouseButtonPress)
    {
        QMouseEvent* me = static_cast<QMouseEvent*>(event);
        if (me->button() == Qt::LeftButton)
        {
            m_dragStart = me->pos();
            m_dragSource = obj;
        }
    }
    else if (event->type() == QEvent::MouseMove && obj == m_dragSource)
    {
        QMouseEvent* me = static_cast<QMouseEvent*>(event);
        if ((me->buttons() & Qt::LeftButton)
            && (me->pos() - m_dragStart).manhattanLength() >= QApplication::startDragDistance())
        {
            m_dragSource = nullptr;
            m_clickFrame = nullptr; // it's a drag, not a click
            QDrag* drag = new QDrag(this);
            QMimeData* mime = new QMimeData;
            mime->setText(m_dragCode.value(obj));
            // Carry the card title so the editor's drop preview can label its
            // box the same way the drag projection does.
            if (QWidget* f = m_dragFrames.value(obj))
                mime->setData(QStringLiteral("application/x-sonic-pi-card-title"),
                              m_cardFx.value(f).title.toUtf8());
            drag->setMimeData(mime);
            if (QWidget* frame = m_dragFrames.value(obj))
            {
                // Drag projection: a bordered code card with the title drawn on
                // its top border; what you pick up reads as a titled card.
                QPixmap flat = cardDragPixmap(frame).scaledToWidth(
                    qRound(ScaleHeightForDPI(240) * devicePixelRatioF()), Qt::SmoothTransformation);
                // Tilt the lifted card a few degrees, like it's been picked up.
                QTransform tilt;
                tilt.rotate(-5);
                QPixmap pm = flat.transformed(tilt, Qt::SmoothTransformation);
                drag->setPixmap(pm);
                drag->setHotSpot(QPoint(pm.width() / 2, ScaleHeightForDPI(16)));
            }
            drag->exec(Qt::CopyAction);
            // A fumbled release outside the editor still commits any
            // live preview at its last position.
            emit dragEnded();
            return true;
        }
    }
    else if (event->type() == QEvent::MouseButtonRelease)
    {
        m_dragSource = nullptr;
    }
    return QWidget::eventFilter(obj, event);
}

namespace
{
// The left "page back" zone: invisible at rest; entering it fades in a slim
// header-green bar with a ‹ chevron, centred in the margin. The click target
// is the whole margin (lane reaches the first card's edge, no dead gap);
// only the painted bar is slim. Clicks are handled by
// QuickstartPane::eventFilter.
class BackZone : public QWidget
{
public:
    QColor fill, chev;
    int contentTop = 0;    // the card row's top offset, so the bar aligns with the cards
    int contentHeight = 0; // the shared card height (0 = span the whole strip)

    explicit BackZone(QWidget* parent)
        : QWidget(parent)
    {
        m_fade = new QVariantAnimation(this);
        // Linear: an eased curve front-loads the opacity ramp so hard the
        // fade-in reads as a snap.
        m_fade->setEasingCurve(QEasingCurve::Linear);
        connect(m_fade, &QVariantAnimation::valueChanged, this, [this](const QVariant& v) {
            m_opacity = v.toReal();
            update();
        });
    }

protected:
    void enterEvent(QEnterEvent*) override { fadeTo(1.0); }
    void leaveEvent(QEvent*) override { fadeTo(0.0); }
    void paintEvent(QPaintEvent*) override
    {
        if (!isEnabled() || m_opacity <= 0.01)
            return; // invisible until hovered (and inert on page one)
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing, true);
        p.setOpacity(m_opacity);
        const int top = contentTop;
        const int h = contentHeight > 0 ? qMin(contentHeight, height() - top) : height() - top;
        const qreal w = ScaleWidthForDPI(16); // slim bar; the click zone stays full-width
        // Snug against the first card's edge (its 2px transparent border sits
        // just past the lane), keeping the breathing room on the tab side.
        const QRectF r(width() - w - ScaleWidthForDPI(3), top, w, h);
        p.setPen(Qt::NoPen);
        p.setBrush(fill);
        const qreal rad = ScaleHeightForDPI(6);
        p.drawRoundedRect(r, rad, rad);
        QPen pen(chev, ScaleHeightForDPI(2), Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);
        p.setPen(pen);
        p.setBrush(Qt::NoBrush);
        const qreal cx = r.center().x();
        const qreal cy = r.center().y();
        const qreal s = ScaleHeightForDPI(5);
        QPolygonF ch;
        ch << QPointF(cx + s * 0.5, cy - s) << QPointF(cx - s * 0.5, cy)
           << QPointF(cx + s * 0.5, cy + s);
        p.drawPolyline(ch);
    }

private:
    void fadeTo(qreal target)
    {
        if (SonicPi::prefersReducedMotion())
        {
            m_opacity = target;
            update();
            return;
        }
        m_fade->stop();
        if (target < m_opacity)
        {
            // Leaving: snap out.
            m_opacity = target;
            update();
            return;
        }
        m_fade->setDuration(500);
        m_fade->setStartValue(m_opacity);
        m_fade->setEndValue(target);
        m_fade->start();
    }

    QVariantAnimation* m_fade;
    qreal m_opacity = 0.0;
};
} // namespace

void QuickstartPane::resizeEvent(QResizeEvent* event)
{
    QWidget::resizeEvent(event);
    updateHeaderForHeight();
    updateCarousel(); // a width change alters how many whole cards fit per page
}

void QuickstartPane::updateHeaderForHeight()
{
    if (!m_deckDesc)
        return;
    // Keep the deck bar and a whole card visible; spend any height left over on
    // the description row, hiding it (and its accent rail) when it won't fit.
    // Reserve a fixed two lines (not the per-deck text height) so every deck's
    // description shows or hides at the same pane height; otherwise a longer,
    // two-line blurb (Basics) would hide while a one-line one (FX) stayed.
    const int deckBarH = m_topBar ? m_topBar->sizeHint().height() : 0;
    const int cardRow = m_cardHeight + uiScale().y(20);
    QFont descFont;
    descFont.setPixelSize(fontPx(15));
    const int descH = 2 * QFontMetrics(descFont).height() + uiScale().y(12);
    const bool showDesc = height() - deckBarH - cardRow >= descH;
    m_deckDesc->setVisible(showDesc);
    if (m_headerRail)
        m_headerRail->setVisible(showDesc);
}

void QuickstartPane::runStarted(int jobId, const QString& workspace)
{
    if (!workspace.startsWith("sonic-pi-quickstart"))
        return;
    m_jobs[workspace] = jobId;
    setCardPlaying(workspace, true);

    // Focus follows the loop: when this card starts a live_loop, the server
    // redefines that loop away from any other card that owns it. Release
    // those cards here so their Run button and scope track the handover.
    // (Cards with different loop names, as in a layering deck, keep playing.)
    const QSet<QString>& mine = m_cardLoops.value(workspace);
    if (mine.isEmpty())
        return;
    const QList<QString> others = m_jobs.keys();
    for (const QString& ws : others)
    {
        if (ws == workspace)
            continue;
        QSet<QString> theirs = m_cardLoops.value(ws);
        if (theirs.isEmpty())
            continue;
        theirs.subtract(mine);
        if (theirs.isEmpty()) // every loop this card owned is now owned by us
        {
            m_jobs.remove(ws);
            setCardPlaying(ws, false);
        }
    }
}

void QuickstartPane::runEnded(int jobId)
{
    for (auto it = m_jobs.begin(); it != m_jobs.end(); ++it)
    {
        if (it.value() == jobId)
        {
            const QString workspace = it.key();
            m_jobs.erase(it);
            setCardPlaying(workspace, false);
            return;
        }
    }
}

void QuickstartPane::setScopeHover(QPushButton* button, bool hover)
{
    const QString workspace = m_runButtons.key(button);
    if (workspace.isEmpty())
        return;
    CardScope* scope = m_scopes.value(workspace);
    if (!scope)
        return;
    const QColor accent = m_theme->color("HighlightedBackground");
    const QColor fg = m_theme->color("Foreground");
    // A playing card keeps its rings in the full resting colours — the live
    // trace is the point; only the glyph tracks hover during playback.
    const bool idleHover = hover && !m_jobs.contains(workspace);
    if (idleHover)
        scope->setColours(fg, fg);
    else // the resting pair from addCard
        scope->setColours(accent, SonicPiTheme::blend(fg, accent, 0.5));
    // Hover also lifts the idle fade, so the rings match the glyph at full
    // strength rather than a washed-out version of it.
    scope->setLit(idleHover);
}

void QuickstartPane::setCardPlaying(const QString& workspace, bool playing)
{
    QPushButton* button = m_runButtons.value(workspace);
    if (button)
    {
        // The run control is the scope disc: refresh both hover variants and
        // show the one matching the current pointer state.
        const int d = button->iconSize().width();
        m_iconNormal[button] = QIcon(playDisc(playing, d, false));
        m_iconHover[button] = QIcon(playDisc(playing, d, true));
        button->setIcon(button->underMouse() ? m_iconHover.value(button)
                                             : m_iconNormal.value(button));
        button->setAccessibleName(playing ? tr("Stop this card") : tr("Run this card"));
        // Re-derive the ring colours for the new playback state: starting
        // under the pointer settles them to full colour, stopping under the
        // pointer lets them pick the hover tint back up.
        setScopeHover(button, button == m_hoverIcon);
    }
    CardScope* scope = m_scopes.value(workspace);
    if (scope)
    {
        if (playing)
            scope->start(m_spAPI.get());
        else
            scope->stop();
    }
}

void QuickstartPane::flashLine(const QString& workspace, int line)
{
    // Runtime lines are 1-based and the run is wrapped in one leading
    // with_fx :scope_out line, so the card's own code starts at line 2.
    const int idx = line - 2;
    const QVector<QLabel*> lines = m_codeLines.value(workspace);
    if (idx < 0 || idx >= lines.size())
        return;
    QLabel* label = lines[idx];
    // A snippet that overruns the card's budget scrolls, so follow the run and
    // keep the lit line in view.
    if (QScrollArea* sa = m_codeScrolls.value(workspace))
        sa->ensureWidgetVisible(label, 0, 0);
    label->setProperty("flashing", true);
    repolish(label);
    QPointer<QLabel> guard(label);
    QTimer::singleShot(SonicPi::kFlashHoldMs, this, [guard] {
        if (!guard)
            return;
        guard->setProperty("flashing", false);
        repolish(guard);
    });
}

void QuickstartPane::setUserZoom(int zoom)
{
    m_userZoom = qBound(kFontZoomMin, zoom, kFontZoomMax);
    m_zoomFactor = FontZoomFactor(m_userZoom);
    rebuild();
}

// Shared curve (utils/fontroles.h): multiplicative, so the card's type
// hierarchy holds its proportions as it zooms. The old `base + m_userZoom`
// added the same pixel to every size, which pulled a 17px heading down
// toward a 13px pill the further you zoomed in.
int QuickstartPane::fontPx(int base) const
{
    return qMax(8, qRound(ScaleHeightForDPI(base) * m_zoomFactor));
}

void QuickstartPane::rebuild()
{
    // Clear the whole layout: rebuild() is called from the deck pills / zoom
    // buttons, so deleteLater (not delete) avoids destroying the widget whose
    // click we are still handling. Takes the side column and the right
    // container (header + scroll) so nothing accumulates.
    if (QLayout* lay = layout())
    {
        while (QLayoutItem* item = lay->takeAt(0))
        {
            if (QWidget* w = item->widget())
            {
                w->setParent(nullptr);
                w->deleteLater();
            }
            delete item;
        }
    }
    m_topBar = nullptr;
    m_navRow = nullptr;
    m_scroll = nullptr;
    m_cardRow = nullptr;
    m_cardGrid = nullptr;
    m_cardFrames.clear();
    m_gridSpacer = nullptr;
    m_gridRows = 0;
    m_prevArrow = nullptr;
    m_nextArrow = nullptr;
    m_backEdge = nullptr;
    m_dotsHost = nullptr;
    m_dotsLayout = nullptr;
    m_headerRail = nullptr;
    m_deckTitle = nullptr;
    m_deckDesc = nullptr;
    m_runButtons.clear();
    m_cardFx.clear();
    m_hoverCard = nullptr; // frames about to be destroyed; drop dangling refs
    if (m_hoverIcon && m_addCode.contains(m_hoverIcon))
        emit insertPreviewCleared(); // don't orphan a hover preview across a rebuild
    m_hoverIcon = nullptr;
    m_iconNormal.clear();
    m_iconHover.clear();
    m_scopes.clear();
    m_codeLines.clear();
    m_codeScrolls.clear();
    m_cardLoops.clear();
    m_dragCode.clear();
    m_dragFrames.clear();
    m_addCode.clear();
    m_addTitle.clear();
    m_frameWs.clear();
    m_dragSource = nullptr;

    const QVector<Deck>& decks = quickstartDecks(m_cardsPath);
    if (decks.isEmpty())
        return;
    if (m_deckIdx >= decks.size())
        m_deckIdx = 0;

    const QColor accent = m_theme->color("HighlightedBackground");

    // A single vertical stack: a full-width deck bar (the deck selector) over
    // the deck description over the cards.
    QWidget* rightSide = new QWidget(this);
    QVBoxLayout* rightCol = new QVBoxLayout(rightSide);
    rightCol->setContentsMargins(0, 0, 0, 0);
    rightCol->setSpacing(0);

    // Deck bar: selector pills on the left, the carousel pill centred, zoom on
    // the right. The highlighted pill is the current deck (it doubles as title).
    m_topBar = new QWidget(rightSide);
    // Containers a screen reader walks through get names — an anonymous
    // "group" stop tells the user nothing about where they are.
    m_topBar->setAccessibleName(tr("Deck selector"));
    QHBoxLayout* deckBar = new QHBoxLayout(m_topBar);
    deckBar->setContentsMargins(uiScale().y(14), uiScale().y(8),
                                uiScale().y(12), uiScale().y(3));
    deckBar->setSpacing(uiScale().y(6));
    for (int i = 0; i < decks.size(); ++i)
    {
        QPushButton* pill = new QPushButton(decks[i].title, m_topBar);
        pill->setObjectName(QStringLiteral("qsDeckPill"));
        pill->setProperty("current", i == m_deckIdx);
        pill->setCursor(Qt::PointingHandCursor);
        pill->setAccessibleName(tr("%1 card deck").arg(decks[i].title));
        pill->setStyleSheet(QString("font-size: %1px;").arg(fontPx(13)));
        connect(pill, &QPushButton::clicked, this, [this, i] {
            // Each deck keeps its own carousel position: stash where this deck
            // was, restore where the target deck last was (first card if new).
            m_deckPages[m_deckIdx] = m_pageIndex;
            m_deckIdx = i;
            m_pageIndex = m_deckPages.value(i, 0);
            rebuild();
            // Hand focus to the carousel so the Left/Right keys drive it
            // straight away after choosing a deck, and say what arrived.
            if (m_scroll)
                m_scroll->setFocus(Qt::OtherFocusReason);
            const QVector<Deck>& d = quickstartDecks(m_cardsPath);
            if (m_deckIdx < d.size())
                emit announceRequested(tr("%1 deck, %2 cards")
                                           .arg(d[m_deckIdx].title)
                                           .arg(d[m_deckIdx].cards.size()));
        });
        deckBar->addWidget(pill);
    }
    deckBar->addStretch(1);

    // Carousel controls: prev arrow, page dots and next arrow in one grouped pill.
    auto makeArrow = [&](const QString& glyph, const QString& a11y) {
        QPushButton* b = new QPushButton(glyph, m_topBar);
        b->setObjectName(QStringLiteral("qsArrow"));
        ApplyFontRole(b, FontRole::Arrow);
        b->setCursor(Qt::PointingHandCursor);
        b->setAccessibleName(a11y);
        b->setFixedSize(uiScale().size(30, 30));
        return b;
    };
    m_prevArrow = makeArrow(QStringLiteral("‹"), tr("Previous cards"));
    m_nextArrow = makeArrow(QStringLiteral("›"), tr("More cards"));
    connect(m_prevArrow, &QPushButton::clicked, this, [this] { goToPage(m_pageIndex - 1); });
    connect(m_nextArrow, &QPushButton::clicked, this, [this] { goToPage(m_pageIndex + 1); });

    m_dotsHost = new QWidget(m_topBar);
    m_dotsLayout = new QHBoxLayout(m_dotsHost);
    m_dotsLayout->setContentsMargins(uiScale().y(6), 0, uiScale().y(6), 0);
    m_dotsLayout->setSpacing(uiScale().y(8));

    QWidget* nav = new QWidget(m_topBar);
    nav->setObjectName(QStringLiteral("qsNav"));
    nav->setAccessibleName(tr("Card navigation"));
    QHBoxLayout* navLay = new QHBoxLayout(nav);
    navLay->setContentsMargins(uiScale().y(4), uiScale().y(2),
                               uiScale().y(4), uiScale().y(2));
    navLay->setSpacing(uiScale().y(2));
    // Pin the height rather than deriving it: the radius has to be exactly half
    // of what the widget ACTUALLY ends up as, and the layout was giving this
    // row 30px where the contents-plus-margins calculation said 27 — leaving
    // the pill a pixel-and-a-half short of round. Fixing the height makes the
    // two agree by construction. Even, so the halving is exact.
    int navH = uiScale().y(30) + 2 * uiScale().y(2);
    navH += navH % 2;
    nav->setFixedHeight(navH);
    nav->setStyleSheet(
        QStringLiteral("QWidget#qsNav { border-radius: %1px; }").arg(navH / 2));
    navLay->addWidget(m_prevArrow);
    navLay->addWidget(m_dotsHost);
    navLay->addWidget(m_nextArrow);
    // nav is added to its own centred row below the cards (see below).
    // A-/A+ zoom lives in the dock title row (zoomControls()), not here.
    rightCol->addWidget(m_topBar);

    // Description row: a small accent rail and the deck's one-line description.
    // Hidden first as the pane shrinks (updateHeaderForHeight).
    m_deckTitle = nullptr;
    m_deckDesc = nullptr;
    m_headerRail = nullptr;
    const QString desc = decks[m_deckIdx].description;
    if (!desc.isEmpty())
    {
        QWidget* descRow = new QWidget(rightSide);
        QHBoxLayout* descLay = new QHBoxLayout(descRow);
        descLay->setContentsMargins(uiScale().y(14), uiScale().y(4), uiScale().y(14),
                                    uiScale().y(9));
        descLay->setSpacing(uiScale().y(10));
        QWidget* rail = new QWidget(descRow);
        rail->setObjectName(QStringLiteral("qsHeaderRail"));
        int railW = uiScale().y(4);
        railW += railW % 2;   // even: the radius must be exactly half (see rebuildDots)
        rail->setFixedWidth(railW);
        rail->setStyleSheet(
            QStringLiteral("QWidget#qsHeaderRail { border-radius: %1px; }").arg(railW / 2));
        descLay->addWidget(rail);
        m_headerRail = rail;
        QLabel* descLabel = new QLabel(desc, descRow);
        descLabel->setObjectName(QStringLiteral("qsDeckDesc"));
        descLabel->setWordWrap(true);
        descLabel->setStyleSheet(QString("font-size: %1px;").arg(fontPx(15)));
        descLay->addWidget(descLabel, 1);
        m_deckDesc = descLabel;
        rightCol->addWidget(descRow);
    }

    // Cards live on a single horizontal row with no scrollbar and no wheel
    // scrolling. You move through the deck a whole page at a time (all the
    // cards that fit), snapping to card boundaries so a card is never left
    // half on screen. The header arrows and dots drive it.
    m_scroll = new QScrollArea(rightSide);
    m_scroll->setWidgetResizable(false); // content keeps its natural width so it can overflow
    m_scroll->setFrameShape(QFrame::NoFrame);
    m_scroll->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    m_scroll->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    m_scroll->setFocusPolicy(Qt::StrongFocus); // hold focus so Left/Right page it
    m_scroll->setAccessibleName(tr("%1 cards").arg(decks[m_deckIdx].title));
    m_scroll->setAccessibleDescription(
        tr("Left and Right arrow keys move through the cards."));
    // Focusing the pane (F6 cycling, focusPane) lands on the current card —
    // rebuildDots keeps the proxy pointing at the page's first card.
    setFocusProxy(m_scroll);
    QWidget* content = new QWidget(m_scroll);
    content->setAccessibleName(tr("Cards"));
    m_cardRow = content;
    m_cardGrid = new QGridLayout(content);
    const int pad = uiScale().y(14);
    // No left margin: the back zone's lane provides the left pad, so the whole
    // margin up to the first card is one contiguous click target.
    m_cardGrid->setContentsMargins(0, uiScale().y(8), pad, pad);
    m_cardGrid->setHorizontalSpacing(uiScale().y(14));
    m_cardGrid->setVerticalSpacing(uiScale().y(14));

    // One set of dimensions for every card in every deck.
    computeGlobalLayout();

    const QVector<SonicPi::QuickstartCard>& cards = decks[m_deckIdx].cards;
    m_cardCount = cards.size();
    m_cardFrames.clear();
    for (int i = 0; i < cards.size(); ++i)
    {
        // Each card gets its own scope slot, wrapping within the cards' slot
        // budget so a user-authored deck with more cards than slots shares
        // slots between cards rather than colliding with the live_loop range.
        QWidget* f = addCard(cards[i],
                             QString("sonic-pi-quickstart-%1-%2").arg(m_deckIdx).arg(i),
                             kFirstScopeSlot + (i % kCardScopeSlots));
        // The name carries the blurb: a screen reader always speaks the name
        // on focus, whereas the description (AXHelp) is at the mercy of the
        // user's hint-verbosity settings.
        const QString plainBlurb =
            QTextDocumentFragment::fromHtml(cards[i].blurb).toPlainText().simplified();
        f->setAccessibleName(tr("%1 card, %2 of %3. %4")
                                 .arg(cards[i].title)
                                 .arg(i + 1)
                                 .arg(cards.size())
                                 .arg(plainBlurb));
        m_cardFrames << f;
    }
    // Trailing spacer: sits in the grid column after the last card and holds
    // the right-hand pad so the final page still snaps to a card boundary
    // (no half cards) without the grid spreading the cards apart.
    m_gridSpacer = new QWidget(content);
    m_gridSpacer->setFixedWidth(0);
    m_gridRows = 0; // force the first updateCarousel to place the grid
    m_cardHeight = m_cardFrames.isEmpty() ? 0 : m_cardFrames.first()->sizeHint().height();

    m_scroll->setWidget(content);
    m_scroll->installEventFilter(this);
    m_scroll->viewport()->installEventFilter(this);

    // Left-edge "page back" affordance: the right side is easy (click the card
    // peeking in), but a full page snaps flush-left with nothing to click going
    // back. The whole left margin is the click target, right up to the first
    // card's edge (the grid's left pad lives in this lane, not the grid).
    // Invisible at rest; hovering reveals a soft wash + chevron (see
    // BackZone). Click handled in eventFilter.
    BackZone* backZone = new BackZone(rightSide);
    backZone->fill = accent; // the card headers' green
    backZone->chev = m_theme->accentContrastText();
    m_backEdge = backZone;
    m_backEdge->setFixedWidth(uiScale().x(26) + uiScale().y(14));
    m_backEdge->setFocusPolicy(Qt::NoFocus);
    // No accessible name: unnamed, it's pruned from the accessibility tree.
    // It's a mouse-only convenience that duplicates the Previous arrow — to
    // a screen reader it was just one more mystery stop.
    m_backEdge->installEventFilter(this);

    QHBoxLayout* scrollRow = new QHBoxLayout;
    scrollRow->setContentsMargins(0, 0, 0, 0);
    scrollRow->setSpacing(0);
    scrollRow->addWidget(m_backEdge);
    scrollRow->addWidget(m_scroll, 1);
    rightCol->addLayout(scrollRow, 1);

    // The row must always rest with a full card flush left. Qt scrolls the
    // area behind our back (e.g. auto-revealing a clicked button inside a
    // partly-visible card), so once the scrollbar settles anywhere off a card
    // boundary, snap to the nearest page.
    QTimer* settle = new QTimer(m_scroll);
    settle->setSingleShot(true);
    settle->setInterval(160);
    connect(m_scroll->horizontalScrollBar(), &QScrollBar::valueChanged, settle,
            [settle] { settle->start(); });
    connect(settle, &QTimer::timeout, this, [this] {
        if (!m_scroll)
            return;
        const int step = cardWidth() + uiScale().y(14);
        QScrollBar* sb = m_scroll->horizontalScrollBar();
        const int p = qBound(0, qRound(double(sb->value()) / step), pageCount() - 1);
        // Same clamp goToPage applies, so an unreachable boundary can't make
        // this re-fire forever.
        const int target = qBound(sb->minimum(), p * step, sb->maximum());
        if (sb->value() != target)
            goToPage(p);
        else if (p != m_pageIndex)
        {
            m_pageIndex = p; // keep the dots honest if something scrolled silently
            rebuildDots();
        }
    });

    // Carousel control on its own full-width row beneath the cards, centred.
    QWidget* navRow = new QWidget(rightSide);
    QHBoxLayout* navRowLay = new QHBoxLayout(navRow);
    navRowLay->setContentsMargins(0, uiScale().y(4), 0, uiScale().y(6));
    navRowLay->addStretch(1);
    navRowLay->addWidget(nav);
    navRowLay->addStretch(1);
    rightCol->addWidget(navRow);
    m_navRow = navRow;

    m_pageIndex = qBound(0, m_pageIndex, qMax(0, pageCount() - 1));
    static_cast<QHBoxLayout*>(layout())->addWidget(rightSide, 1);
    // The viewport size (and so rows/columns per page) isn't final until the
    // pane is laid out; compute it now and again once the geometry settles.
    updateCarousel();
    QTimer::singleShot(0, this, [this] { updateCarousel(); });
    updateHeaderForHeight();
}

int QuickstartPane::rowsThatFit() const
{
    if (!m_scroll || m_cardHeight <= 0)
        return 1;
    const int gap = uiScale().y(14);
    const int marginsV = uiScale().y(8) + uiScale().y(14);
    const int vh = m_scroll->viewport()->height() - marginsV;
    int rows = qMax(1, (vh + gap) / (m_cardHeight + gap));
    if (m_cardCount > 0)
        rows = qMin(rows, m_cardCount);
    return rows;
}

int QuickstartPane::cardsPerView() const
{
    if (!m_scroll)
        return 1;
    const int step = cardWidth() + uiScale().y(14);
    const int vw = m_scroll->viewport()->width();
    return qMax(1, (vw + uiScale().y(14)) / step);
}

int QuickstartPane::pageCount() const
{
    // The carousel moves one card (column) at a time, so the number of scroll
    // stops is the columns that don't fit, plus one (each stop shows a full
    // set of whole cards, shifted along by one).
    const int rows = rowsThatFit();
    const int totalCols = m_cardCount > 0 ? (m_cardCount + rows - 1) / rows : 1;
    const int cpv = cardsPerView();
    return qMax(1, totalCols - cpv + 1);
}

void QuickstartPane::rebuildDots()
{
    if (!m_dotsLayout || !m_dotsHost || !m_prevArrow || !m_nextArrow)
        return;
    const int pages = pageCount();
    m_pageIndex = qBound(0, m_pageIndex, pages - 1);
    const int rows = rowsThatFit();
    const int cpv = cardsPerView();

    // One dot per card; every card currently on screen lights accent, so the
    // lit run of dots shows how many cards are visible and where you are.
    // The dots are created once per rebuild and restyled in place on page
    // changes: destroying them would yank the very button a keyboard or
    // screen-reader user just pressed out from under their focus (which then
    // falls all the way back to the window).
    if (m_dotsLayout->count() != m_cardCount)
    {
        while (QLayoutItem* it = m_dotsLayout->takeAt(0))
        {
            if (QWidget* w = it->widget())
                w->deleteLater();
            delete it;
        }
        // Even, so the radius below is exactly half. Qt does not clamp a
        // radius to half the box — it paints artifacts above that and visible
        // corners below (see kPillRadiusDx in dpi.h) — so only an exact half
        // renders a true circle. An odd box makes that impossible in integer
        // pixels, which is why rounding either way alternated circle/square
        // as zoom nudged the size.
        int dot = uiScale().y(10);
        dot += dot % 2;
        for (int i = 0; i < m_cardCount; ++i)
        {
            QPushButton* d = new QPushButton(m_dotsHost);
            d->setObjectName(QStringLiteral("qsDot"));
            d->setCursor(Qt::PointingHandCursor);
            d->setFixedSize(dot, dot);
            // Exactly half the (even) box — see the size calculation above.
            d->setStyleSheet(
                QStringLiteral("QPushButton#qsDot { border-radius: %1px; }").arg(dot / 2));
            d->setAccessibleName(tr("Scroll to card %1 of %2").arg(i + 1).arg(m_cardCount));
            // Row count (and so this card's column) can change between
            // presses; resolve at click time.
            connect(d, &QPushButton::clicked, this, [this, i] {
                goToPage(qBound(0, i / qMax(1, rowsThatFit()), pageCount() - 1));
            });
            m_dotsLayout->addWidget(d);
        }
    }
    for (int i = 0; i < m_dotsLayout->count(); ++i)
    {
        QWidget* d = m_dotsLayout->itemAt(i)->widget();
        if (!d)
            continue;
        const int col = rows > 0 ? i / rows : i;
        const bool visible = col >= m_pageIndex && col < m_pageIndex + cpv;
        if (d->property("lit").toBool() != visible)
        {
            d->setProperty("lit", visible);
            repolish(d);
        }
    }
    m_dotsHost->setVisible(pages > 1);
    m_prevArrow->setVisible(pages > 1);
    m_nextArrow->setVisible(pages > 1);
    m_prevArrow->setEnabled(m_pageIndex > 0);
    m_nextArrow->setEnabled(m_pageIndex < pages - 1);
    // Keep the pane's focus entry point on the page's current card (see
    // focusCarousel for why a card, not the scroll area).
    QWidget* entry = firstVisibleCard();
    setFocusProxy(entry ? entry : static_cast<QWidget*>(m_scroll));
    updateBackEdge();
}

void QuickstartPane::updateBackEdge()
{
    if (!m_backEdge)
        return;
    // The margin keeps its slot (fixed width, no layout shift); only its
    // clickability and hover-reveal come and go with the page.
    const bool canBack = m_pageIndex > 0;
    BackZone* bz = static_cast<BackZone*>(m_backEdge);
    // Wash aligns with the cards' PAINTED extent: the frame reserves a 2px
    // transparent hover border top and bottom, so inset past it.
    const int borderW = uiScale().y(2);
    bz->contentTop = uiScale().y(8) + borderW;
    bz->contentHeight = qMax(0, m_cardHeight - 2 * borderW);
    bz->setEnabled(canBack);
    bz->setCursor(canBack ? Qt::PointingHandCursor : Qt::ArrowCursor);
    bz->setToolTip(canBack ? tr("Click to see the previous cards.") : QString());
    bz->update();
}

void QuickstartPane::relayoutGrid(int rows)
{
    if (!m_cardGrid)
        return;
    // Take every card out of the grid (the card widgets survive; only the
    // layout items are freed) and re-place them column-major, so paging left
    // and right moves through whole columns.
    while (QLayoutItem* it = m_cardGrid->takeAt(0))
        delete it;
    for (int i = 0; i < m_cardFrames.size(); ++i)
        m_cardGrid->addWidget(m_cardFrames[i], i % rows, i / rows,
                              Qt::AlignTop | Qt::AlignLeft);
    const int totalCols = m_cardFrames.isEmpty() ? 1 : (m_cardFrames.size() + rows - 1) / rows;
    if (m_gridSpacer)
        m_cardGrid->addWidget(m_gridSpacer, 0, totalCols, qMax(1, rows), 1);
}

void QuickstartPane::updateCarousel()
{
    if (!m_scroll || !m_cardGrid || !m_cardRow)
        return;
    const int gap = uiScale().y(14);
    const int pad = uiScale().y(14);
    const int step = cardWidth() + gap;

    // Use as many rows as fit the height, then page horizontally by columns.
    const int rows = rowsThatFit();
    if (rows != m_gridRows)
    {
        relayoutGrid(rows);
        m_gridRows = rows;
    }
    const int totalCols = m_cardCount > 0 ? (m_cardCount + rows - 1) / rows : 1;
    const int pages = pageCount();
    m_pageIndex = qBound(0, m_pageIndex, pages - 1);

    // Pad the trailing spacer so the last stop still lands on a card boundary
    // (no half cards). The spacer absorbs the slack so the cards themselves
    // stay tightly packed rather than the grid spreading them apart.
    const int vw = m_scroll->viewport()->width();
    // Right margin only; the left pad lives in the back zone's lane.
    const int gridW = totalCols > 0 ? totalCols * cardWidth() + (totalCols - 1) * gap + pad : 0;
    const int lastOffset = (pages - 1) * step;
    if (m_gridSpacer)
        m_gridSpacer->setFixedWidth(qMax(0, lastOffset + vw - gridW));
    // Recompute the grid immediately: adjustSize() otherwise reads a stale
    // size hint from before the spacer resize, leaving the content too narrow
    // for the last page to land on a card boundary.
    m_cardGrid->invalidate();
    m_cardGrid->activate();
    m_cardRow->adjustSize();

    rebuildDots();
    // Snap straight to the current stop (no animation; this is a relayout).
    m_scroll->horizontalScrollBar()->setValue(m_pageIndex * step);
}

QWidget* QuickstartPane::firstVisibleCard() const
{
    const int rows = qMax(1, rowsThatFit());
    return m_cardFrames.value(m_pageIndex * rows, nullptr);
}

void QuickstartPane::focusCarousel()
{
    // Land on the current card, not the scroll area: the card is a named
    // group ("Play a note card, 1 of 31") with the Space/I actions, whereas
    // the scroll area reads as an anonymous container a screen-reader user
    // has to dig into.
    if (QWidget* card = firstVisibleCard())
    {
        card->setFocus(Qt::OtherFocusReason);
        return; // the card's accessible name already carries its position
    }
    if (m_scroll)
    {
        m_scroll->setFocus(Qt::OtherFocusReason);
        announcePage();
    }
}

void QuickstartPane::goToPage(int page)
{
    if (!m_scroll)
        return;
    const int step = cardWidth() + uiScale().y(14);
    const int pages = pageCount();
    // Linear deck: the ends are hard stops.
    const int prevIndex = m_pageIndex;
    m_pageIndex = qBound(0, page, pages - 1);

    QScrollBar* sb = m_scroll->horizontalScrollBar();
    const int target = qBound(sb->minimum(), m_pageIndex * step, sb->maximum());
    // With a screen reader connected, snap instead of glide: focus moves to
    // the new page's card immediately, and an animation would leave the
    // reader's focus rectangle drawn where the card was mid-slide.
    if (SonicPi::prefersReducedMotion() || QAccessible::isActive())
    {
        sb->setValue(target);
    }
    else
    {
        QPropertyAnimation* anim = new QPropertyAnimation(sb, "value", sb);
        anim->setDuration(240);
        anim->setStartValue(sb->value());
        anim->setEndValue(target);
        anim->setEasingCurve(QEasingCurve::OutCubic);
        anim->start(QAbstractAnimation::DeleteWhenStopped);
    }
    rebuildDots(); // recolour dots + arrow state for the page we moved to
    if (m_pageIndex != prevIndex)
        announcePage();
}

// Tell a screen reader where paging landed. Always speaks per-card ("Card 6
// of 10") — a screen reader experiences one card at a time, so the visual
// page range ("cards 6 to 10") is meaningless to it.
void QuickstartPane::announcePage()
{
    if (m_cardCount <= 0)
        return;
    // When focus is riding the cards (keyboard/screen-reader paging), the
    // newly focused card announces itself.
    QWidget* focused = QApplication::focusWidget();
    if (focused && m_frameWs.contains(focused))
        return;
    const int lead = m_pageIndex * qMax(1, rowsThatFit());
    if (lead >= m_cardCount)
        return;
    emit announceRequested(tr("Card %1 of %2").arg(lead + 1).arg(m_cardCount));
}

void QuickstartPane::scrollCardIntoView(QWidget* frame)
{
    const int idx = m_cardFrames.indexOf(frame);
    if (idx < 0)
        return;
    const int rows = qMax(1, rowsThatFit());
    const int col = idx / rows;
    const int cpv = cardsPerView();
    // The card sits in column `col`; the fully-visible columns are
    // [m_pageIndex, m_pageIndex + cpv). If it's outside (only peeking), page so
    // it becomes flush at the near edge.
    if (col < m_pageIndex)
        goToPage(col);
    else if (col >= m_pageIndex + cpv)
        goToPage(col - cpv + 1);
}

QIcon QuickstartPane::svgIcon(TablerIcons::Glyph glyph, const QColor& colour, int px) const
{
    return TablerIcons::icon(glyph, colour, px, devicePixelRatioF());
}

QPixmap QuickstartPane::playDisc(bool playing, int d, bool hover) const
{
    const qreal dpr = devicePixelRatioF();
    const QColor accent = m_theme->color("HighlightedBackground");
    const QColor fg = m_theme->color("Foreground");
    // The disc is a solid accent ring, flipping to the same foreground the
    // scope rings light up in on hover (see setScopeHover). The glyph is
    // always the plain dark pane grey — a clean cut-out in the logo rather
    // than a tinted blend.
    const QColor bg = m_theme->color("PaneBackground");
    const QColor disc = hover ? fg : accent;
    return TablerIcons::transportRing(playing, disc, bg, d, dpr);
}

QPixmap QuickstartPane::cardDragPixmap(QWidget* frame) const
{
    const CardHoverFx fx = m_cardFx.value(frame);
    if (!fx.body)
        return frame->grab();

    // Paint in device pixels; set the DPR at the end.
    const qreal dpr = frame->devicePixelRatioF();
    const QPixmap code = fx.body->grab(); // highlighted code area
    const int cwD = code.width();
    const int chD = code.height();

    const QColor accent = m_theme->color("HighlightedBackground");
    const QColor cardBg = m_theme->accentTint();

    QFont titleFont(QStringLiteral("Hack"));
    titleFont.setPixelSize(qRound(fontPx(15) * dpr));
    titleFont.setBold(true);
    const QFontMetrics fm(titleFont);
    const int titleHD = fm.height();
    const int textWD = fm.horizontalAdvance(fx.title);

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
    p.drawText(QRectF(titleX, 0, textWD, titleHD), Qt::AlignVCenter | Qt::AlignLeft, fx.title);
    p.end();

    pm.setDevicePixelRatio(dpr);
    return pm;
}

QWidget* QuickstartPane::addCard(const SonicPi::QuickstartCard& card, const QString& workspace,
                                 int scopeSlot)
{
    const QColor accent = m_theme->color("HighlightedBackground");
    const QColor fg = m_theme->color("Foreground");

    // Cheat-sheet-style card: solid accent header bar, tinted body below,
    // like the printed Sonic Pi cheat sheets. Chrome is styled in app.qss
    // (#qsCard and friends); only the zoomed font sizes are set here.
    const int cardW = cardWidth();
    QFrame* frame = new QFrame;
    frame->setObjectName(QStringLiteral("qsCard"));
    frame->setFixedWidth(cardW);
    QVBoxLayout* cardLayout = new QVBoxLayout(frame);
    cardLayout->setContentsMargins(0, 0, 0, 0);
    cardLayout->setSpacing(0);

    // Header bar: accent strip, title on the left, action buttons on the
    // right. The bar background and title are created now but the buttons
    // are created LAST — after the code and description — and only PLACED
    // here: assistive technology reads widgets in creation order, so the
    // card must read content first and actions after, even though the
    // buttons render at the top. The grid makes that possible (where a
    // widget lands is independent of when it was made). No vertical margin
    // so the icon buttons fill the bar top to bottom; the outer button's
    // corner is rounded to match the card.
    QGridLayout* headerGrid = new QGridLayout();
    headerGrid->setContentsMargins(0, 0, 0, 0);
    headerGrid->setSpacing(0);
    QWidget* header = new QWidget(frame);
    header->setObjectName(QStringLiteral("qsCardHeader"));
    headerGrid->addWidget(header, 0, 0, 1, 6);
    headerGrid->setColumnMinimumWidth(0, uiScale().y(14)); // title padded on the left
    headerGrid->setColumnStretch(2, 1);
    // TutHeading: a real heading (level 2, under the deck) for screen-reader
    // rotor navigation from card to card.
    QLabel* heading = new TutHeading(card.title, 2, frame);
    heading->setObjectName(QStringLiteral("qsCardTitle"));
    heading->setStyleSheet(QString("font-size: %1px;").arg(fontPx(17)));
    // Match the widget font to the stylesheet size so the bar height (and thus
    // the button height) is measured correctly; sizeHint uses the widget font.
    {
        QFont hf = heading->font();
        hf.setPixelSize(fontPx(17));
        hf.setBold(true);
        heading->setFont(hf);
    }
    // Copy/Add/Drag are round icon buttons on the right of the card's header
    // bar (styled for the accent background); the title sits on the left. The
    // rest of the card (and the header around the buttons) stays a drag
    // surface onto the editor.
    const QString title = card.title;
    const QString snippet = card.code;
    const bool playing = m_jobs.contains(workspace);
    // What lands in the editor for every path (drag, Add, menu, keyboard):
    // just the card's code with blank lines around it; the title rides on the
    // drag image, not as a comment left behind in the editor.
    const QString payload = QStringLiteral("\n%1\n\n").arg(card.code);
    const QString addTitle = card.title;

    const QColor onAccent = m_theme->accentContrastText();
    // Fill the whole header-bar height: the title sets the bar height (measured
    // via the font above), so the buttons match it exactly.
    const int btnH = heading->sizeHint().height() + uiScale().y(22);
    const int iconPx = uiScale().y(24);
    // Width snug around the glyph so the two icons sit close together (a wide
    // button padded the icons far apart); still a comfortable click target.
    const int btnW = iconPx + uiScale().y(14);
    // Icon buttons: a white glyph on the accent bar. On hover the button fills
    // with the near-white code-body colour and the glyph flips to a dark
    // contrasting ink; a strong, legible contrast (done on Enter/Leave in the
    // eventFilter).
    const QColor codeBg = m_theme->accentTint();
    const QColor hoverInk = m_theme->contrastingText(codeBg);
    auto setupIconBtn = [this, iconPx, &hoverInk, &onAccent](QPushButton* b,
                                                             TablerIcons::Glyph svg) {
        // Tab reaches the button (the :focus wash is for keyboard users), but a
        // click must not take focus; the wash would linger after the click.
        b->setFocusPolicy(Qt::TabFocus);
        const QIcon normal = svgIcon(svg, onAccent, iconPx);
        b->setIcon(normal);
        m_iconNormal[b] = normal;
        m_iconHover[b] = svgIcon(svg, hoverInk, iconPx);
    };

    // Play/Stop is the scope in the footer, not a header button; the header
    // action buttons are created after the content (see below).
    headerGrid->addWidget(heading, 0, 1, Qt::AlignVCenter);
    cardLayout->addLayout(headerGrid);

    // Card anatomy, top to bottom: accent header, code area, docs strip
    // (deeper tint), live scope strip. Code and docs sections are equalised
    // across the deck after all cards are built.
    // The card is a fixed-size format, but the budgets in computeGlobalLayout
    // are a design target, not a guarantee: nothing validates a cards file, so
    // an over-long or over-wide snippet used to be clipped away with no sign
    // it was there. The body scrolls on both axes instead — the card keeps its
    // uniform size (every paging calculation depends on that) and the overflow
    // stays reachable. The body *is* the scroll area rather than holding one,
    // so this adds no node to the accessibility tree.
    QScrollArea* body = new QScrollArea(frame);
    body->setObjectName(QStringLiteral("qsCardBody"));
    body->setAccessibleName(tr("Code"));
    body->setFixedHeight(m_codeBodyH); // uniform across all decks
    body->setFrameShape(QFrame::NoFrame);
    body->setWidgetResizable(true);
    // Unlike the deck's carousel (which is paged, never free-scrolled), the
    // body scrolls normally — but only when the snippet actually overruns.
    body->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    body->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    // The card owns focus and the arrow keys walk the deck, so the body must
    // not become a tab stop of its own.
    body->setFocusPolicy(Qt::NoFocus);
    // Let the body's accent tint show through: a viewport fills its own
    // background by default, which would paint over it.
    body->viewport()->setAutoFillBackground(false);
    // Wheel is shared with the carousel — see eventFilter.
    body->viewport()->installEventFilter(this);
    const int pad = uiScale().y(14);
    cardLayout->addWidget(body);
    m_codeScrolls[workspace] = body;

    SonicPi::CodeColours colours;
    colours.keyword = m_theme->color("KeywordForeground").name();
    colours.symbol = m_theme->color("SymbolForeground").name();
    colours.number = m_theme->color("NumberForeground").name();
    colours.string = m_theme->color("DoubleQuotedStringForeground").name();
    colours.comment = m_theme->color("CommentForeground").name();

    // One label per code line so the trigger wash can light individual
    // lines, mirroring the editor's flash. The block carries the body's
    // padding, the scroll area itself having none.
    QWidget* codeBlock = new QWidget(body);
    QVBoxLayout* codeLayout = new QVBoxLayout(codeBlock);
    codeLayout->setContentsMargins(pad, uiScale().y(10), pad, uiScale().y(10));
    codeLayout->setSpacing(0);
    QVector<QLabel*> lineLabels;
    const QStringList codeLines = card.code.split('\n');
    for (const QString& lineText : codeLines)
    {
        QLabel* line = new QLabel(
            lineText.isEmpty() ? QString("&nbsp;")
                               : SonicPi::TutorialDocs::highlightCode(lineText, colours),
            codeBlock);
        line->setObjectName(QStringLiteral("qsCodeLine"));
        line->setTextFormat(Qt::RichText);
        // A rich-text label exposes its raw markup to assistive technology;
        // the accessible name must be the plain code line. (Blank spacer
        // lines stay unnamed and are pruned from the tree.)
        line->setAccessibleName(lineText);
        // Not selectable: selection would swallow the mouse press, and the
        // whole card is a drag surface.
        line->setStyleSheet(QString("font-size: %1px;").arg(m_codeFontPx));
        codeLayout->addWidget(line);
        lineLabels << line;
    }
    m_codeLines[workspace] = lineLabels;

    // Record the live_loop names this card defines, so focus can follow a
    // loop when another card redefines it.
    QSet<QString> loops;
    static const QRegularExpression llRe(
        QStringLiteral("live_loop\\s+[:\"]([A-Za-z0-9_]+)"));
    auto m = llRe.globalMatch(card.code);
    while (m.hasNext())
        loops.insert(m.next().captured(1));
    m_cardLoops[workspace] = loops;
    // widgetResizable stretches the block to fill the viewport, so a tail
    // stretch keeps a short snippet's lines packed at the top instead of
    // spreading them down the body.
    codeLayout->addStretch(1);
    body->setWidget(codeBlock);

    QWidget* footer = new QWidget(frame);
    footer->setObjectName(QStringLiteral("qsCardFooter"));
    footer->setFixedHeight(m_footerH); // uniform across all decks
    // Footer: the description filling the left, a full-height scope on the
    // right (the Run/Add buttons live in the header).
    QHBoxLayout* footerLayout = new QHBoxLayout(footer);
    footerLayout->setContentsMargins(pad, uiScale().y(8), pad, uiScale().y(8));
    footerLayout->setSpacing(uiScale().y(12));

    // Italic description at the code's size. Top-aligned so every card's
    // description starts at the same spot however many lines it wraps to.
    QLabel* blurb = new QLabel(card.blurb, footer);
    blurb->setObjectName(QStringLiteral("qsCardBlurb"));
    blurb->setTextFormat(Qt::RichText);
    // Plain prose for assistive technology (rich-text labels expose markup).
    blurb->setAccessibleName(
        QTextDocumentFragment::fromHtml(card.blurb).toPlainText().simplified());
    blurb->setWordWrap(true);
    blurb->setMaximumWidth(m_blurbW);
    blurb->setStyleSheet(QString("font-size: %1px;").arg(m_codeFontPx));
    footerLayout->addWidget(blurb, 1, Qt::AlignTop);

    // The scope sits centred in a hit box spanning the footer's full inner
    // height: the scope square is inset so its rings never clip, but the
    // whole region reads as the control, so the click/hover target must not
    // stop at the drawn rings.
    QWidget* scopeHit = new QWidget(footer);
    const int hitSide = m_footerH - 2 * uiScale().y(8); // footer inner height
    scopeHit->setFixedSize(hitSide, hitSide);
    QGridLayout* hitLay = new QGridLayout(scopeHit);
    hitLay->setContentsMargins(0, 0, 0, 0);
    CardScope* scope = new CardScope(scopeHit);
    scope->setFixedSize(m_scopeSide, m_scopeSide);
    scope->setColours(accent, SonicPiTheme::blend(fg, accent, 0.5));
    scope->setSlot(scopeSlot);
    m_scopes[workspace] = scope;
    hitLay->addWidget(scope, 0, 0, Qt::AlignCenter);
    footerLayout->addWidget(scopeHit, 0, Qt::AlignVCenter);

    // The scope IS the play/stop control: a transparent button fills the hit
    // box, with an outline play/stop ring in the centre and the audio rings
    // drawn around it. Clicking anywhere on or around the rings toggles the
    // card. The tabler ring spans 18 of its 24 grid, so the icon box is sized
    // up to keep the drawn circle smaller than the audio rings with a clear
    // gap.
    const int discD = int(m_scopeSide * 0.52);
    QPushButton* run = new QPushButton(scopeHit);
    run->setObjectName(QStringLiteral("qsCardRun"));
    run->setCursor(Qt::PointingHandCursor);
    // A QPushButton's vertical size policy is Fixed, so a layout never
    // stretches it to the cell — pin it to the hit box explicitly or the
    // clickable area collapses to a button-height band across the middle.
    run->setFixedSize(hitSide, hitSide);
    run->setIconSize(QSize(discD, discD));
    // Register normal + hover discs so the polling hover swaps them like the
    // other icon buttons.
    m_iconNormal[run] = QIcon(playDisc(playing, discD, false));
    m_iconHover[run] = QIcon(playDisc(playing, discD, true));
    run->setIcon(m_iconNormal.value(run));
    run->setAccessibleName(playing ? tr("Stop %1").arg(card.title) : tr("Run %1").arg(card.title));
    run->setToolTip(tr("Play this card. Press again to stop."));
    hitLay->addWidget(run, 0, 0); // overlays the scope and takes the mouse
    connect(run, &QPushButton::clicked, this, [this, title, snippet, workspace, scopeSlot, frame] {
        // Playing a card also selects it (the click would otherwise leave
        // focus — and the selection ring — on the button, not the card).
        frame->setFocus(Qt::OtherFocusReason);
        if (m_jobs.contains(workspace))
            emit stopJobRequested(m_jobs.value(workspace));
        else
            emit runRequested(title, snippet, workspace, scopeSlot);
    });
    m_runButtons[workspace] = run;

    // The header's action buttons, created after the content so a screen
    // reader meets the card as title → code → description → actions, then
    // placed into the header bar's grid (they still render at the top).

    // Add: a plus that drops the card's code into the editor. Hovering
    // projects a live preview into the editor (see updateHover); the click
    // commits it.
    QPushButton* add = new QPushButton(frame);
    add->setObjectName(QStringLiteral("qsCardBtn"));
    add->setCursor(Qt::PointingHandCursor);
    add->setFixedSize(btnW, btnH);
    add->setIconSize(QSize(iconPx, iconPx));
    add->setAccessibleName(tr("Add %1 to the editor at the cursor").arg(card.title));
    add->setToolTip(tr("Add this card to your code at the cursor."));
    setupIconBtn(add, TablerIcons::Glyph::SquareChevronsUp);
    m_addCode[add] = payload;
    m_addTitle[add] = addTitle;
    connect(add, &QPushButton::clicked, this,
            [this, addTitle, payload] { emit insertRequested(addTitle, payload); });

    // Copy: the card's code (unwrapped) onto the clipboard. The glyph flashes
    // to a tick so the copy visibly registered right where it was clicked.
    QPushButton* copy = new QPushButton(frame);
    copy->setObjectName(QStringLiteral("qsCardBtn"));
    copy->setCursor(Qt::PointingHandCursor);
    copy->setFixedSize(btnW, btnH);
    copy->setIconSize(QSize(iconPx, iconPx));
    copy->setAccessibleName(tr("Copy %1 to the clipboard").arg(card.title));
    copy->setToolTip(tr("Copy this card's code to the clipboard."));
    setupIconBtn(copy, TablerIcons::Glyph::Copy);
    connect(copy, &QPushButton::clicked, this,
            [this, addTitle, snippet, copy, iconPx, hoverInk, onAccent] {
                emit copyRequested(addTitle, snippet);
                const QIcon prevN = m_iconNormal.value(copy);
                const QIcon prevH = m_iconHover.value(copy);
                m_iconNormal[copy] = svgIcon(TablerIcons::Glyph::Check, onAccent, iconPx);
                m_iconHover[copy] = svgIcon(TablerIcons::Glyph::Check, hoverInk, iconPx);
                copy->setIcon(copy->underMouse() ? m_iconHover.value(copy)
                                                 : m_iconNormal.value(copy));
                QPointer<QPushButton> alive(copy);
                QTimer::singleShot(1400, this, [this, alive, prevN, prevH] {
                    if (!alive || !m_iconNormal.contains(alive))
                        return; // rebuilt (deck/zoom/theme change) meanwhile
                    m_iconNormal[alive] = prevN;
                    m_iconHover[alive] = prevH;
                    alive->setIcon(alive->underMouse() ? prevH : prevN);
                });
            });

    // Drag: an explicit drag handle in the top-left corner with its own
    // tooltip. The whole card is draggable, but this makes it obvious. Wired
    // into the drag system like the card body so grabbing it drops the card's
    // code on the editor.
    QPushButton* drag = new QPushButton(frame);
    drag->setObjectName(QStringLiteral("qsCardBtn"));
    drag->setProperty("corner", "tr"); // outer corner rounded to match the card
    drag->setCursor(Qt::OpenHandCursor);
    drag->setFixedSize(btnW, btnH);
    drag->setIconSize(QSize(iconPx, iconPx));
    drag->setToolTip(tr("Drag me into your editor."));
    setupIconBtn(drag, TablerIcons::Glyph::Texture);
    // A drag handle only means something to a pointer: keep it out of the
    // Tab ring (it does nothing on Enter) and out of the accessibility tree
    // (the Add button and the I shortcut are its keyboard equivalents).
    drag->setFocusPolicy(Qt::NoFocus);
    drag->setProperty("a11yIgnored", true);
    // Buttons accept the press, so the frame's filter never sees it; the
    // handle needs its own filter to start drags.
    drag->installEventFilter(this);
    m_dragCode[drag] = payload;
    m_dragFrames[drag] = frame;

    headerGrid->addWidget(copy, 0, 3, Qt::AlignVCenter);
    headerGrid->addWidget(add, 0, 4, Qt::AlignVCenter);
    headerGrid->addWidget(drag, 0, 5, Qt::AlignVCenter);

    CardHoverFx fx;
    fx.footer = footer;
    fx.blurb = blurb;
    fx.body = body;
    fx.title = card.title;
    m_cardFx[frame] = fx;

    // Cards are picked up anywhere on their face and dropped on the editor:
    // the drop preview makes the insertion point visible, so no clipboard
    // or cursor concepts are needed.
    frame->setCursor(Qt::OpenHandCursor);
    header->setCursor(Qt::OpenHandCursor);
    // No whole-card tooltip: the drag handle button carries the "drag me" hint
    // (a small widget the tooltip manager anchors to, not the big card frame).
    for (QObject* handle : { (QObject*)header, (QObject*)heading, (QObject*)frame })
    {
        handle->installEventFilter(this);
        m_dragCode[handle] = payload;
        m_dragFrames[handle] = frame;
    }

    // Keyboard/screen-reader path: the card itself is focusable; Space
    // plays or stops it and I inserts the code at the editor's cursor, the
    // accessible equivalent of the drag. The context menu offers the same
    // actions for discoverability. The name — "<title> card, n of m. <blurb>"
    // — is set by rebuild, which knows the card's position in the deck; the
    // code is read from the child widgets.
    frame->setFocusPolicy(Qt::TabFocus);
    frame->setAccessibleDescription(tr(
        "Press Space to play or stop, C to hear the code, I to insert it into the editor."));
    m_frameWs[frame] = workspace;
    frame->setContextMenuPolicy(Qt::CustomContextMenu);
    const QString cardTitle = card.title;
    const QString cardCode = payload;
    connect(frame, &QWidget::customContextMenuRequested, this,
            [this, frame, workspace, cardTitle, cardCode](const QPoint& pos) {
                QMenu menu(frame);
                QAction* runAct =
                    menu.addAction(m_jobs.contains(workspace) ? tr("Stop") : tr("Run"));
                QAction* insertAct = menu.addAction(tr("Insert at Cursor in Editor"));
                QAction* chosen = menu.exec(frame->mapToGlobal(pos));
                if (chosen == runAct && m_runButtons.contains(workspace))
                    m_runButtons.value(workspace)->click();
                else if (chosen == insertAct)
                    emit insertRequested(cardTitle, cardCode);
            });

    cardLayout->addWidget(footer);
    if (playing)
        scope->start(m_spAPI.get());

    // No blanket child filters: hover is poll-driven, and presses on the
    // (mouse-ignoring) labels/body propagate up to the frame's own filter.
    // Only widgets that ACCEPT the press (the drag handle, installed above)
    // need their own filter.

    return frame;
}
