//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#ifndef QUICKSTARTPANE_H
#define QUICKSTARTPANE_H

#include <QHash>
#include <QIcon>
#include <QSet>
#include <QWidget>

#include <memory>

#include "dpi.h"
#include "utils/tablericons.h"

class SonicPiTheme;
class QGridLayout;
class QPushButton;
class QScrollArea;
class QTimer;
class CardScope; // circular stereo mini scope (defined in the .cpp)

namespace SonicPi
{
struct QuickstartCard;
class SonicPiAPI;
}

// Quickstart cheat-sheet tab: decks of snap cards (intro, performance,
// synth design, ...), each card one core idea as a small syntax-highlighted
// snippet with Run/Copy buttons and a live mini scope. Styled like the
// printed cheat sheets: accent header bar, code body, tinted footer.
class QuickstartPane : public QWidget
{
    Q_OBJECT
public:
    // First scope-buffer slot for card runs. Each visible card gets its own
    // slot (kFirstScopeSlot + card index, wrapping within kCardScopeSlots) so
    // its rings show only its own audio; slots 0 = master, 1 = jukebox are
    // reserved, and slots 10+ belong to live_loop mini-scopes. Must stay in
    // sync with the live-loop slot range in runtime.rb (__live_loop_scope_slot).
    static constexpr unsigned int kFirstScopeSlot = 2;
    static constexpr unsigned int kCardScopeSlots = 8;

    explicit QuickstartPane(SonicPiTheme* theme, QWidget* parent = nullptr);

    void setAudioApi(std::shared_ptr<SonicPi::SonicPiAPI> api) { m_spAPI = api; }

    // Path to the cards file (etc/quickstart/cards.txt or the user override);
    // set by MainWindow. Rebuilds the pane from it.
    void setCardsFile(const QString& path)
    {
        m_cardsPath = path;
        m_deckPages.clear(); // a fresh set starts every deck on its first card
        m_pageIndex = 0;
        rebuild();
    }

    void applyTheme(); // rebuild in the current theme colours

    // Land keyboard/screen-reader focus on the carousel and say where it is
    // — the menu path in (Examples > Quickstart Cards...) calls this so
    // arriving here behaves like the Focus menu's jumps.
    void focusCarousel();

    // True if `path` is a readable card set with at least one deck. On failure
    // sets *error to a human-readable reason (used by the Load Card Set menu).
    static bool validateCardsFile(const QString& path, QString* error);

    // Pane-local zoom steps (A-/A+), persisted as a pref by MainWindow
    int userZoom() const { return m_userZoom; }
    void setUserZoom(int zoom);

    // Dock height that shows the header plus one full card row (first-boot
    // sizing); cards carousel horizontally, so no more height is needed.
    int preferredDockHeight() const;

    // The A-/A+ zoom controls (circle -/+), created once and displayed in the
    // dock title row (like the Docs tab), so all tabs' controls share a row.
    QWidget* zoomControls();

    // Wire to QtAPIClient's RunStartedReceived/RunEndedReceived/FlashReceived
    void runStarted(int jobId, const QString& workspace);
    void runEnded(int jobId);
    // Editor-style trigger wash on the card's code line. `line` is the
    // 1-based runtime line within the submitted (scope-tap wrapped) code.
    void flashLine(const QString& workspace, int line);

signals:
    void runRequested(const QString& title, const QString& code, const QString& workspace,
                      int scopeSlot);
    void stopJobRequested(int jobId);
    // Keyboard/menu path to the editor (the accessible equivalent of
    // dragging a card): insert the code at the editor's cursor.
    void insertRequested(const QString& title, const QString& code);
    // Copy the card's code to the clipboard (MainWindow copies + announces).
    void copyRequested(const QString& title, const QString& code);
    // Hovering the add button projects the code as a live preview in the editor
    // (like a drag-over); leaving reverts it. The click commits via insertRequested.
    void insertPreviewRequested(const QString& title, const QString& code);
    void insertPreviewCleared();
    // A card drag finished (dropped or fumbled); commit any live preview.
    void dragEnded();
    // Spoken feedback for screen readers (page changes, deck switches);
    // MainWindow relays it through announce().
    void announceRequested(const QString& msg);

protected:
    // The hover poll runs only while the pane is visible.
    void showEvent(QShowEvent* event) override;
    void hideEvent(QHideEvent* event) override;
    // As the dock shrinks below one card, shed the header (description, then
    // title) so the card itself is never clipped.
    void resizeEvent(QResizeEvent* event) override;
    // Drag a card by its header to drop its code on the editor
    bool eventFilter(QObject* obj, QEvent* event) override;

private:
    // Card width grows with zoom (same factor as the font) so A+/A- scale
    // the whole card, not just its text.
    int cardWidth() const;
    // Measure every card in every deck at the current zoom so all cards share
    // one code font (small enough that the widest line never clips; there is
    // no horizontal scroll), one code-area height and one footer height.
    void computeGlobalLayout();
    void updateHeaderForHeight(); // hide description/title as the dock shrinks
    // Carousel: whole-card snap paging with a page-dot indicator.
    int rowsThatFit() const;     // whole card rows that fit the viewport height
    int cardsPerView() const;    // whole card columns that fit the viewport width
    int pageCount() const;
    void relayoutGrid(int rows); // (re)place the cards column-major in `rows` rows
    void updateCarousel();       // recompute paging, grid, dots; snap
    void rebuildDots();          // repaint dots + arrow state for the current page
    void updateBackEdge();       // show/position the left-edge page-back scrim
    void goToPage(int page);     // animate (or jump under reduce motion) to a page; wraps at the ends
    void announcePage();         // tell a screen reader where paging landed
    QWidget* firstVisibleCard() const; // the current page's lead card (focus entry point)
    void scrollCardIntoView(QWidget* frame); // page toward a partly-visible card
    void rebuild();
    QWidget* addCard(const SonicPi::QuickstartCard& card, const QString& workspace, int scopeSlot);
    void setCardPlaying(const QString& workspace, bool playing);
    // Sizes come from the shared type scale (dpi.h), zoomed. Raw numbers here
    // put the pane on its own ladder — 15 and 17 sat between the scale's steps,
    // so this pane's text never lined up with anything else in the window.
    int rolePx(FontRole role) const;
    // Cached FontZoomFactor(m_userZoom): uiScale()/fontPx() are called ~60
    // times per card rebuild and from hit-test paths, and recomputing a
    // std::pow each time is pure waste. Refreshed in setUserZoom().
    double m_zoomFactor = 1.0;
    // Zoom-aware pixel metrics for the card format (shared with the docs
    // pane via dpi.h) — card geometry and padding grow with the type, so
    // zooming adds room rather than crowding the content that grew.
    UiScale uiScale() const { return UiScale(m_zoomFactor); }
    // A Tabler-icon glyph rendered in one colour at the given size.
    QIcon svgIcon(TablerIcons::Glyph glyph, const QColor& colour, int px) const;
    // A solid accent disc with a centred play/stop glyph; the scope's control.
    QPixmap playDisc(bool playing, int d, bool hover = false) const;
    // Rings follow the play glyph's hover colour (the whole scope is the
    // click target, so it lights up as one control).
    void setScopeHover(QPushButton* button, bool hover);
    // The drag projection: a bordered code card with the card's title drawn
    // straddling the top border (legend style).
    QPixmap cardDragPixmap(QWidget* frame) const;

    // Hover feedback, driven by polling the cursor rather than Enter/Leave
    // events: updateHover() hit-tests the pointer against the cards and icon
    // buttons every tick and restyles only on change, so it can never stick.
    void updateHover();
    void setCardHover(QWidget* frame, bool on);

    // The A-/A+ zoom controls, shown in the dock title row (see zoomControls()).
    // Persistent across rebuilds; retinted on theme change.
    class ZoomBar* m_zoomBar = nullptr;

    SonicPiTheme* m_theme;
    std::shared_ptr<SonicPi::SonicPiAPI> m_spAPI;
    QWidget* m_topBar = nullptr;
    QWidget* m_navRow = nullptr; // centred carousel-control row beneath the cards
    QScrollArea* m_scroll = nullptr;
    // Carousel state: whole-card snap paging. Each deck remembers its own
    // position (m_deckPages); every deck starts on its first card.
    int m_pageIndex = 0;
    QHash<int, int> m_deckPages; // deck index -> last viewed page
    int m_cardCount = 0;
    QWidget* m_cardRow = nullptr;        // scroll content (holds the card grid)
    class QGridLayout* m_cardGrid = nullptr;
    QVector<QWidget*> m_cardFrames;      // card frames in deck order
    QWidget* m_gridSpacer = nullptr;     // trailing pad so the last page snaps clean
    int m_gridRows = 0;                  // rows the grid is currently laid out with
    QPushButton* m_prevArrow = nullptr;
    QPushButton* m_nextArrow = nullptr;
    QWidget* m_backEdge = nullptr; // invisible left-margin "page back" click zone
    QWidget* m_dotsHost = nullptr;
    class QHBoxLayout* m_dotsLayout = nullptr;
    // Header pieces, hidden progressively as the dock shrinks (resizeEvent).
    QWidget* m_headerRail = nullptr;
    class QLabel* m_deckTitle = nullptr;
    class QLabel* m_deckDesc = nullptr;
    QString m_cardsPath;
    int m_deckIdx = 0;
    int m_userZoom = 0;
    int m_cardHeight = 0;    // uniform card height (header + body + footer)
    int m_codeFontPx = 0;    // code font, shrunk so the widest line fits
    int m_codeBodyH = 0;     // uniform code-area height across all decks
    int m_footerH = 0;       // uniform footer height across all decks
    int m_scopeSide = 0;     // scope square side = footer inner height
    int m_blurbW = 0;        // fixed blurb width (so wrap height is deterministic)
    int m_layoutZoom = 999;  // m_userZoom the cached layout was computed for

    // Per-card widgets for the current deck, keyed by workspace; job state
    // (m_jobs) outlives rebuilds so a playing card survives zoom/theme/deck
    // changes.
    QHash<QString, QPushButton*> m_runButtons;
    QHash<QString, CardScope*> m_scopes;
    QHash<QString, QVector<class QLabel*>> m_codeLines;
    // The code body, which scrolls when a snippet overruns the card's fixed
    // budget; held so a flashed line can be brought back into view.
    QHash<QString, class QScrollArea*> m_codeScrolls;
    QHash<QString, QSet<QString>> m_cardLoops; // workspace -> live_loop names in its code
    QHash<QString, int> m_jobs;
    // Card hover: a high-contrast border lights up while the pointer is
    // anywhere over the card ([cardHover] in app.qss). Poll-driven (see
    // updateHover) so crossing child widgets never flickers or sticks.
    struct CardHoverFx
    {
        QWidget* footer = nullptr;
        class QLabel* blurb = nullptr;
        QWidget* body = nullptr; // code area, grabbed for the drag ghost
        QString title;
    };
    QHash<QWidget*, CardHoverFx> m_cardFx; // keyed by card frame
    QTimer* m_hoverTimer = nullptr;
    // Theme changed while the pane was hidden: the deck rebuild (the
    // expensive part of applyTheme) is deferred to the next showEvent.
    bool m_themeDirty = false;
    QWidget* m_hoverCard = nullptr;        // card currently under the pointer
    QPushButton* m_hoverIcon = nullptr;    // icon button currently under the pointer
    // Icon-button glyph swap on hover (normal white glyph <-> contrasting ink).
    QHash<QObject*, QIcon> m_iconNormal;
    QHash<QObject*, QIcon> m_iconHover;
    QHash<QObject*, QString> m_dragCode;   // drag handle (card header) -> snippet
    QHash<QObject*, QWidget*> m_dragFrames; // drag handle -> card frame (drag image)
    QHash<QObject*, QString> m_addCode;    // add button -> snippet (hover preview)
    QHash<QObject*, QString> m_addTitle;   // add button -> card title
    QHash<QObject*, QString> m_frameWs;    // card frame -> workspace (keyboard control)
    QPoint m_dragStart;
    QObject* m_dragSource = nullptr;
    // A plain click on a partly-visible card scrolls it fully into view.
    QWidget* m_clickFrame = nullptr;
    QPoint m_clickPos;
    // Side-scrolling pages one card per gesture; the cooldown swallows a
    // trackpad swipe's momentum so it doesn't fly through the deck.
    bool m_wheelCooldown = false;
};

#endif // QUICKSTARTPANE_H
