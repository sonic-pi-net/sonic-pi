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

#include <QVector>
#include <QWidget>

#include <memory>

#include "dpi.h"
#include "model/quickstartprogress.h"

class ChevronButton;
class SonicPiTheme;
class QHBoxLayout;
class QLabel;
class QPushButton;

namespace SonicPi
{
struct QuickstartCard;
class SonicPiAPI;
}

// The QuickStart cards: decks of short programs, each card one idea as a small highlighted snippet with Play and Stop
// and its own scope. Dealt as the web's are (QuickstartProgress): a deck opens on its welcome card, Start deals its
// first card, the tile after the newest card deals the next, and the end of a deck deals the next deck. A page at a
// time, in one row; the deck buttons, or in a deck the bar with its name and pages, above.
class QuickstartPane : public QWidget
{
    Q_OBJECT
public:
    // First scope-buffer slot for card runs. Each card gets its own slot (kFirstScopeSlot + card index, wrapping
    // within kCardScopeSlots) so its rings show only its own audio; slots 0 = master, 1 = jukebox are reserved, and
    // slots 10+ belong to live_loop mini-scopes. Must stay in sync with the live-loop slot range in runtime.rb
    // (__live_loop_scope_slot).
    static constexpr unsigned int kFirstScopeSlot = 2;
    static constexpr unsigned int kCardScopeSlots = 8;

    explicit QuickstartPane(SonicPiTheme* theme, QWidget* parent = nullptr);

    void setAudioApi(std::shared_ptr<SonicPi::SonicPiAPI> api);

    // Path to the cards file (etc/quickstart/cards.txt or the user override); set by MainWindow. Rebuilds the pane
    // from it, with the progress kept for that set (another set starts afresh).
    void setCardsFile(const QString& path);

    void applyTheme(); // rebuild in the current theme colours

    // Land keyboard/screen-reader focus on the page's first card (or the welcome card's Start) and say where it is:
    // the menu path in (Examples > Quickstart Cards...) calls this so arriving here behaves like the Focus menu's jumps.
    void focusCarousel();

    // True if `path` is a readable card set with at least one deck. On failure sets *error to a human-readable reason
    // (used by the Load Card Set menu).
    static bool validateCardsFile(const QString& path, QString* error);

    // The pane's text and layout this many 1.1× steps from its own size (UiScale): the help's text size
    // (MainWindow::setHelpZoom).
    int userZoom() const { return m_userZoom; }
    void setUserZoom(int zoom);

    // Dock height that shows the deck bar plus one row of cards (first-boot sizing).
    int preferredDockHeight() const;

    // Wire to QtAPIClient's RunStartedReceived/RunEndedReceived/FlashReceived
    void runStarted(int jobId, const QString& workspace);
    void runEnded(int jobId);
    // Editor-style trigger wash on the card's code line. `line` is the 1-based runtime line within the submitted
    // (scope-tap wrapped) code.
    void flashLine(const QString& workspace, int line);
    // How bright the cards' washes are: Flash Brightness, a percent.
    void setFlashBrightness(int percent);
    // What one of a card's runs puts, and the error that ended one, shown in the card's footer. `line` is the run's,
    // counted as flashLine's is.
    void runOutput(int jobId, const QString& text);
    void runError(int jobId, const QString& message, int line);

signals:
    void runRequested(const QString& title, const QString& code, const QString& workspace,
                      int scopeSlot);
    void stopJobRequested(int jobId);
    // Keyboard/menu path to the editor (the accessible equivalent of dragging a card): insert the code at the
    // editor's cursor.
    void insertRequested(const QString& title, const QString& code);
    // Copy the card's code to the clipboard (MainWindow copies + announces).
    void copyRequested(const QString& title, const QString& code);
    // Hovering the add button projects the code as a live preview in the editor (like a drag-over); leaving reverts
    // it. The click commits via insertRequested.
    void insertPreviewRequested(const QString& title, const QString& code);
    void insertPreviewCleared();
    // A card drag finished (dropped or fumbled); commit any live preview.
    void dragEnded();
    // Spoken feedback for screen readers, where the focus does not say it (a page turned, a deck reached);
    // MainWindow relays it through announce().
    void announceRequested(const QString& msg);

protected:
    // A theme change while hidden rebuilds the deck on the next show.
    void showEvent(QShowEvent* event) override;
    // A width that fits more or fewer cards redraws the page.
    void resizeEvent(QResizeEvent* event) override;
    // Left and Right, from anywhere in the pane: the next or the previous card, turning the page at its ends
    void keyPressEvent(QKeyEvent* event) override;
    // The wheel turns the pages, one a gesture
    void wheelEvent(QWheelEvent* event) override;
    // The wheel over a card's code, when the code has nothing to scroll
    bool eventFilter(QObject* obj, QEvent* event) override;

private:
    // Where the focus goes once a page is drawn, if it was in the pane
    enum class FocusTo
    {
        Keep,   // where it was, or the page's first item if that is gone
        Card,   // the card given
        Start,  // the welcome card's Start
        Tile,   // the tile
    };

    // Card width grows with zoom (same factor as the font) so the whole card scales, not just its text.
    int cardWidth() const;
    // Measure the cards at the current zoom so every card in every deck shares one code font (small enough that the
    // widest line never clips), one code-area height, one footer height and so one height.
    void computeGlobalLayout();
    int perPage() const;   // whole cards that fit the row
    void rebuild();        // the deck buttons, the bar, and the page
    void render(FocusTo focus = FocusTo::Keep, int card = -1);
    void rebuildDots(int pages);
    QWidget* welcomeCard();
    QWidget* nextTile();
    class CodeCard* addCard(int index);
    QWidget* focusTarget(QWidget* item) const;   // an item's way in: the welcome card's Start, or the item
    bool focusOnCard() const;
    void loadProgress();
    void saveProgress() const;

    // The controls, each as the web's does it
    void chooseDeck(int deck);
    void startDeck();
    void deal();
    void showDealt(int card);
    void nextDeck();
    void showNewDeck();
    void leave();
    void turn(int by);
    void forward();
    void goToPage(int page);
    void stepFrom(QWidget* item, int delta);   // Left/Right from an item: the next one, turning the page at the ends
    void sayPage(bool moved);                   // a page turned: where it landed, unless a card has the focus
    void pageByWheel(const QWheelEvent* event);

    // Sizes come from the shared type scale (dpi.h), zoomed.
    int rolePx(FontRole role) const;
    // Cached FontZoomFactor(m_userZoom), refreshed in setUserZoom().
    double m_zoomFactor = 1.0;
    // Zoom-aware pixel metrics for the card format (shared with the docs pane via dpi.h).
    UiScale uiScale() const { return UiScale(m_zoomFactor); }

    SonicPiTheme* m_theme;
    std::shared_ptr<SonicPi::SonicPiAPI> m_spAPI;
    QString m_cardsPath;
    SonicPi::QuickstartProgress m_progress;

    QWidget* m_head = nullptr;               // the deck buttons and the bar
    QWidget* m_decks = nullptr;              // the deck buttons, out of a deck
    QVector<QPushButton*> m_deckPills;
    QWidget* m_bar = nullptr;                // in a deck: Decks, its name and its pages
    QPushButton* m_exit = nullptr;
    QLabel* m_barTitle = nullptr;
    ChevronButton* m_prevArrow = nullptr;
    ChevronButton* m_nextArrow = nullptr;
    QWidget* m_dotsHost = nullptr;
    QHBoxLayout* m_dotsLayout = nullptr;
    QLabel* m_count = nullptr;               // where you are, in words, once the dots would crowd the bar
    QWidget* m_row = nullptr;                // the page
    QHBoxLayout* m_rowLayout = nullptr;
    QVector<QWidget*> m_items;               // the page's items in order: the welcome card, cards, the tile
    QVector<QWidget*> m_cardFrames;          // the page's code cards
    QPushButton* m_start = nullptr;          // the welcome card's Start, when it is on the page
    QPushButton* m_tile = nullptr;           // the tile, when it is on the page
    int m_laidOut = 0;                       // the cards a page the page was drawn for

    int m_userZoom = 0;
    int m_cardHeight = 0;    // uniform card height (header + body + footer)
    int m_codeFontPx = 0;    // code font, shrunk so the widest line fits
    int m_codeBodyH = 0;     // uniform code-area height across all decks
    int m_footerH = 0;       // uniform footer height across all decks
    int m_scopeSide = 0;     // scope square side = footer inner height
    int m_blurbW = 0;        // fixed blurb width (so wrap height is deterministic)
    int m_layoutZoom = 999;  // m_userZoom the cached layout was computed for

    // The cards' runs, which outlive a redraw, so a playing card survives a page turn, a zoom, a theme or a deck
    // change; and the cards' hover.
    class CardDeck* m_deck = nullptr;
    // Theme changed while the pane was hidden: the rebuild is deferred to the next showEvent.
    bool m_themeDirty = false;
    // One page a wheel gesture: the cooldown swallows a trackpad swipe's momentum.
    bool m_wheelCooldown = false;
};

#endif // QUICKSTARTPANE_H
