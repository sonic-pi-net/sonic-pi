//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright (C) 2026 by Sam Aaron
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#ifndef QUICKSTARTPROGRESS_H
#define QUICKSTARTPROGRESS_H

// How far through the QuickStart decks someone is, dealt as the web's are (app/web/app/src/quickstart.js): a deck
// opens on its welcome card, Start deals its first card, and the tile after the newest card deals the next; the decks
// are reached in order, the end of one dealing the next. Shown a page at a time: the welcome card is item 0 and card
// k is item k + 1; once in a deck, a tile ends the last page while a card or a deck is still to come. Pure: gui-tests
// pins it, and QuickstartPane draws what it says.

#include <QVector>

#include <algorithm>

namespace SonicPi
{
class QuickstartProgress
{
public:
    enum class Tile { None, NextCard, NextDeck };
    enum class Start { Start, CarryOn, Again };   // the welcome card's call: Start deck, Carry on, Play through again
    enum class Moved { None, Page, Dealt, NewDeck };

    // What a page shows: the welcome card, cards [from, to) of the deck, and the tile
    struct Page
    {
        bool welcome = false;
        int from = 0;
        int to = 0;
        Tile tile = Tile::None;
    };

    // The decks' sizes, in order. Progress kept for them is restored after.
    void setDecks(QVector<int> cards)
    {
        m_cards = std::move(cards);
        m_dealt = QVector<int>(m_cards.size(), 0);
        m_deck = 0;
        m_reached = 0;
        m_inDeck = false;
        m_page = 0;
    }

    // What was kept: the deck open, how many of each deck's cards were dealt, the furthest deck reached. A deck under
    // way counts as reached, so no one loses a deck they had; all of it held within this set.
    void restore(int deck, const QVector<int>& dealt, int reached)
    {
        const int last = std::max(0, int(m_cards.size()) - 1);
        m_deck = std::clamp(deck, 0, last);
        m_reached = std::clamp(std::max(reached, m_deck), 0, last);
        for (int i = 0; i < m_cards.size(); ++i)
        {
            m_dealt[i] = std::clamp(i < dealt.size() ? dealt[i] : 0, 0, m_cards[i]);
            if (m_dealt[i] > 0)
                m_reached = std::max(m_reached, i);
        }
        m_inDeck = false;
        m_page = 0;
    }

    int deck() const { return m_deck; }
    int cards() const { return m_cards.value(m_deck, 0); }
    int dealt(int deck) const { return m_dealt.value(deck, 0); }
    int dealt() const { return dealt(m_deck); }
    QVector<int> dealtAll() const { return m_dealt; }
    int reached() const { return m_reached; }
    bool inDeck() const { return m_inDeck; }
    int page() const { return m_page; }
    // The page on show at `perPage` cards a page: the one kept, within the pages a new width leaves
    int page(int perPage) const { return clamped(m_page, perPage); }

    // The decks to choose between: shown out of a deck once there is more than one
    bool decksShown() const { return !m_inDeck && m_reached > 0; }
    bool deckShown(int deck) const { return deck <= m_reached; }

    bool hasNextDeck() const { return dealt() >= cards() && m_deck < int(m_cards.size()) - 1; }

    Start start() const { return dealt() == 0 ? Start::Start : dealt() < cards() ? Start::CarryOn : Start::Again; }

    // Out of a deck, only its welcome card. In one, the welcome card, the cards dealt, and the tile while a card or a
    // deck is to come; a row one card wide has no room for the tile, and Next deals from the newest card.
    int items(int perPage) const
    {
        if (!m_inDeck)
            return 1;
        return 1 + dealt() + (tileWanted() && perPage > 1 ? 1 : 0);
    }
    int pages(int perPage) const
    {
        const int per = std::max(1, perPage);
        return std::max(1, (items(per) + per - 1) / per);
    }
    // Where a page starts: a page's worth at a time, but the last page ends on the last item, so the tile always has
    // the newest card beside it and a card dealt into a full page moves the view along by one.
    int first(int page, int perPage) const
    {
        const int per = std::max(1, perPage);
        return page == pages(per) - 1 ? std::max(0, items(per) - per) : page * per;
    }

    Page shown(int perPage) const
    {
        const int per = std::max(1, perPage);
        const int page = clamped(m_page, per);
        const int from = first(page, per);
        Page p;
        p.welcome = from == 0;
        if (m_inDeck)
        {
            p.from = std::max(0, from - 1);
            p.to = std::max(p.from, std::min(dealt(), from + per - 1));
            if (page == pages(per) - 1 && per > 1)
                p.tile = dealt() > 0 && dealt() < cards() ? Tile::NextCard : hasNextDeck() ? Tile::NextDeck : Tile::None;
        }
        return p;
    }

    bool canBack() const { return m_page > 0; }
    bool canForward(int perPage) const { return m_page < pages(perPage) - 1 || dealt() < cards() || hasNextDeck(); }

    // A deck button: its welcome card
    void choose(int deck)
    {
        m_deck = std::clamp(deck, 0, std::max(0, int(m_cards.size()) - 1));
        m_inDeck = false;
        m_page = 0;
    }

    // The welcome card's call: Start deals the first card; part-way, Carry on goes to the newest one; all dealt, Play
    // through again goes back to the first.
    void startDeck(int perPage)
    {
        if (dealt() == 0)
        {
            deal(perPage);
            return;
        }
        m_inDeck = true;
        m_page = dealt() < cards() ? pages(perPage) - 1 : 1 / std::max(1, perPage);
    }

    // The Next card tile
    void deal(int perPage)
    {
        if (dealt() >= cards())
            return;
        ++m_dealt[m_deck];
        m_inDeck = true;
        m_page = pages(perPage) - 1;
    }

    // The Next deck tile: the next deck is reached, on its welcome card
    void nextDeck()
    {
        if (m_deck >= int(m_cards.size()) - 1)
            return;
        ++m_deck;
        m_reached = std::max(m_reached, m_deck);
        m_inDeck = false;
        m_page = 0;
    }

    // Back to the decks: the deck's welcome card
    void leave()
    {
        m_inDeck = false;
        m_page = 0;
    }

    // The arrows and the dots: true if the page moved. Moving off the welcome card goes into the deck; a move that
    // goes nowhere leaves things as they were.
    bool turn(int by, int perPage) { return goTo(m_page + by, perPage); }
    bool goTo(int page, int perPage)
    {
        const int was = m_page;
        const bool wasIn = m_inDeck;
        if (page == was)
            return false;
        m_inDeck = true;
        m_page = clamped(page, perPage);
        if (m_page == was)
            m_inDeck = wasIn;
        return m_page != was;
    }

    // Next: the next page, or at the end the next card, then the next deck
    Moved forward(int perPage)
    {
        if (m_page < pages(perPage) - 1)
            return turn(1, perPage) ? Moved::Page : Moved::None;
        if (hasNextDeck())
        {
            nextDeck();
            return Moved::NewDeck;
        }
        if (dealt() < cards())
        {
            deal(perPage);
            return Moved::Dealt;
        }
        return Moved::None;
    }

private:
    bool tileWanted() const { return (dealt() > 0 && dealt() < cards()) || hasNextDeck(); }
    int clamped(int page, int perPage) const { return std::clamp(page, 0, pages(perPage) - 1); }

    QVector<int> m_cards;
    QVector<int> m_dealt;
    int m_deck = 0;
    int m_reached = 0;
    bool m_inDeck = false;
    int m_page = 0;
};
} // namespace SonicPi

#endif // QUICKSTARTPROGRESS_H
