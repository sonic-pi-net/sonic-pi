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

#ifndef CARDDECK_H
#define CARDDECK_H

#include <QHash>
#include <QList>
#include <QObject>
#include <QPointer>
#include <QSet>
#include <QString>
#include <QTimer>

class CodeCard;
class QWidget;

namespace SonicPi
{
class SonicPiAPI;
}

// The runs a host's code cards start, as the web's deck.js keeps them: each
// card's Play and Stop are the deck's, and the session's run events come back
// to the card whose run they are. A card's runs outlive the card, so a host
// may make its cards again (a zoom, a theme) and they are adopted, playing.
//
// The deck drives its cards' hover too, polling the pointer while the host
// shows: a card can slide beneath a pointer that never moved (a carousel
// turning, a page scrolling).
class CardDeck : public QObject
{
    Q_OBJECT
public:
    // Layered: the cards play together, as a performance deck's loops stack.
    // OneAtATime: playing a card stops the one that played, as a page does.
    enum class Playing { Layered, OneAtATime };

    CardDeck(QWidget* host, Playing playing);

    // The rings draw each card's scope slot from here.
    void setAudioApi(SonicPi::SonicPiAPI* api) { m_api = api; }

    // A card joins, its runs going out on `workspace`.
    void add(CodeCard* card, const QString& workspace);
    // The cards are about to go: none is left lit. Their runs carry on.
    void clear();
    // Every run of every card.
    void stopAll();
    QList<CodeCard*> cards() const;

    // The session's events, for the deck's runs (others are ignored). A run's
    // lines count from 1, the first being the with_fx :scope_out a card's code
    // is wrapped in.
    void runStarted(int jobId, const QString& workspace);
    void runEnded(int jobId);
    void flashLine(const QString& workspace, int line);
    void runOutput(int jobId, const QString& text);
    void runError(int jobId, const QString& message, int line);

signals:
    void runRequested(const QString& title, const QString& code, const QString& workspace,
                      int scopeSlot);
    void stopJobRequested(int jobId);

protected:
    // The host showing and hiding: hover is polled only while it shows.
    bool eventFilter(QObject* obj, QEvent* event) override;

private:
    void play(const QString& workspace);
    void stop(const QString& workspace);
    void setPlaying(const QString& workspace, bool playing);
    CodeCard* cardOfJob(int jobId) const;
    void pollHover();

    QWidget* m_host;
    Playing m_playing;
    SonicPi::SonicPiAPI* m_api = nullptr;
    QHash<QString, QPointer<CodeCard>> m_cards; // by workspace
    // Every workspace the deck has had a card for: a run may start after its
    // card has gone (a deck switched while it booted), and is still the
    // deck's, for the card made again to adopt and stop.
    QSet<QString> m_workspaces;
    // Each card's runs, by workspace. A card taking another's live_loops over
    // inherits its runs: live_loop redefines the running thread rather than
    // starting one, so the sound stays with the run that first started it.
    QHash<QString, QSet<int>> m_jobs;
    QTimer m_hoverTimer;
};

#endif // CARDDECK_H
