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

#include "carddeck.h"

#include <QApplication>
#include <QCursor>
#include <QEvent>
#include <QWidget>

#include "widgets/codecard.h"

CardDeck::CardDeck(QWidget* host, Playing playing)
    : QObject(host), m_host(host), m_playing(playing)
{
    m_hoverTimer.setInterval(50);
    connect(&m_hoverTimer, &QTimer::timeout, this, &CardDeck::pollHover);
    host->installEventFilter(this);
    if (host->isVisible())
        m_hoverTimer.start();
}

void CardDeck::add(CodeCard* card, const QString& workspace)
{
    m_cards[workspace] = card;
    m_workspaces.insert(workspace);
    connect(card, &CodeCard::playRequested, this, [this, workspace] { play(workspace); });
    connect(card, &CodeCard::stopRequested, this, [this, workspace] { stop(workspace); });
    if (m_jobs.contains(workspace))
        card->setPlaying(true, m_api);
}

void CardDeck::clear()
{
    // A card projecting its code into the editor takes the preview with it,
    // rather than orphaning it.
    for (CodeCard* card : cards())
        card->hoverAt(QPoint(), false);
    m_cards.clear();
}

void CardDeck::stopAll()
{
    for (const QString& workspace : m_jobs.keys())
        stop(workspace);
}

QList<CodeCard*> CardDeck::cards() const
{
    QList<CodeCard*> all;
    for (const QPointer<CodeCard>& card : m_cards)
        if (card)
            all << card;
    return all;
}

// Play runs the card's code as it stands, edited or not; again while it plays,
// the new run takes over (a live_loop takes the new code). The arc goes round
// Play until the engine says the run started.
void CardDeck::play(const QString& workspace)
{
    CodeCard* card = m_cards.value(workspace);
    if (!card)
        return;
    if (m_playing == Playing::OneAtATime)
    {
        // The card that played lets go at once, as the web's deck does: its
        // runs are stopped and are no longer any card's.
        for (const QString& other : m_jobs.keys())
        {
            if (other == workspace)
                continue;
            stop(other);
            m_jobs.remove(other);
            setPlaying(other, false);
        }
    }
    card->setBooting(true);
    emit runRequested(card->title(), card->code(), workspace, static_cast<int>(card->spec().scopeSlot));
}

// Every run the card started (or took over). It shows stopped once the engine
// says they ended.
void CardDeck::stop(const QString& workspace)
{
    for (int jobId : m_jobs.value(workspace))
        emit stopJobRequested(jobId);
}

void CardDeck::setPlaying(const QString& workspace, bool playing)
{
    if (CodeCard* card = m_cards.value(workspace))
        card->setPlaying(playing, m_api);
}

void CardDeck::runStarted(int jobId, const QString& workspace)
{
    if (!m_workspaces.contains(workspace))
        return;
    m_jobs[workspace].insert(jobId);
    setPlaying(workspace, true);

    // When this card starts a live_loop, the server redefines that loop away
    // from any other card that runs it: release those cards, so their
    // transport and rings follow the handover. (Cards with loops of their
    // own, as in a layering deck, keep playing.)
    const CodeCard* card = m_cards.value(workspace);
    if (!card || card->loopNames().isEmpty())
        return;
    const QSet<QString> mine = card->loopNames();
    for (const QString& other : m_jobs.keys())
    {
        if (other == workspace)
            continue;
        const CodeCard* theirs = m_cards.value(other);
        if (!theirs || theirs->loopNames().isEmpty())
            continue;
        QSet<QString> left = theirs->loopNames();
        left.subtract(mine);
        if (left.isEmpty()) // every loop that card ran is now this card's
        {
            // Inherited rather than dropped: their run still owns the running
            // thread, so it is the one this card's Stop has to end.
            m_jobs[workspace].unite(m_jobs.take(other));
            setPlaying(other, false);
        }
    }
}

void CardDeck::runEnded(int jobId)
{
    for (auto it = m_jobs.begin(); it != m_jobs.end(); ++it)
    {
        if (!it.value().remove(jobId))
            continue;
        // A card stops only once every run it holds has ended: a hot-swapped
        // card outlives its own run, which ends as soon as the redefine lands.
        if (it.value().isEmpty())
        {
            const QString workspace = it.key();
            m_jobs.erase(it);
            setPlaying(workspace, false);
        }
        return;
    }
}

void CardDeck::flashLine(const QString& workspace, int line)
{
    if (CodeCard* card = m_cards.value(workspace))
        card->flashLine(line);
}

CodeCard* CardDeck::cardOfJob(int jobId) const
{
    for (auto it = m_jobs.cbegin(); it != m_jobs.cend(); ++it)
        if (it.value().contains(jobId))
            return m_cards.value(it.key());
    return nullptr;
}

void CardDeck::runOutput(int jobId, const QString& text)
{
    if (CodeCard* card = cardOfJob(jobId))
        for (const QString& line : text.split(QLatin1Char('\n')))
            card->appendOutput(line);
}

void CardDeck::runError(int jobId, const QString& message, int line)
{
    CodeCard* card = cardOfJob(jobId);
    if (!card)
        return;
    // The first of the run's lines is the with_fx :scope_out the card's code
    // is wrapped in.
    const int own = line - 1;
    card->setError(own > 0 ? tr("%1 (line %2)").arg(message).arg(own) : message);
}

bool CardDeck::eventFilter(QObject* obj, QEvent* event)
{
    if (obj == m_host)
    {
        if (event->type() == QEvent::Show)
            m_hoverTimer.start();
        else if (event->type() == QEvent::Hide)
        {
            m_hoverTimer.stop();
            // No hover left stuck while hidden.
            for (CodeCard* card : cards())
                card->hoverAt(QPoint(), false);
        }
    }
    return QObject::eventFilter(obj, event);
}

void CardDeck::pollHover()
{
    const QPoint gp = QCursor::pos();
    // Short-circuit the common case: pointer nowhere near the host. The cheap
    // geometric test can't see occlusion (a completion popup or dialog over
    // the dock), so once it passes, confirm via widgetAt that the host really
    // is what's under the pointer; hover must never fire beneath another
    // window.
    bool onHost = m_host->isVisible() && m_host->window()->isActiveWindow()
        && m_host->rect().contains(m_host->mapFromGlobal(gp));
    if (onHost)
    {
        QWidget* under = QApplication::widgetAt(gp);
        onHost = under && (under == m_host || m_host->isAncestorOf(under));
    }
    for (CodeCard* card : cards())
        card->hoverAt(gp, onHost);
}
