/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2017 - ROLI Ltd.

   JUCE is an open source library subject to commercial or open-source
   licensing.

   By using JUCE, you agree to the terms of both the JUCE 5 End-User License
   Agreement and JUCE 5 Privacy Policy (both updated and effective as of the
   27th April 2017).

   End User License Agreement: www.juce.com/juce-5-licence
   Privacy Policy: www.juce.com/juce-5-privacy-policy

   Or: You may also use this code under the terms of the GPL v3 (see
   www.gnu.org/licenses).

   JUCE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY, AND ALL WARRANTIES, WHETHER
   EXPRESSED OR IMPLIED, INCLUDING MERCHANTABILITY AND FITNESS FOR PURPOSE, ARE
   DISCLAIMED.

  ==============================================================================
*/

namespace juce
{

struct UndoManager::ActionSet
{
    ActionSet (const String& transactionName)
        : name (transactionName),
          time (Time::getCurrentTime())
    {}

    bool perform() const
    {
        for (int i = 0; i < actions.size(); ++i)
            if (! actions.getUnchecked(i)->perform())
                return false;

        return true;
    }

    bool undo() const
    {
        for (int i = actions.size(); --i >= 0;)
            if (! actions.getUnchecked(i)->undo())
                return false;

        return true;
    }

    int getTotalSize() const
    {
        int total = 0;

        for (int i = actions.size(); --i >= 0;)
            total += actions.getUnchecked(i)->getSizeInUnits();

        return total;
    }

    OwnedArray<UndoableAction> actions;
    String name;
    Time time;
};

//==============================================================================
UndoManager::UndoManager (const int maxNumberOfUnitsToKeep,
                          const int minimumTransactions)
   : totalUnitsStored (0),
     nextIndex (0),
     newTransaction (true),
     reentrancyCheck (false)
{
    setMaxNumberOfStoredUnits (maxNumberOfUnitsToKeep,
                               minimumTransactions);
}

UndoManager::~UndoManager()
{
}

//==============================================================================
void UndoManager::clearUndoHistory()
{
    transactions.clear();
    totalUnitsStored = 0;
    nextIndex = 0;
    sendChangeMessage();
}

int UndoManager::getNumberOfUnitsTakenUpByStoredCommands() const
{
    return totalUnitsStored;
}

void UndoManager::setMaxNumberOfStoredUnits (const int maxNumberOfUnitsToKeep,
                                             const int minimumTransactions)
{
    maxNumUnitsToKeep          = jmax (1, maxNumberOfUnitsToKeep);
    minimumTransactionsToKeep  = jmax (1, minimumTransactions);
}

//==============================================================================
bool UndoManager::perform (UndoableAction* const newAction, const String& actionName)
{
    if (perform (newAction))
    {
        if (actionName.isNotEmpty())
            setCurrentTransactionName (actionName);

        return true;
    }

    return false;
}

bool UndoManager::perform (UndoableAction* const newAction)
{
    if (newAction != nullptr)
    {
        ScopedPointer<UndoableAction> action (newAction);

        if (reentrancyCheck)
        {
            jassertfalse;  // don't call perform() recursively from the UndoableAction::perform()
                           // or undo() methods, or else these actions will be discarded!
            return false;
        }

        if (action->perform())
        {
            ActionSet* actionSet = getCurrentSet();

            if (actionSet != nullptr && ! newTransaction)
            {
                if (UndoableAction* const lastAction = actionSet->actions.getLast())
                {
                    if (UndoableAction* const coalescedAction = lastAction->createCoalescedAction (action))
                    {
                        action = coalescedAction;
                        totalUnitsStored -= lastAction->getSizeInUnits();
                        actionSet->actions.removeLast();
                    }
                }
            }
            else
            {
                actionSet = new ActionSet (newTransactionName);
                transactions.insert (nextIndex, actionSet);
                ++nextIndex;
            }

            totalUnitsStored += action->getSizeInUnits();
            actionSet->actions.add (action.release());
            newTransaction = false;

            moveFutureTransactionsToStash();
            dropOldTransactionsIfTooLarge();
            sendChangeMessage();
            return true;
        }
    }

    return false;
}

void UndoManager::moveFutureTransactionsToStash()
{
    if (nextIndex < transactions.size())
    {
        stashedFutureTransactions.clear();

        while (nextIndex < transactions.size())
        {
            ActionSet* removed = transactions.removeAndReturn (nextIndex);
            stashedFutureTransactions.add (removed);
            totalUnitsStored -= removed->getTotalSize();
        }
    }
}

void UndoManager::restoreStashedFutureTransactions()
{
    while (nextIndex < transactions.size())
    {
        totalUnitsStored -= transactions.getUnchecked (nextIndex)->getTotalSize();
        transactions.remove (nextIndex);
    }

    for (int i = 0; i < stashedFutureTransactions.size(); ++i)
    {
        ActionSet* action = stashedFutureTransactions.removeAndReturn (i);
        totalUnitsStored += action->getTotalSize();
        transactions.add (action);
    }

    stashedFutureTransactions.clearQuick (false);
}

void UndoManager::dropOldTransactionsIfTooLarge()
{
    while (nextIndex > 0
            && totalUnitsStored > maxNumUnitsToKeep
            && transactions.size() > minimumTransactionsToKeep)
    {
        totalUnitsStored -= transactions.getFirst()->getTotalSize();
        transactions.remove (0);
        --nextIndex;

        // if this fails, then some actions may not be returning
        // consistent results from their getSizeInUnits() method
        jassert (totalUnitsStored >= 0);
    }
}

void UndoManager::beginNewTransaction() noexcept
{
    beginNewTransaction (String());
}

void UndoManager::beginNewTransaction (const String& actionName) noexcept
{
    newTransaction = true;
    newTransactionName = actionName;
}

void UndoManager::setCurrentTransactionName (const String& newName) noexcept
{
    if (newTransaction)
        newTransactionName = newName;
    else if (ActionSet* action = getCurrentSet())
        action->name = newName;
}

String UndoManager::getCurrentTransactionName() const noexcept
{
    if (ActionSet* action = getCurrentSet())
        return action->name;

    return newTransactionName;
}

//==============================================================================
UndoManager::ActionSet* UndoManager::getCurrentSet() const noexcept     { return transactions [nextIndex - 1]; }
UndoManager::ActionSet* UndoManager::getNextSet() const noexcept        { return transactions [nextIndex]; }

bool UndoManager::canUndo() const noexcept   { return getCurrentSet() != nullptr; }
bool UndoManager::canRedo() const noexcept   { return getNextSet()    != nullptr; }

bool UndoManager::undo()
{
    if (const ActionSet* const s = getCurrentSet())
    {
        const ScopedValueSetter<bool> setter (reentrancyCheck, true);

        if (s->undo())
            --nextIndex;
        else
            clearUndoHistory();

        beginNewTransaction();
        sendChangeMessage();
        return true;
    }

    return false;
}

bool UndoManager::redo()
{
    if (const ActionSet* const s = getNextSet())
    {
        const ScopedValueSetter<bool> setter (reentrancyCheck, true);

        if (s->perform())
            ++nextIndex;
        else
            clearUndoHistory();

        beginNewTransaction();
        sendChangeMessage();
        return true;
    }

    return false;
}

String UndoManager::getUndoDescription() const
{
    if (auto* s = getCurrentSet())
        return s->name;

    return {};
}

String UndoManager::getRedoDescription() const
{
    if (auto* s = getNextSet())
        return s->name;

    return {};
}

Time UndoManager::getTimeOfUndoTransaction() const
{
    if (auto* s = getCurrentSet())
        return s->time;

    return {};
}

Time UndoManager::getTimeOfRedoTransaction() const
{
    if (auto* s = getNextSet())
        return s->time;

    return Time::getCurrentTime();
}

bool UndoManager::undoCurrentTransactionOnly()
{
    if ((! newTransaction) && undo())
    {
        restoreStashedFutureTransactions();
        return true;
    }

    return false;
}

void UndoManager::getActionsInCurrentTransaction (Array<const UndoableAction*>& actionsFound) const
{
    if (! newTransaction)
        if (const ActionSet* const s = getCurrentSet())
            for (int i = 0; i < s->actions.size(); ++i)
                actionsFound.add (s->actions.getUnchecked(i));
}

int UndoManager::getNumActionsInCurrentTransaction() const
{
    if (! newTransaction)
        if (const ActionSet* const s = getCurrentSet())
            return s->actions.size();

    return 0;
}

} // namespace juce
