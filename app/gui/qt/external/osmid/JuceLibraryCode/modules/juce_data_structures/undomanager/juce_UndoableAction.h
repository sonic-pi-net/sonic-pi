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

//==============================================================================
/**
    Used by the UndoManager class to store an action which can be done
    and undone.

    @see UndoManager
*/
class JUCE_API  UndoableAction
{
protected:
    /** Creates an action. */
    UndoableAction() noexcept   {}

public:
    /** Destructor. */
    virtual ~UndoableAction()   {}

    //==============================================================================
    /** Overridden by a subclass to perform the action.

        This method is called by the UndoManager, and shouldn't be used directly by
        applications.

        Be careful not to make any calls in a perform() method that could call
        recursively back into the UndoManager::perform() method

        @returns    true if the action could be performed.
        @see UndoManager::perform
    */
    virtual bool perform() = 0;

    /** Overridden by a subclass to undo the action.

        This method is called by the UndoManager, and shouldn't be used directly by
        applications.

        Be careful not to make any calls in an undo() method that could call
        recursively back into the UndoManager::perform() method

        @returns    true if the action could be undone without any errors.
        @see UndoManager::perform
    */
    virtual bool undo() = 0;

    //==============================================================================
    /** Returns a value to indicate how much memory this object takes up.

        Because the UndoManager keeps a list of UndoableActions, this is used
        to work out how much space each one will take up, so that the UndoManager
        can work out how many to keep.

        The default value returned here is 10 - units are arbitrary and
        don't have to be accurate.

        @see UndoManager::getNumberOfUnitsTakenUpByStoredCommands,
             UndoManager::setMaxNumberOfStoredUnits
    */
    virtual int getSizeInUnits()    { return 10; }

    /** Allows multiple actions to be coalesced into a single action object, to reduce storage space.

        If possible, this method should create and return a single action that does the same job as
        this one followed by the supplied action.

        If it's not possible to merge the two actions, the method should return a nullptr.
    */
    virtual UndoableAction* createCoalescedAction (UndoableAction* nextAction)  { ignoreUnused (nextAction); return nullptr; }
};

} // namespace juce
