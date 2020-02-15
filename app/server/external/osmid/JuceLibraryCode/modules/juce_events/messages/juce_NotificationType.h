/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2017 - ROLI Ltd.

   JUCE is an open source library subject to commercial or open-source
   licensing.

   The code included in this file is provided under the terms of the ISC license
   http://www.isc.org/downloads/software-support-policy/isc-license. Permission
   To use, copy, modify, and/or distribute this software for any purpose with or
   without fee is hereby granted provided that the above copyright notice and
   this permission notice appear in all copies.

   JUCE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY, AND ALL WARRANTIES, WHETHER
   EXPRESSED OR IMPLIED, INCLUDING MERCHANTABILITY AND FITNESS FOR PURPOSE, ARE
   DISCLAIMED.

  ==============================================================================
*/

namespace juce
{

//==============================================================================
/**
    These enums are used in various classes to indicate whether a notification
    event should be sent out.
*/
enum NotificationType
{
    dontSendNotification = 0,   /**< No notification message should be sent. */
    sendNotification = 1,       /**< Requests a notification message, either synchronous or not. */
    sendNotificationSync,       /**< Requests a synchronous notification. */
    sendNotificationAsync,      /**< Requests an asynchronous notification. */
};

} // namespace juce
