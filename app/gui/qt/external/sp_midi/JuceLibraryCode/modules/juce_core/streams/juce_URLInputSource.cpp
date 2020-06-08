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

URLInputSource::URLInputSource (const URL& url)
    : u (url)
{
}

URLInputSource::URLInputSource (URL&& url)
    : u (std::move (url))
{
}

URLInputSource::~URLInputSource()
{
}

InputStream* URLInputSource::createInputStream()
{
    return u.createInputStream (false);
}

InputStream* URLInputSource::createInputStreamFor (const String& relatedItemPath)
{
    auto sub = u.getSubPath();
    auto parent = sub.containsChar (L'/') ? sub.upToLastOccurrenceOf ("/", false, false)
                                          : String ();

    return u.withNewSubPath (parent).getChildURL (relatedItemPath).createInputStream (false);
}

int64 URLInputSource::hashCode() const
{
    return u.toString (true).hashCode64();
}

} // namespace juce
