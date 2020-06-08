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

#if JUCE_MAC || JUCE_WINDOWS || defined (DOXYGEN)

//==============================================================================
/**
    An instance of this class will provide callbacks when drives are
    mounted or unmounted on the system.

    Just inherit from this class and implement the pure virtual method
    to get the callbacks, there's no need to do anything else.

    @see File::findFileSystemRoots()

    @tags{Events}
*/
class JUCE_API  MountedVolumeListChangeDetector
{
public:
    MountedVolumeListChangeDetector();
    virtual ~MountedVolumeListChangeDetector();

    /** This method is called when a volume is mounted or unmounted. */
    virtual void mountedVolumeListChanged() = 0;

private:
    JUCE_PUBLIC_IN_DLL_BUILD (struct Pimpl)
    std::unique_ptr<Pimpl> pimpl;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MountedVolumeListChangeDetector)
};

#endif

} // namespace juce
