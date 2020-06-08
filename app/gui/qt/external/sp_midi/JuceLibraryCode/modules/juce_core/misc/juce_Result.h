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
    Represents the 'success' or 'failure' of an operation, and holds an associated
    error message to describe the error when there's a failure.

    E.g.
    @code
    Result myOperation()
    {
        if (doSomeKindOfFoobar())
            return Result::ok();
        else
            return Result::fail ("foobar didn't work!");
    }

    const Result result (myOperation());

    if (result.wasOk())
    {
        ...it's all good...
    }
    else
    {
        warnUserAboutFailure ("The foobar operation failed! Error message was: "
                                + result.getErrorMessage());
    }
    @endcode

    @tags{Core}
*/
class JUCE_API  Result
{
public:
    //==============================================================================
    /** Creates and returns a 'successful' result. */
    static Result ok() noexcept                         { return Result(); }

    /** Creates a 'failure' result.
        If you pass a blank error message in here, a default "Unknown Error" message
        will be used instead.
    */
    static Result fail (const String& errorMessage) noexcept;

    //==============================================================================
    /** Returns true if this result indicates a success. */
    bool wasOk() const noexcept;

    /** Returns true if this result indicates a failure.
        You can use getErrorMessage() to retrieve the error message associated
        with the failure.
    */
    bool failed() const noexcept;

    /** Returns true if this result indicates a success.
        This is equivalent to calling wasOk().
    */
    operator bool() const noexcept;

    /** Returns true if this result indicates a failure.
        This is equivalent to calling failed().
    */
    bool operator!() const noexcept;

    /** Returns the error message that was set when this result was created.
        For a successful result, this will be an empty string;
    */
    const String& getErrorMessage() const noexcept;

    //==============================================================================
    Result (const Result&);
    Result& operator= (const Result&);
    Result (Result&&) noexcept;
    Result& operator= (Result&&) noexcept;

    bool operator== (const Result& other) const noexcept;
    bool operator!= (const Result& other) const noexcept;

private:
    String errorMessage;

    // The default constructor is not for public use!
    // Instead, use Result::ok() or Result::fail()
    Result() noexcept;
    explicit Result (const String&) noexcept;

    // These casts are private to prevent people trying to use the Result object in numeric contexts
    operator int() const;
    operator void*() const;
};

} // namespace juce
