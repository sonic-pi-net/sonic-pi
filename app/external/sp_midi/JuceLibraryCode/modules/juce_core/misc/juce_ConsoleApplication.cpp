/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2020 - Raw Material Software Limited

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

static File resolveFilename (const String& name)
{
    return File::getCurrentWorkingDirectory().getChildFile (name.unquoted());
}

static File checkFileExists (const File& f)
{
    if (! f.exists())
        ConsoleApplication::fail ("Could not find file: " + f.getFullPathName());

    return f;
}

static File checkFolderExists (const File& f)
{
    if (! f.isDirectory())
        ConsoleApplication::fail ("Could not find folder: " + f.getFullPathName());

    return f;
}

static File resolveFilenameForOption (const ArgumentList& args, StringRef option, const String& filename)
{
    if (filename.isEmpty())
    {
        args.failIfOptionIsMissing (option);
        ConsoleApplication::fail ("Expected a filename after the " + option + " option");
    }

    return resolveFilename (filename);
}

File ArgumentList::Argument::resolveAsFile() const
{
    return resolveFilename (text);
}

File ArgumentList::Argument::resolveAsExistingFile() const
{
    return checkFileExists (resolveAsFile());
}

File ArgumentList::Argument::resolveAsExistingFolder() const
{
    auto f = resolveAsFile();

    if (! f.isDirectory())
        ConsoleApplication::fail ("Could not find folder: " + f.getFullPathName());

    return f;
}

static bool isShortOptionFormat (StringRef s)  { return s[0] == '-' && s[1] != '-'; }
static bool isLongOptionFormat  (StringRef s)  { return s[0] == '-' && s[1] == '-' && s[2] != '-'; }
static bool isOptionFormat      (StringRef s)  { return s[0] == '-'; }

bool ArgumentList::Argument::isLongOption() const     { return isLongOptionFormat (text); }
bool ArgumentList::Argument::isShortOption() const    { return isShortOptionFormat (text); }
bool ArgumentList::Argument::isOption() const         { return isOptionFormat (text); }

bool ArgumentList::Argument::isLongOption (const String& option) const
{
    if (! isLongOptionFormat (option))
    {
        jassert (! isShortOptionFormat (option)); // this will always fail to match
        return isLongOption ("--" + option);
    }

    return text.upToFirstOccurrenceOf ("=", false, false) == option;
}

String ArgumentList::Argument::getLongOptionValue() const
{
    if (isLongOption())
    {
        auto equalsIndex = text.indexOfChar ('=');

        if (equalsIndex > 0)
            return text.substring (equalsIndex + 1);
    }

    return {};
}

bool ArgumentList::Argument::isShortOption (char option) const
{
    jassert (option != '-'); // this is probably not what you intended to pass in

    return isShortOption() && text.containsChar (String (option)[0]);
}

bool ArgumentList::Argument::operator== (StringRef wildcard) const
{
    for (auto& o : StringArray::fromTokens (wildcard, "|", {}))
    {
        if (text == o)
            return true;

        if (isShortOptionFormat (o) && o.length() == 2 && isShortOption ((char) o[1]))
            return true;

        if (isLongOptionFormat (o) && isLongOption (o))
            return true;
    }

    return false;
}

bool ArgumentList::Argument::operator!= (StringRef s) const   { return ! operator== (s); }

//==============================================================================
ArgumentList::ArgumentList (String exeName, StringArray args)
    : executableName (std::move (exeName))
{
    args.trim();
    args.removeEmptyStrings();

    for (auto& a : args)
        arguments.add ({ a });
}

ArgumentList::ArgumentList (int argc, char* argv[])
    : ArgumentList (argv[0], StringArray (argv + 1, argc - 1))
{
}

ArgumentList::ArgumentList (const String& exeName, const String& args)
    : ArgumentList (exeName, StringArray::fromTokens (args, true))
{
}

int ArgumentList::size() const                                      { return arguments.size(); }
ArgumentList::Argument ArgumentList::operator[] (int index) const   { return arguments[index]; }

void ArgumentList::checkMinNumArguments (int expectedMinNumberOfArgs) const
{
    if (size() < expectedMinNumberOfArgs)
        ConsoleApplication::fail ("Not enough arguments!");
}

int ArgumentList::indexOfOption (StringRef option) const
{
    jassert (option == String (option).trim()); // passing non-trimmed strings will always fail to find a match!

    for (int i = 0; i < arguments.size(); ++i)
        if (arguments.getReference(i) == option)
            return i;

    return -1;
}

bool ArgumentList::containsOption (StringRef option) const
{
    return indexOfOption (option) >= 0;
}

bool ArgumentList::removeOptionIfFound (StringRef option)
{
    auto i = indexOfOption (option);

    if (i >= 0)
        arguments.remove (i);

    return i >= 0;
}

void ArgumentList::failIfOptionIsMissing (StringRef option) const
{
    if (indexOfOption (option) < 0)
        ConsoleApplication::fail ("Expected the option " + option);
}

String ArgumentList::getValueForOption (StringRef option) const
{
    jassert (isOptionFormat (option)); // the thing you're searching for must be an option

    for (int i = 0; i < arguments.size(); ++i)
    {
        auto& arg = arguments.getReference(i);

        if (arg == option)
        {
            if (arg.isShortOption())
            {
                if (i < arguments.size() - 1 && ! arguments.getReference (i + 1).isOption())
                    return arguments.getReference (i + 1).text;

                return {};
            }

            if (arg.isLongOption())
                return arg.getLongOptionValue();
        }
    }

    return {};
}

String ArgumentList::removeValueForOption (StringRef option)
{
    jassert (isOptionFormat (option)); // the thing you're searching for must be an option

    for (int i = 0; i < arguments.size(); ++i)
    {
        auto& arg = arguments.getReference(i);

        if (arg == option)
        {
            if (arg.isShortOption())
            {
                if (i < arguments.size() - 1 && ! arguments.getReference (i + 1).isOption())
                {
                    auto result = arguments.getReference (i + 1).text;
                    arguments.removeRange (i, 2);
                    return result;
                }

                arguments.remove (i);
                return {};
            }

            if (arg.isLongOption())
            {
                auto result = arg.getLongOptionValue();
                arguments.remove (i);
                return result;
            }
        }
    }

    return {};
}

File ArgumentList::getFileForOption (StringRef option) const
{
    return resolveFilenameForOption (*this, option, getValueForOption (option));
}

File ArgumentList::getFileForOptionAndRemove (StringRef option)
{
    return resolveFilenameForOption (*this, option, removeValueForOption (option));
}

File ArgumentList::getExistingFileForOption (StringRef option) const
{
    return checkFileExists (getFileForOption (option));
}

File ArgumentList::getExistingFileForOptionAndRemove (StringRef option)
{
    return checkFileExists (getFileForOptionAndRemove (option));
}

File ArgumentList::getExistingFolderForOption (StringRef option) const
{
    return checkFolderExists (getFileForOption (option));
}

File ArgumentList::getExistingFolderForOptionAndRemove (StringRef option)
{
    return checkFolderExists (getFileForOptionAndRemove (option));
}

//==============================================================================
struct ConsoleAppFailureCode
{
    String errorMessage;
    int returnCode;
};

void ConsoleApplication::fail (String errorMessage, int returnCode)
{
    throw ConsoleAppFailureCode { std::move (errorMessage), returnCode };
}

int ConsoleApplication::invokeCatchingFailures (std::function<int()>&& f)
{
    int returnCode = 0;

    try
    {
        returnCode = f();
    }
    catch (const ConsoleAppFailureCode& error)
    {
        std::cerr << error.errorMessage << std::endl;
        returnCode = error.returnCode;
    }

    return returnCode;
}

const ConsoleApplication::Command* ConsoleApplication::findCommand (const ArgumentList& args, bool optionMustBeFirstArg) const
{
    for (auto& c : commands)
    {
        auto index = args.indexOfOption (c.commandOption);

        if (optionMustBeFirstArg ? (index == 0) : (index >= 0))
            return &c;
    }

    if (commandIfNoOthersRecognised >= 0)
        return &commands[(size_t) commandIfNoOthersRecognised];

    return {};
}

int ConsoleApplication::findAndRunCommand (const ArgumentList& args, bool optionMustBeFirstArg) const
{
    return invokeCatchingFailures ([&args, optionMustBeFirstArg, this]
    {
        if (auto c = findCommand (args, optionMustBeFirstArg))
            c->command (args);
        else
            fail ("Unrecognised arguments");

        return 0;
    });
}

int ConsoleApplication::findAndRunCommand (int argc, char* argv[]) const
{
    return findAndRunCommand (ArgumentList (argc, argv));
}

void ConsoleApplication::addCommand (Command c)
{
    commands.emplace_back (std::move (c));
}

void ConsoleApplication::addDefaultCommand (Command c)
{
    commandIfNoOthersRecognised = (int) commands.size();
    addCommand (std::move (c));
}

void ConsoleApplication::addHelpCommand (String arg, String helpMessage, bool makeDefaultCommand)
{
    Command c { arg, arg, "Prints the list of commands", {},
                [this, helpMessage] (const ArgumentList& args)
                {
                    std::cout << helpMessage << std::endl;
                    printCommandList (args);
                }};

    if (makeDefaultCommand)
        addDefaultCommand (std::move (c));
    else
        addCommand (std::move (c));
}

void ConsoleApplication::addVersionCommand (String arg, String versionText)
{
    addCommand ({ arg, arg, "Prints the current version number", {},
                  [versionText] (const ArgumentList&)
                  {
                      std::cout << versionText << std::endl;
                  }});
}

const std::vector<ConsoleApplication::Command>& ConsoleApplication::getCommands() const
{
    return commands;
}

static String getExeNameAndArgs (const ArgumentList& args, const ConsoleApplication::Command& command)
{
    auto exeName = args.executableName.fromLastOccurrenceOf ("/", false, false)
                                      .fromLastOccurrenceOf ("\\", false, false);

    return " " + exeName + " " + command.argumentDescription;
}

static void printCommandDescription (const ArgumentList& args, const ConsoleApplication::Command& command,
                                     int descriptionIndent)
{
    auto nameAndArgs = getExeNameAndArgs (args, command);

    if (nameAndArgs.length() > descriptionIndent)
        std::cout << nameAndArgs << std::endl << String().paddedRight (' ', descriptionIndent);
    else
        std::cout << nameAndArgs.paddedRight (' ', descriptionIndent);

    std::cout << command.shortDescription << std::endl;
}

void ConsoleApplication::printCommandList (const ArgumentList& args) const
{
    int descriptionIndent = 0;

    for (auto& c : commands)
        descriptionIndent = std::max (descriptionIndent, getExeNameAndArgs (args, c).length());

    descriptionIndent = std::min (descriptionIndent + 2, 40);

    for (auto& c : commands)
        printCommandDescription (args, c, descriptionIndent);

    std::cout << std::endl;
}

void ConsoleApplication::printCommandDetails (const ArgumentList& args, const Command& command) const
{
    auto len = getExeNameAndArgs (args, command).length();

    printCommandDescription (args, command, std::min (len + 3, 40));

    if (command.longDescription.isNotEmpty())
        std::cout << std::endl << command.longDescription << std::endl;
}


} // namespace juce
