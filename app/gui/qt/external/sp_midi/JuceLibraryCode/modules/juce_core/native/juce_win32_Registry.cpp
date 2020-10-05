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

struct RegistryKeyWrapper
{
    RegistryKeyWrapper (String name, bool createForWriting, DWORD wow64Flags)
    {
        if (HKEY rootKey = getRootKey (name))
        {
            name = name.substring (name.indexOfChar ('\\') + 1);

            auto lastSlash = name.lastIndexOfChar ('\\');
            valueName = name.substring (lastSlash + 1);
            wideCharValueName = valueName.toWideCharPointer();

            name = name.substring (0, lastSlash);
            auto wideCharName = name.toWideCharPointer();
            DWORD result;

            if (createForWriting)
                RegCreateKeyEx (rootKey, wideCharName, 0, nullptr, REG_OPTION_NON_VOLATILE,
                                KEY_WRITE | KEY_QUERY_VALUE | wow64Flags, nullptr, &key, &result);
            else
                RegOpenKeyEx (rootKey, wideCharName, 0, KEY_READ | wow64Flags, &key);
        }
    }

    ~RegistryKeyWrapper()
    {
        if (key != nullptr)
            RegCloseKey (key);
    }

    static HKEY getRootKey (const String& name) noexcept
    {
        if (name.startsWithIgnoreCase ("HKEY_CURRENT_USER\\"))  return HKEY_CURRENT_USER;
        if (name.startsWithIgnoreCase ("HKCU\\"))               return HKEY_CURRENT_USER;
        if (name.startsWithIgnoreCase ("HKEY_LOCAL_MACHINE\\")) return HKEY_LOCAL_MACHINE;
        if (name.startsWithIgnoreCase ("HKLM\\"))               return HKEY_LOCAL_MACHINE;
        if (name.startsWithIgnoreCase ("HKEY_CLASSES_ROOT\\"))  return HKEY_CLASSES_ROOT;
        if (name.startsWithIgnoreCase ("HKCR\\"))               return HKEY_CLASSES_ROOT;
        if (name.startsWithIgnoreCase ("HKEY_USERS\\"))         return HKEY_USERS;
        if (name.startsWithIgnoreCase ("HKU\\"))                return HKEY_USERS;

        jassertfalse; // The name starts with an unknown root key (or maybe an old Win9x type)
        return nullptr;
    }

    static bool setValue (const String& regValuePath, const DWORD type,
                          const void* data, size_t dataSize, const DWORD wow64Flags)
    {
        const RegistryKeyWrapper key (regValuePath, true, wow64Flags);

        return key.key != nullptr
                && RegSetValueEx (key.key, key.wideCharValueName, 0, type,
                                  reinterpret_cast<const BYTE*> (data),
                                  (DWORD) dataSize) == ERROR_SUCCESS;
    }

    static uint32 getBinaryValue (const String& regValuePath, MemoryBlock& result, DWORD wow64Flags)
    {
        const RegistryKeyWrapper key (regValuePath, false, wow64Flags);

        if (key.key != nullptr)
        {
            for (unsigned long bufferSize = 1024; ; bufferSize *= 2)
            {
                result.setSize (bufferSize, false);
                DWORD type = REG_NONE;

                auto err = RegQueryValueEx (key.key, key.wideCharValueName, nullptr, &type,
                                            (LPBYTE) result.getData(), &bufferSize);

                if (err == ERROR_SUCCESS)
                {
                    result.setSize (bufferSize, false);
                    return type;
                }

                if (err != ERROR_MORE_DATA)
                    break;
            }
        }

        return REG_NONE;
    }

    static String getValue (const String& regValuePath, const String& defaultValue, DWORD wow64Flags)
    {
        MemoryBlock buffer;

        switch (getBinaryValue (regValuePath, buffer, wow64Flags))
        {
            case REG_SZ:    return static_cast<const WCHAR*> (buffer.getData());
            case REG_DWORD: return String ((int) *reinterpret_cast<const DWORD*> (buffer.getData()));
            default:        break;
        }

        return defaultValue;
    }

    static bool keyExists (const String& regKeyPath, const DWORD wow64Flags)
    {
        return RegistryKeyWrapper (regKeyPath + "\\", false, wow64Flags).key != nullptr;
    }

    static bool valueExists (const String& regValuePath, const DWORD wow64Flags)
    {
        const RegistryKeyWrapper key (regValuePath, false, wow64Flags);

        if (key.key == nullptr)
            return false;

        unsigned char buffer [512];
        unsigned long bufferSize = sizeof (buffer);
        DWORD type = 0;

        auto result = RegQueryValueEx (key.key, key.wideCharValueName,
                                       nullptr, &type, buffer, &bufferSize);

        return result == ERROR_SUCCESS || result == ERROR_MORE_DATA;
    }

    HKEY key = nullptr;
    const wchar_t* wideCharValueName = nullptr;
    String valueName;

    JUCE_DECLARE_NON_COPYABLE (RegistryKeyWrapper)
};

uint32 JUCE_CALLTYPE WindowsRegistry::getBinaryValue (const String& regValuePath, MemoryBlock& result, WoW64Mode mode)
{
    return RegistryKeyWrapper::getBinaryValue (regValuePath, result, (DWORD) mode);
}

String JUCE_CALLTYPE WindowsRegistry::getValue (const String& regValuePath, const String& defaultValue, WoW64Mode mode)
{
    return RegistryKeyWrapper::getValue (regValuePath, defaultValue, (DWORD) mode);
}

bool JUCE_CALLTYPE WindowsRegistry::setValue (const String& regValuePath, const String& value, WoW64Mode mode)
{
    return RegistryKeyWrapper::setValue (regValuePath, REG_SZ, value.toWideCharPointer(),
                                         CharPointer_UTF16::getBytesRequiredFor (value.getCharPointer()), mode);
}

bool JUCE_CALLTYPE WindowsRegistry::setValue (const String& regValuePath, const uint32 value, WoW64Mode mode)
{
    return RegistryKeyWrapper::setValue (regValuePath, REG_DWORD, &value, sizeof (value), (DWORD) mode);
}

bool JUCE_CALLTYPE WindowsRegistry::setValue (const String& regValuePath, const uint64 value, WoW64Mode mode)
{
    return RegistryKeyWrapper::setValue (regValuePath, REG_QWORD, &value, sizeof (value), (DWORD) mode);
}

bool JUCE_CALLTYPE WindowsRegistry::setValue (const String& regValuePath, const MemoryBlock& value, WoW64Mode mode)
{
    return RegistryKeyWrapper::setValue (regValuePath, REG_BINARY, value.getData(), value.getSize(), (DWORD) mode);
}

bool JUCE_CALLTYPE WindowsRegistry::valueExists (const String& regValuePath, WoW64Mode mode)
{
    return RegistryKeyWrapper::valueExists (regValuePath, (DWORD) mode);
}

bool JUCE_CALLTYPE WindowsRegistry::keyExists (const String& regKeyPath, WoW64Mode mode)
{
    return RegistryKeyWrapper::keyExists (regKeyPath, (DWORD) mode);
}

bool JUCE_CALLTYPE WindowsRegistry::deleteValue (const String& regValuePath, WoW64Mode mode)
{
    const RegistryKeyWrapper key (regValuePath, true, (DWORD) mode);

    return key.key != nullptr && RegDeleteValue (key.key, key.wideCharValueName) == ERROR_SUCCESS;
}

static bool deleteKeyNonRecursive (const String& regKeyPath, WindowsRegistry::WoW64Mode mode)
{
    const RegistryKeyWrapper key (regKeyPath, true, (DWORD) mode);

    return key.key != nullptr && RegDeleteKey (key.key, key.wideCharValueName) == ERROR_SUCCESS;
}

bool JUCE_CALLTYPE WindowsRegistry::deleteKey (const String& regKeyPath, WoW64Mode mode)
{
    if (deleteKeyNonRecursive (regKeyPath, mode))
        return true;

    for (const RegistryKeyWrapper key (regKeyPath + "\\", false, (DWORD) mode);;)
    {
        wchar_t subKey[MAX_PATH + 1] = {};
        DWORD subKeySize = MAX_PATH;

        if (RegEnumKeyEx (key.key, 0, subKey, &subKeySize, nullptr, nullptr, nullptr, nullptr) != ERROR_SUCCESS
             || ! deleteKey (regKeyPath + "\\" + String (subKey), mode))
            break;
    }

    return deleteKeyNonRecursive (regKeyPath, mode);
}

bool JUCE_CALLTYPE WindowsRegistry::registerFileAssociation (const String& fileExtension,
                                                             const String& symbolicDescription,
                                                             const String& fullDescription,
                                                             const File& targetExecutable,
                                                             const int iconResourceNumber,
                                                             const bool registerForCurrentUserOnly,
                                                             WoW64Mode mode)
{
    auto root = registerForCurrentUserOnly ? "HKEY_CURRENT_USER\\Software\\Classes\\"
                                           : "HKEY_CLASSES_ROOT\\";
    auto key = root + symbolicDescription;

    return setValue (root + fileExtension + "\\", symbolicDescription, mode)
        && setValue (key + "\\", fullDescription, mode)
        && setValue (key + "\\shell\\open\\command\\", targetExecutable.getFullPathName() + " \"%1\"", mode)
        && (iconResourceNumber == 0
              || setValue (key + "\\DefaultIcon\\",
                           targetExecutable.getFullPathName() + "," + String (iconResourceNumber)));
}

// These methods are deprecated:
String WindowsRegistry::getValueWow64 (const String& p, const String& defVal)  { return getValue (p, defVal, WoW64_64bit); }
bool WindowsRegistry::valueExistsWow64 (const String& p)                       { return valueExists (p, WoW64_64bit); }
bool WindowsRegistry::keyExistsWow64 (const String& p)                         { return keyExists (p, WoW64_64bit); }

} // namespace juce
