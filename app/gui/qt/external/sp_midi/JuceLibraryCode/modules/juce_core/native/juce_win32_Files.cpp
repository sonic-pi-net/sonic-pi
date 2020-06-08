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

#ifndef INVALID_FILE_ATTRIBUTES
 #define INVALID_FILE_ATTRIBUTES ((DWORD) -1)
#endif

//==============================================================================
namespace WindowsFileHelpers
{
    //==============================================================================
   #if JUCE_WINDOWS
    typedef struct _REPARSE_DATA_BUFFER {
      ULONG  ReparseTag;
      USHORT ReparseDataLength;
      USHORT Reserved;
      union {
        struct {
          USHORT SubstituteNameOffset;
          USHORT SubstituteNameLength;
          USHORT PrintNameOffset;
          USHORT PrintNameLength;
          ULONG  Flags;
          WCHAR  PathBuffer[1];
        } SymbolicLinkReparseBuffer;
        struct {
          USHORT SubstituteNameOffset;
          USHORT SubstituteNameLength;
          USHORT PrintNameOffset;
          USHORT PrintNameLength;
          WCHAR  PathBuffer[1];
        } MountPointReparseBuffer;
        struct {
          UCHAR DataBuffer[1];
        } GenericReparseBuffer;
      } DUMMYUNIONNAME;
    } *PREPARSE_DATA_BUFFER, REPARSE_DATA_BUFFER;
   #endif

    //==============================================================================
    DWORD getAtts (const String& path) noexcept
    {
        return GetFileAttributes (path.toWideCharPointer());
    }

    bool changeAtts (const String& path, DWORD bitsToSet, DWORD bitsToClear) noexcept
    {
        auto oldAtts = getAtts (path);

        if (oldAtts == INVALID_FILE_ATTRIBUTES)
            return false;

        auto newAtts = ((oldAtts | bitsToSet) & ~bitsToClear);

        return newAtts == oldAtts
                || SetFileAttributes (path.toWideCharPointer(), newAtts) != FALSE;
    }

    int64 fileTimeToTime (const FILETIME* const ft) noexcept
    {
        static_assert (sizeof (ULARGE_INTEGER) == sizeof (FILETIME),
                       "ULARGE_INTEGER is too small to hold FILETIME: please report!");

        return (int64) ((reinterpret_cast<const ULARGE_INTEGER*> (ft)->QuadPart - 116444736000000000LL) / 10000);
    }

    FILETIME* timeToFileTime (const int64 time, FILETIME* const ft) noexcept
    {
        if (time <= 0)
            return nullptr;

        reinterpret_cast<ULARGE_INTEGER*> (ft)->QuadPart = (ULONGLONG) (time * 10000 + 116444736000000000LL);
        return ft;
    }

    String getDriveFromPath (String path)
    {
        if (path.isNotEmpty() && path[1] == ':' && path[2] == 0)
            path << '\\';

        const size_t numBytes = CharPointer_UTF16::getBytesRequiredFor (path.getCharPointer()) + 4;
        HeapBlock<WCHAR> pathCopy;
        pathCopy.calloc (numBytes, 1);
        path.copyToUTF16 (pathCopy, numBytes);

        if (PathStripToRoot (pathCopy))
            path = static_cast<const WCHAR*> (pathCopy);

        return path;
    }

    int64 getDiskSpaceInfo (const String& path, const bool total)
    {
        ULARGE_INTEGER spc, tot, totFree;

        if (GetDiskFreeSpaceEx (getDriveFromPath (path).toWideCharPointer(), &spc, &tot, &totFree))
            return total ? (int64) tot.QuadPart
                         : (int64) spc.QuadPart;

        return 0;
    }

    unsigned int getWindowsDriveType (const String& path)
    {
        return GetDriveType (getDriveFromPath (path).toWideCharPointer());
    }

    File getSpecialFolderPath (int type)
    {
        WCHAR path[MAX_PATH + 256];

        if (SHGetSpecialFolderPath (0, path, type, FALSE))
            return File (String (path));

        return {};
    }

    File getModuleFileName (HINSTANCE moduleHandle)
    {
        WCHAR dest[MAX_PATH + 256];
        dest[0] = 0;
        GetModuleFileName (moduleHandle, dest, (DWORD) numElementsInArray (dest));
        return File (String (dest));
    }

    Result getResultForLastError()
    {
        TCHAR messageBuffer[256] = { 0 };

        FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                       nullptr, GetLastError(), MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT),
                       messageBuffer, (DWORD) numElementsInArray (messageBuffer) - 1, nullptr);

        return Result::fail (String (messageBuffer));
    }
}

//==============================================================================
JUCE_DECLARE_DEPRECATED_STATIC (const juce_wchar File::separator = '\\';)
JUCE_DECLARE_DEPRECATED_STATIC (const StringRef File::separatorString ("\\");)

juce_wchar File::getSeparatorChar()    { return '\\'; }
StringRef File::getSeparatorString()   { return "\\"; }

void* getUser32Function (const char*);

//==============================================================================
bool File::exists() const
{
    return fullPath.isNotEmpty()
            && WindowsFileHelpers::getAtts (fullPath) != INVALID_FILE_ATTRIBUTES;
}

bool File::existsAsFile() const
{
    return fullPath.isNotEmpty()
            && (WindowsFileHelpers::getAtts (fullPath) & FILE_ATTRIBUTE_DIRECTORY) == 0;
}

bool File::isDirectory() const
{
    auto attr = WindowsFileHelpers::getAtts (fullPath);
    return (attr & FILE_ATTRIBUTE_DIRECTORY) != 0 && attr != INVALID_FILE_ATTRIBUTES;
}

bool File::hasWriteAccess() const
{
    if (fullPath.isEmpty())
        return true;

    auto attr = WindowsFileHelpers::getAtts (fullPath);

    // NB: According to MS, the FILE_ATTRIBUTE_READONLY attribute doesn't work for
    // folders, and can be incorrectly set for some special folders, so we'll just say
    // that folders are always writable.
    return attr == INVALID_FILE_ATTRIBUTES
            || (attr & FILE_ATTRIBUTE_DIRECTORY) != 0
            || (attr & FILE_ATTRIBUTE_READONLY) == 0;
}

bool File::setFileReadOnlyInternal (bool shouldBeReadOnly) const
{
    return WindowsFileHelpers::changeAtts (fullPath,
                                           shouldBeReadOnly ? FILE_ATTRIBUTE_READONLY : 0,
                                           shouldBeReadOnly ? 0 : FILE_ATTRIBUTE_READONLY);
}

bool File::setFileExecutableInternal (bool /*shouldBeExecutable*/) const
{
    // XXX is this possible?
    return false;
}

bool File::isHidden() const
{
    return (WindowsFileHelpers::getAtts (fullPath) & FILE_ATTRIBUTE_HIDDEN) != 0;
}

//==============================================================================
bool File::deleteFile() const
{
    if (! exists())
        return true;

    return isDirectory() ? RemoveDirectory (fullPath.toWideCharPointer()) != 0
                         : DeleteFile (fullPath.toWideCharPointer()) != 0;
}

bool File::moveToTrash() const
{
    if (! exists())
        return true;

    // The string we pass in must be double null terminated..
    const size_t numBytes = CharPointer_UTF16::getBytesRequiredFor (fullPath.getCharPointer()) + 8;
    HeapBlock<WCHAR> doubleNullTermPath;
    doubleNullTermPath.calloc (numBytes, 1);
    fullPath.copyToUTF16 (doubleNullTermPath, numBytes);

    SHFILEOPSTRUCT fos = { 0 };
    fos.wFunc = FO_DELETE;
    fos.pFrom = doubleNullTermPath;
    fos.fFlags = FOF_ALLOWUNDO | FOF_NOERRORUI | FOF_SILENT | FOF_NOCONFIRMATION
                   | FOF_NOCONFIRMMKDIR | FOF_RENAMEONCOLLISION;

    return SHFileOperation (&fos) == 0;
}

bool File::copyInternal (const File& dest) const
{
    return CopyFile (fullPath.toWideCharPointer(),
                     dest.getFullPathName().toWideCharPointer(), false) != 0;
}

bool File::moveInternal (const File& dest) const
{
    return MoveFile (fullPath.toWideCharPointer(),
                     dest.getFullPathName().toWideCharPointer()) != 0;
}

bool File::replaceInternal (const File& dest) const
{
    return ReplaceFile (dest.getFullPathName().toWideCharPointer(),
                        fullPath.toWideCharPointer(),
                        0, REPLACEFILE_IGNORE_MERGE_ERRORS | 4 /*REPLACEFILE_IGNORE_ACL_ERRORS*/,
                        nullptr, nullptr) != 0;
}

Result File::createDirectoryInternal (const String& fileName) const
{
    return CreateDirectory (fileName.toWideCharPointer(), 0) ? Result::ok()
                                                             : WindowsFileHelpers::getResultForLastError();
}

//==============================================================================
int64 juce_fileSetPosition (void* handle, int64 pos)
{
    LARGE_INTEGER li;
    li.QuadPart = pos;
    li.LowPart = SetFilePointer ((HANDLE) handle, (LONG) li.LowPart,
                                 &li.HighPart, FILE_BEGIN);  // (returns -1 if it fails)
    return li.QuadPart;
}

void FileInputStream::openHandle()
{
    auto h = CreateFile (file.getFullPathName().toWideCharPointer(),
                         GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, 0,
                         OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN, 0);

    if (h != INVALID_HANDLE_VALUE)
        fileHandle = (void*) h;
    else
        status = WindowsFileHelpers::getResultForLastError();
}

FileInputStream::~FileInputStream()
{
    CloseHandle ((HANDLE) fileHandle);
}

size_t FileInputStream::readInternal (void* buffer, size_t numBytes)
{
    if (fileHandle != 0)
    {
        DWORD actualNum = 0;

        if (! ReadFile ((HANDLE) fileHandle, buffer, (DWORD) numBytes, &actualNum, 0))
            status = WindowsFileHelpers::getResultForLastError();

        return (size_t) actualNum;
    }

    return 0;
}

//==============================================================================
void FileOutputStream::openHandle()
{
    auto h = CreateFile (file.getFullPathName().toWideCharPointer(),
                         GENERIC_WRITE, FILE_SHARE_READ, 0,
                         OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);

    if (h != INVALID_HANDLE_VALUE)
    {
        LARGE_INTEGER li;
        li.QuadPart = 0;
        li.LowPart = SetFilePointer (h, 0, &li.HighPart, FILE_END);

        if (li.LowPart != INVALID_SET_FILE_POINTER)
        {
            fileHandle = (void*) h;
            currentPosition = li.QuadPart;
            return;
        }
    }

    status = WindowsFileHelpers::getResultForLastError();
}

void FileOutputStream::closeHandle()
{
    CloseHandle ((HANDLE) fileHandle);
}

ssize_t FileOutputStream::writeInternal (const void* bufferToWrite, size_t numBytes)
{
    DWORD actualNum = 0;

    if (fileHandle != nullptr)
        if (! WriteFile ((HANDLE) fileHandle, bufferToWrite, (DWORD) numBytes, &actualNum, 0))
            status = WindowsFileHelpers::getResultForLastError();

    return (ssize_t) actualNum;
}

void FileOutputStream::flushInternal()
{
    if (fileHandle != nullptr)
        if (! FlushFileBuffers ((HANDLE) fileHandle))
            status = WindowsFileHelpers::getResultForLastError();
}

Result FileOutputStream::truncate()
{
    if (fileHandle == nullptr)
        return status;

    flush();
    return SetEndOfFile ((HANDLE) fileHandle) ? Result::ok()
                                              : WindowsFileHelpers::getResultForLastError();
}

//==============================================================================
void MemoryMappedFile::openInternal (const File& file, AccessMode mode, bool exclusive)
{
    jassert (mode == readOnly || mode == readWrite);

    if (range.getStart() > 0)
    {
        SYSTEM_INFO systemInfo;
        GetNativeSystemInfo (&systemInfo);

        range.setStart (range.getStart() - (range.getStart() % systemInfo.dwAllocationGranularity));
    }

    DWORD accessMode = GENERIC_READ, createType = OPEN_EXISTING;
    DWORD protect = PAGE_READONLY, access = FILE_MAP_READ;

    if (mode == readWrite)
    {
        accessMode = GENERIC_READ | GENERIC_WRITE;
        createType = OPEN_ALWAYS;
        protect = PAGE_READWRITE;
        access = FILE_MAP_ALL_ACCESS;
    }

    auto h = CreateFile (file.getFullPathName().toWideCharPointer(), accessMode,
                         exclusive ? 0 : (FILE_SHARE_READ | FILE_SHARE_DELETE | (mode == readWrite ? FILE_SHARE_WRITE : 0)), 0,
                         createType, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN, 0);

    if (h != INVALID_HANDLE_VALUE)
    {
        fileHandle = (void*) h;

        auto mappingHandle = CreateFileMapping (h, 0, protect,
                                                (DWORD) (range.getEnd() >> 32),
                                                (DWORD) range.getEnd(), 0);

        if (mappingHandle != 0)
        {
            address = MapViewOfFile (mappingHandle, access, (DWORD) (range.getStart() >> 32),
                                     (DWORD) range.getStart(), (SIZE_T) range.getLength());

            if (address == nullptr)
                range = Range<int64>();

            CloseHandle (mappingHandle);
        }
    }
}

MemoryMappedFile::~MemoryMappedFile()
{
    if (address != nullptr)
        UnmapViewOfFile (address);

    if (fileHandle != nullptr)
        CloseHandle ((HANDLE) fileHandle);
}

//==============================================================================
int64 File::getSize() const
{
    WIN32_FILE_ATTRIBUTE_DATA attributes;

    if (GetFileAttributesEx (fullPath.toWideCharPointer(), GetFileExInfoStandard, &attributes))
        return (((int64) attributes.nFileSizeHigh) << 32) | attributes.nFileSizeLow;

    return 0;
}

void File::getFileTimesInternal (int64& modificationTime, int64& accessTime, int64& creationTime) const
{
    using namespace WindowsFileHelpers;
    WIN32_FILE_ATTRIBUTE_DATA attributes;

    if (GetFileAttributesEx (fullPath.toWideCharPointer(), GetFileExInfoStandard, &attributes))
    {
        modificationTime = fileTimeToTime (&attributes.ftLastWriteTime);
        creationTime     = fileTimeToTime (&attributes.ftCreationTime);
        accessTime       = fileTimeToTime (&attributes.ftLastAccessTime);
    }
    else
    {
        creationTime = accessTime = modificationTime = 0;
    }
}

bool File::setFileTimesInternal (int64 modificationTime, int64 accessTime, int64 creationTime) const
{
    using namespace WindowsFileHelpers;

    bool ok = false;
    auto h = CreateFile (fullPath.toWideCharPointer(),
                         GENERIC_WRITE, FILE_SHARE_READ, 0,
                         OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);

    if (h != INVALID_HANDLE_VALUE)
    {
        FILETIME m, a, c;

        ok = SetFileTime (h,
                          timeToFileTime (creationTime, &c),
                          timeToFileTime (accessTime, &a),
                          timeToFileTime (modificationTime, &m)) != 0;

        CloseHandle (h);
    }

    return ok;
}

//==============================================================================
void File::findFileSystemRoots (Array<File>& destArray)
{
    TCHAR buffer[2048] = { 0 };
    GetLogicalDriveStrings (2048, buffer);

    const TCHAR* n = buffer;
    StringArray roots;

    while (*n != 0)
    {
        roots.add (String (n));

        while (*n++ != 0)
        {}
    }

    roots.sort (true);

    for (int i = 0; i < roots.size(); ++i)
        destArray.add (roots[i]);
}

//==============================================================================
String File::getVolumeLabel() const
{
    TCHAR dest[64];

    if (! GetVolumeInformation (WindowsFileHelpers::getDriveFromPath (getFullPathName()).toWideCharPointer(), dest,
                                (DWORD) numElementsInArray (dest), 0, 0, 0, 0, 0))
        dest[0] = 0;

    return dest;
}

int File::getVolumeSerialNumber() const
{
    TCHAR dest[64];
    DWORD serialNum;

    if (! GetVolumeInformation (WindowsFileHelpers::getDriveFromPath (getFullPathName()).toWideCharPointer(), dest,
                                (DWORD) numElementsInArray (dest), &serialNum, 0, 0, 0, 0))
        return 0;

    return (int) serialNum;
}

int64 File::getBytesFreeOnVolume() const
{
    return WindowsFileHelpers::getDiskSpaceInfo (getFullPathName(), false);
}

int64 File::getVolumeTotalSize() const
{
    return WindowsFileHelpers::getDiskSpaceInfo (getFullPathName(), true);
}

uint64 File::getFileIdentifier() const
{
    uint64 result = 0;

    String path = getFullPathName();

    if (isRoot())
        path += "\\";

    auto h = CreateFile (path.toWideCharPointer(),
                         GENERIC_READ, FILE_SHARE_READ, nullptr,
                         OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);

    if (h != INVALID_HANDLE_VALUE)
    {
        BY_HANDLE_FILE_INFORMATION info;
        zerostruct (info);

        if (GetFileInformationByHandle (h, &info))
            result = (((uint64) info.nFileIndexHigh) << 32) | info.nFileIndexLow;

        CloseHandle (h);
    }

    return result;
}

//==============================================================================
bool File::isOnCDRomDrive() const
{
    return WindowsFileHelpers::getWindowsDriveType (getFullPathName()) == DRIVE_CDROM;
}

bool File::isOnHardDisk() const
{
    if (fullPath.isEmpty())
        return false;

    auto n = WindowsFileHelpers::getWindowsDriveType (getFullPathName());

    return n != DRIVE_REMOVABLE
        && n != DRIVE_CDROM
        && n != DRIVE_REMOTE
        && n != DRIVE_NO_ROOT_DIR;
}

bool File::isOnRemovableDrive() const
{
    if (fullPath.isEmpty())
        return false;

    auto n = WindowsFileHelpers::getWindowsDriveType (getFullPathName());

    return n == DRIVE_CDROM
        || n == DRIVE_REMOTE
        || n == DRIVE_REMOVABLE
        || n == DRIVE_RAMDISK;
}

//==============================================================================
File JUCE_CALLTYPE File::getSpecialLocation (const SpecialLocationType type)
{
    int csidlType = 0;

    switch (type)
    {
        case userHomeDirectory:                 csidlType = CSIDL_PROFILE; break;
        case userDocumentsDirectory:            csidlType = CSIDL_PERSONAL; break;
        case userDesktopDirectory:              csidlType = CSIDL_DESKTOP; break;
        case userApplicationDataDirectory:      csidlType = CSIDL_APPDATA; break;
        case commonApplicationDataDirectory:    csidlType = CSIDL_COMMON_APPDATA; break;
        case commonDocumentsDirectory:          csidlType = CSIDL_COMMON_DOCUMENTS; break;
        case globalApplicationsDirectory:       csidlType = CSIDL_PROGRAM_FILES; break;
        case globalApplicationsDirectoryX86:    csidlType = CSIDL_PROGRAM_FILESX86; break;
        case userMusicDirectory:                csidlType = 0x0d; /*CSIDL_MYMUSIC*/ break;
        case userMoviesDirectory:               csidlType = 0x0e; /*CSIDL_MYVIDEO*/ break;
        case userPicturesDirectory:             csidlType = 0x27; /*CSIDL_MYPICTURES*/ break;

        case tempDirectory:
        {
            WCHAR dest[2048];
            dest[0] = 0;
            GetTempPath ((DWORD) numElementsInArray (dest), dest);
            return File (String (dest));
        }

        case windowsSystemDirectory:
        {
            WCHAR dest[2048];
            dest[0] = 0;
            GetSystemDirectoryW (dest, (UINT) numElementsInArray (dest));
            return File (String (dest));
        }

        case invokedExecutableFile:
        case currentExecutableFile:
        case currentApplicationFile:
            return WindowsFileHelpers::getModuleFileName ((HINSTANCE) Process::getCurrentModuleInstanceHandle());

        case hostApplicationPath:
            return WindowsFileHelpers::getModuleFileName (0);

        default:
            jassertfalse; // unknown type?
            return {};
    }

    return WindowsFileHelpers::getSpecialFolderPath (csidlType);
}

//==============================================================================
File File::getCurrentWorkingDirectory()
{
    WCHAR dest[MAX_PATH + 256];
    dest[0] = 0;
    GetCurrentDirectory ((DWORD) numElementsInArray (dest), dest);
    return File (String (dest));
}

bool File::setAsCurrentWorkingDirectory() const
{
    return SetCurrentDirectory (getFullPathName().toWideCharPointer()) != FALSE;
}

//==============================================================================
String File::getVersion() const
{
    String result;

    DWORD handle = 0;
    DWORD bufferSize = GetFileVersionInfoSize (getFullPathName().toWideCharPointer(), &handle);
    HeapBlock<char> buffer;
    buffer.calloc (bufferSize);

    if (GetFileVersionInfo (getFullPathName().toWideCharPointer(), 0, bufferSize, buffer))
    {
        VS_FIXEDFILEINFO* vffi;
        UINT len = 0;

        if (VerQueryValue (buffer, (LPTSTR) _T("\\"), (LPVOID*) &vffi, &len))
        {
            result << (int) HIWORD (vffi->dwFileVersionMS) << '.'
                   << (int) LOWORD (vffi->dwFileVersionMS) << '.'
                   << (int) HIWORD (vffi->dwFileVersionLS) << '.'
                   << (int) LOWORD (vffi->dwFileVersionLS);
        }
    }

    return result;
}

//==============================================================================
bool File::isSymbolicLink() const
{
    return (GetFileAttributes (fullPath.toWideCharPointer()) & FILE_ATTRIBUTE_REPARSE_POINT) != 0;
}

bool File::isShortcut() const
{
    return hasFileExtension (".lnk");
}

static String readWindowsLnkFile (File lnkFile, bool wantsAbsolutePath)
{
    if (! lnkFile.exists())
        lnkFile = File (lnkFile.getFullPathName() + ".lnk");

    if (lnkFile.exists())
    {
        ComSmartPtr<IShellLink> shellLink;
        ComSmartPtr<IPersistFile> persistFile;

        if (SUCCEEDED (shellLink.CoCreateInstance (CLSID_ShellLink))
             && SUCCEEDED (shellLink.QueryInterface (persistFile))
             && SUCCEEDED (persistFile->Load (lnkFile.getFullPathName().toWideCharPointer(), STGM_READ))
             && (! wantsAbsolutePath || SUCCEEDED (shellLink->Resolve (0, SLR_ANY_MATCH | SLR_NO_UI))))
        {
            WIN32_FIND_DATA winFindData;
            WCHAR resolvedPath[MAX_PATH];

            DWORD flags = SLGP_UNCPRIORITY;

            if (! wantsAbsolutePath)
                flags |= SLGP_RAWPATH;

            if (SUCCEEDED (shellLink->GetPath (resolvedPath, MAX_PATH, &winFindData, flags)))
                return resolvedPath;
        }
    }

    return {};
}

static String readWindowsShortcutOrLink (const File& shortcut, bool wantsAbsolutePath)
{
   #if JUCE_WINDOWS
    if (! wantsAbsolutePath)
    {
        HANDLE h = CreateFile (shortcut.getFullPathName().toWideCharPointer(),
                               GENERIC_READ, FILE_SHARE_READ, nullptr, OPEN_EXISTING,
                               FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT,
                               0);

        if (h != INVALID_HANDLE_VALUE)
        {
            HeapBlock<WindowsFileHelpers::REPARSE_DATA_BUFFER> reparseData;

            reparseData.calloc (1, MAXIMUM_REPARSE_DATA_BUFFER_SIZE);
            DWORD bytesReturned = 0;

            bool success = DeviceIoControl (h, FSCTL_GET_REPARSE_POINT, nullptr, 0,
                                            reparseData.getData(), MAXIMUM_REPARSE_DATA_BUFFER_SIZE,
                                            &bytesReturned, nullptr) != 0;
             CloseHandle (h);

            if (success)
            {
                if (IsReparseTagMicrosoft (reparseData->ReparseTag))
                {
                    String targetPath;

                    switch (reparseData->ReparseTag)
                    {
                        case IO_REPARSE_TAG_SYMLINK:
                        {
                            auto& symlinkData = reparseData->SymbolicLinkReparseBuffer;
                            targetPath = {symlinkData.PathBuffer + (symlinkData.SubstituteNameOffset / sizeof (WCHAR)),
                                          symlinkData.SubstituteNameLength / sizeof (WCHAR)};
                        }
                        break;

                        case IO_REPARSE_TAG_MOUNT_POINT:
                        {
                            auto& mountData = reparseData->MountPointReparseBuffer;
                            targetPath = {mountData.PathBuffer + (mountData.SubstituteNameOffset / sizeof (WCHAR)),
                                          mountData.SubstituteNameLength / sizeof (WCHAR)};
                        }
                        break;

                        default:
                            break;
                    }

                    if (targetPath.isNotEmpty())
                    {
                        const StringRef prefix ("\\??\\");

                        if (targetPath.startsWith (prefix))
                            targetPath = targetPath.substring (prefix.length());

                        return targetPath;
                    }
                }
            }
        }
    }

    if (! wantsAbsolutePath)
        return readWindowsLnkFile (shortcut, false);

    typedef DWORD (WINAPI* GetFinalPathNameByHandleFunc) (HANDLE, LPTSTR, DWORD, DWORD);

    static GetFinalPathNameByHandleFunc getFinalPathNameByHandle
             = (GetFinalPathNameByHandleFunc) getUser32Function ("GetFinalPathNameByHandle");

    if (getFinalPathNameByHandle != nullptr)
    {
        HANDLE h = CreateFile (shortcut.getFullPathName().toWideCharPointer(),
                               GENERIC_READ, FILE_SHARE_READ, nullptr,
                               OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);

        if (h != INVALID_HANDLE_VALUE)
        {
            if (DWORD requiredSize = getFinalPathNameByHandle (h, nullptr, 0, 0 /* FILE_NAME_NORMALIZED */))
            {
                HeapBlock<WCHAR> buffer (requiredSize + 2, true);

                if (getFinalPathNameByHandle (h, buffer, requiredSize, 0 /* FILE_NAME_NORMALIZED */) > 0)
                {
                    CloseHandle (h);

                    const StringRef prefix ("\\\\?\\");
                    const String path (buffer.get());

                    // It turns out that GetFinalPathNameByHandleW prepends \\?\ to the path.
                    // This is not a bug, it's feature. See MSDN for more information.
                    return path.startsWith (prefix) ? path.substring (prefix.length()) : path;
                }
            }

            CloseHandle (h);
        }
    }
   #endif

    // as last resort try the resolve method of the ShellLink
    return readWindowsLnkFile (shortcut, true);
}

String File::getNativeLinkedTarget() const
{
    return readWindowsShortcutOrLink (*this, false);
}

File File::getLinkedTarget() const
{
    auto target = readWindowsShortcutOrLink (*this, true);

    if (target.isNotEmpty() && File::isAbsolutePath (target))
        return File (target);

    return *this;
}

bool File::createShortcut (const String& description, const File& linkFileToCreate) const
{
    linkFileToCreate.deleteFile();

    ComSmartPtr<IShellLink> shellLink;
    ComSmartPtr<IPersistFile> persistFile;

    CoInitialize (0);

    return SUCCEEDED (shellLink.CoCreateInstance (CLSID_ShellLink))
        && SUCCEEDED (shellLink->SetPath (getFullPathName().toWideCharPointer()))
        && SUCCEEDED (shellLink->SetDescription (description.toWideCharPointer()))
        && SUCCEEDED (shellLink.QueryInterface (persistFile))
        && SUCCEEDED (persistFile->Save (linkFileToCreate.getFullPathName().toWideCharPointer(), TRUE));
}

//==============================================================================
class DirectoryIterator::NativeIterator::Pimpl
{
public:
    Pimpl (const File& directory, const String& wildCard)
        : directoryWithWildCard (directory.getFullPathName().isNotEmpty() ? File::addTrailingSeparator (directory.getFullPathName()) + wildCard : String()),
          handle (INVALID_HANDLE_VALUE)
    {
    }

    ~Pimpl()
    {
        if (handle != INVALID_HANDLE_VALUE)
            FindClose (handle);
    }

    bool next (String& filenameFound,
               bool* const isDir, bool* const isHidden, int64* const fileSize,
               Time* const modTime, Time* const creationTime, bool* const isReadOnly)
    {
        using namespace WindowsFileHelpers;
        WIN32_FIND_DATA findData;

        if (handle == INVALID_HANDLE_VALUE)
        {
            handle = FindFirstFile (directoryWithWildCard.toWideCharPointer(), &findData);

            if (handle == INVALID_HANDLE_VALUE)
                return false;
        }
        else
        {
            if (FindNextFile (handle, &findData) == 0)
                return false;
        }

        filenameFound = findData.cFileName;

        if (isDir != nullptr)         *isDir        = ((findData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0);
        if (isHidden != nullptr)      *isHidden     = ((findData.dwFileAttributes & FILE_ATTRIBUTE_HIDDEN) != 0);
        if (isReadOnly != nullptr)    *isReadOnly   = ((findData.dwFileAttributes & FILE_ATTRIBUTE_READONLY) != 0);
        if (fileSize != nullptr)      *fileSize     = findData.nFileSizeLow + (((int64) findData.nFileSizeHigh) << 32);
        if (modTime != nullptr)       *modTime      = Time (fileTimeToTime (&findData.ftLastWriteTime));
        if (creationTime != nullptr)  *creationTime = Time (fileTimeToTime (&findData.ftCreationTime));

        return true;
    }

private:
    const String directoryWithWildCard;
    HANDLE handle;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (Pimpl)
};

DirectoryIterator::NativeIterator::NativeIterator (const File& directory, const String& wildCard)
    : pimpl (new DirectoryIterator::NativeIterator::Pimpl (directory, wildCard))
{
}

DirectoryIterator::NativeIterator::~NativeIterator()
{
}

bool DirectoryIterator::NativeIterator::next (String& filenameFound,
                                              bool* const isDir, bool* const isHidden, int64* const fileSize,
                                              Time* const modTime, Time* const creationTime, bool* const isReadOnly)
{
    return pimpl->next (filenameFound, isDir, isHidden, fileSize, modTime, creationTime, isReadOnly);
}


//==============================================================================
bool JUCE_CALLTYPE Process::openDocument (const String& fileName, const String& parameters)
{
    HINSTANCE hInstance = ShellExecute (0, 0, fileName.toWideCharPointer(),
                                        parameters.toWideCharPointer(), 0, SW_SHOWDEFAULT);

    return hInstance > (HINSTANCE) 32;
}

void File::revealToUser() const
{
    DynamicLibrary dll ("Shell32.dll");
    JUCE_LOAD_WINAPI_FUNCTION (dll, ILCreateFromPathW, ilCreateFromPathW, ITEMIDLIST*, (LPCWSTR))
    JUCE_LOAD_WINAPI_FUNCTION (dll, ILFree, ilFree, void, (ITEMIDLIST*))
    JUCE_LOAD_WINAPI_FUNCTION (dll, SHOpenFolderAndSelectItems, shOpenFolderAndSelectItems, HRESULT, (ITEMIDLIST*, UINT, void*, DWORD))

    if (ilCreateFromPathW != nullptr && shOpenFolderAndSelectItems != nullptr && ilFree != nullptr)
    {
        if (ITEMIDLIST* const itemIDList = ilCreateFromPathW (fullPath.toWideCharPointer()))
        {
            shOpenFolderAndSelectItems (itemIDList, 0, nullptr, 0);
            ilFree (itemIDList);
        }
    }
}

//==============================================================================
class NamedPipe::Pimpl
{
public:
    Pimpl (const String& pipeName, const bool createPipe, bool mustNotExist)
        : filename ("\\\\.\\pipe\\" + File::createLegalFileName (pipeName)),
          pipeH (INVALID_HANDLE_VALUE),
          cancelEvent (CreateEvent (0, FALSE, FALSE, 0)),
          connected (false), ownsPipe (createPipe), shouldStop (false)
    {
        if (createPipe)
        {
            pipeH = CreateNamedPipe (filename.toWideCharPointer(),
                                     PIPE_ACCESS_DUPLEX | FILE_FLAG_OVERLAPPED, 0,
                                     PIPE_UNLIMITED_INSTANCES, 4096, 4096, 0, 0);

            if (mustNotExist && GetLastError() == ERROR_ALREADY_EXISTS)
                closePipeHandle();
        }
    }

    ~Pimpl()
    {
        closePipeHandle();
        CloseHandle (cancelEvent);
    }

    bool connect (const int timeOutMs)
    {
        if (! ownsPipe)
        {
            if (pipeH != INVALID_HANDLE_VALUE)
                return true;

            const Time timeOutEnd (Time::getCurrentTime() + RelativeTime::milliseconds (timeOutMs));

            for (;;)
            {
                {
                    const ScopedLock sl (createFileLock);

                    if (pipeH == INVALID_HANDLE_VALUE)
                        pipeH = CreateFile (filename.toWideCharPointer(),
                                            GENERIC_READ | GENERIC_WRITE, 0, 0,
                                            OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0);
                }

                if (pipeH != INVALID_HANDLE_VALUE)
                    return true;

                if (shouldStop || (timeOutMs >= 0 && Time::getCurrentTime() > timeOutEnd))
                    return false;

                Thread::sleep (1);
            }
        }

        if (! connected)
        {
            OverlappedEvent over;

            if (ConnectNamedPipe (pipeH, &over.over) == 0)
            {
                switch (GetLastError())
                {
                    case ERROR_PIPE_CONNECTED:   connected = true; break;
                    case ERROR_IO_PENDING:
                    case ERROR_PIPE_LISTENING:   connected = waitForIO (over, timeOutMs); break;
                    default: break;
                }
            }
        }

        return connected;
    }

    void disconnectPipe()
    {
        if (ownsPipe && connected)
        {
            DisconnectNamedPipe (pipeH);
            connected = false;
        }
    }

    void closePipeHandle()
    {
        if (pipeH != INVALID_HANDLE_VALUE)
        {
            disconnectPipe();
            CloseHandle (pipeH);
            pipeH = INVALID_HANDLE_VALUE;
        }
    }

    int read (void* destBuffer, const int maxBytesToRead, const int timeOutMilliseconds)
    {
        while (connect (timeOutMilliseconds))
        {
            if (maxBytesToRead <= 0)
                return 0;

            OverlappedEvent over;
            unsigned long numRead;

            if (ReadFile (pipeH, destBuffer, (DWORD) maxBytesToRead, &numRead, &over.over))
                return (int) numRead;

            const DWORD lastError = GetLastError();

            if (lastError == ERROR_IO_PENDING)
            {
                if (! waitForIO (over, timeOutMilliseconds))
                    return -1;

                if (GetOverlappedResult (pipeH, &over.over, &numRead, FALSE))
                    return (int) numRead;
            }

            if (ownsPipe && (GetLastError() == ERROR_BROKEN_PIPE || GetLastError() == ERROR_PIPE_NOT_CONNECTED))
                disconnectPipe();
            else
                break;
        }

        return -1;
    }

    int write (const void* sourceBuffer, int numBytesToWrite, int timeOutMilliseconds)
    {
        if (connect (timeOutMilliseconds))
        {
            if (numBytesToWrite <= 0)
                return 0;

            OverlappedEvent over;
            unsigned long numWritten;

            if (WriteFile (pipeH, sourceBuffer, (DWORD) numBytesToWrite, &numWritten, &over.over))
                return (int) numWritten;

            if (GetLastError() == ERROR_IO_PENDING)
            {
                if (! waitForIO (over, timeOutMilliseconds))
                    return -1;

                if (GetOverlappedResult (pipeH, &over.over, &numWritten, FALSE))
                    return (int) numWritten;

                if (GetLastError() == ERROR_BROKEN_PIPE && ownsPipe)
                    disconnectPipe();
            }
        }

        return -1;
    }

    const String filename;
    HANDLE pipeH, cancelEvent;
    bool connected, ownsPipe, shouldStop;
    CriticalSection createFileLock;

private:
    struct OverlappedEvent
    {
        OverlappedEvent()
        {
            zerostruct (over);
            over.hEvent = CreateEvent (0, TRUE, FALSE, 0);
        }

        ~OverlappedEvent()
        {
            CloseHandle (over.hEvent);
        }

        OVERLAPPED over;
    };

    bool waitForIO (OverlappedEvent& over, int timeOutMilliseconds)
    {
        if (shouldStop)
            return false;

        HANDLE handles[] = { over.over.hEvent, cancelEvent };
        DWORD waitResult = WaitForMultipleObjects (2, handles, FALSE,
                                                   timeOutMilliseconds >= 0 ? timeOutMilliseconds
                                                                            : INFINITE);

        if (waitResult == WAIT_OBJECT_0)
            return true;

        CancelIo (pipeH);
        return false;
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (Pimpl)
};

void NamedPipe::close()
{
    if (pimpl != nullptr)
    {
        pimpl->shouldStop = true;
        SetEvent (pimpl->cancelEvent);

        ScopedWriteLock sl (lock);
        pimpl.reset();
    }
}

bool NamedPipe::openInternal (const String& pipeName, const bool createPipe, bool mustNotExist)
{
    pimpl.reset (new Pimpl (pipeName, createPipe, mustNotExist));

    if (createPipe)
    {
        if (pimpl->pipeH == INVALID_HANDLE_VALUE)
        {
            pimpl.reset();
            return false;
        }
    }
    else if (! pimpl->connect (200))
    {
        pimpl.reset();
        return false;
    }

    return true;
}

int NamedPipe::read (void* destBuffer, int maxBytesToRead, int timeOutMilliseconds)
{
    ScopedReadLock sl (lock);
    return pimpl != nullptr ? pimpl->read (destBuffer, maxBytesToRead, timeOutMilliseconds) : -1;
}

int NamedPipe::write (const void* sourceBuffer, int numBytesToWrite, int timeOutMilliseconds)
{
    ScopedReadLock sl (lock);
    return pimpl != nullptr ? pimpl->write (sourceBuffer, numBytesToWrite, timeOutMilliseconds) : -1;
}

} // namespace juce
