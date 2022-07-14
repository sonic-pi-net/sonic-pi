/*
  Copyright (C) 2016 Paul Brossier <piem@aubio.org>

  This file is part of aubio.

  aubio is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  aubio is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with aubio.  If not, see <http://www.gnu.org/licenses/>.

*/

/** @file

  Windows dll entry point.

*/

#include "aubio_priv.h"

#ifdef HAVE_WIN_HACKS

#ifndef __GNUC__ // do not include msvc headers when using gcc/mingw32

// latest version
#include <SDKDDKVer.h>
// for earlier versions, include WinSDKVer.h and set _WIN32_WINNT macro

#endif /* __GNUC__ */

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include "aubio.h"

BOOL APIENTRY DllMain( HMODULE hModule UNUSED,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved UNUSED)
{
  switch (ul_reason_for_call)
  {
    case DLL_PROCESS_ATTACH:
    case DLL_THREAD_ATTACH:
    case DLL_THREAD_DETACH:
    case DLL_PROCESS_DETACH:
      break;
  }
  return TRUE;
}

#endif
