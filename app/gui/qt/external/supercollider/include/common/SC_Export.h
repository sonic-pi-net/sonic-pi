/*
    SuperCollider real time audio synthesis system
    Copyright (c) 2010 Tim Blechmann. All rights reserved.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
*/

#pragma once

#if defined _WIN32 || defined __CYGWIN__
#    define SC_API_IMPORT __declspec(dllimport)
#    define SC_API_EXPORT __declspec(dllexport)
#else
#    if __GNUC__ >= 4
#        define SC_API_IMPORT __attribute__((visibility("default")))
#        define SC_API_EXPORT __attribute__((visibility("default")))
#    else
#        define SC_API_IMPORT
#        define SC_API_EXPORT
#    endif
#endif

#ifdef __cplusplus
#    define C_LINKAGE extern "C"
#else
#    define C_LINKAGE
#endif

#ifdef BUILDING_SCSYNTH // if scsynth is being built, instead of used
#    define SCSYNTH_DLLEXPORT_C C_LINKAGE SC_API_EXPORT
#    define SCSYNTH_DLLEXPORT SC_API_EXPORT
#elif defined(USING_SCSYNTH)
#    define SCSYNTH_DLLEXPORT_C C_LINKAGE SC_API_IMPORT
#    define SCSYNTH_DLLEXPORT SC_API_IMPORT
#else
#    define SCSYNTH_DLLEXPORT_C C_LINKAGE
#    define SCSYNTH_DLLEXPORT /*SC_API_IMPORT*/
#endif

#ifdef BUILDING_SCLANG // if sclang is being built, instead of used
#    define SCLANG_DLLEXPORT_C C_LINKAGE SC_API_EXPORT
#    define SCLANG_DLLEXPORT SC_API_EXPORT
#elif defined(USING_SCSYNTH)
#    define SCLANG_DLLEXPORT_C C_LINKAGE SC_API_IMPORT
#    define SCLANG_DLLEXPORT SC_API_IMPORT
#else
#    define SCLANG_DLLEXPORT_C C_LINKAGE
#    define SCLANG_DLLEXPORT /*SC_API_IMPORT*/
#endif
