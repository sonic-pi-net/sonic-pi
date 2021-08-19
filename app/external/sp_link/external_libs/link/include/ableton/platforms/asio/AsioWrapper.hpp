/* Copyright 2016, Ableton AG, Berlin. All rights reserved.
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  If you would like to incorporate Link into a proprietary software application,
 *  please contact <link-devs@ableton.com>.
 */

#pragma once

/*!
 * \brief Wrapper file for AsioStandalone library
 *
 * This file includes all necessary headers from the AsioStandalone library which are used
 * by Link.
 */

#if !defined(ESP_PLATFORM)
#pragma push_macro("ASIO_STANDALONE")
#define ASIO_STANDALONE 1

#pragma push_macro("ASIO_NO_TYPEID")
#define ASIO_NO_TYPEID 1
#endif

#if defined(LINK_PLATFORM_WINDOWS)
#pragma push_macro("INCL_EXTRA_HTON_FUNCTIONS")
#define INCL_EXTRA_HTON_FUNCTIONS 1
#endif

#if defined(WIN32) || defined(_WIN32)
#if !defined(_WIN32_WINNT)
#define _WIN32_WINNT 0x0501
#endif
#endif

#if defined(__clang__)
#pragma clang diagnostic push
#if __has_warning("-Wcomma")
#pragma clang diagnostic ignored "-Wcomma"
#endif
#if __has_warning("-Wshorten-64-to-32")
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#endif
#if __has_warning("-Wunused-local-typedef")
#pragma clang diagnostic ignored "-Wunused-local-typedef"
#endif
#endif

#if defined(_MSC_VER)
#define _SCL_SECURE_NO_WARNINGS 1
#pragma warning(push, 0)
#pragma warning(disable : 4242)
#pragma warning(disable : 4702)
#pragma warning(disable : 5204)
#endif

#include <asio.hpp>
#include <asio/system_timer.hpp>

#if defined(LINK_PLATFORM_WINDOWS)
#pragma pop_macro("INCL_EXTRA_HTON_FUNCTIONS")
#endif

#if !defined(ESP_PLATFORM)
#pragma pop_macro("ASIO_STANDALONE")
#pragma pop_macro("ASIO_NO_TYPEID")
#endif

#if defined(_MSC_VER)
#pragma warning(pop)
#undef _SCL_SECURE_NO_WARNINGS
#endif

#if defined(__clang__)
#pragma clang diagnostic pop
#endif
