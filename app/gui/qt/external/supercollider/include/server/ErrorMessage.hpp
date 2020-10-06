/**
 *  \file ErrorMessage.hpp
 *
 *  \brief Error message generation.
 *
 *  This header provides generation of complex error messages and warnings for scsynth/supernova.
 *  The messages currently provided are:
 *  - API version mismatch
 *  - API version not found
 *
 *  $Author: Brian Heim $
 *
 *  \version 1.0
 *
 *  $Date: 2017-08-22 $
 *
 *  $Contact: brianlheim@gmail.com $
 *
 *  Created on: 2017-08-22
 *
 *  Original revision by Brian Heim, 2017-08-22
 */
/*
 *  Copyright (C) Brian Heim, 2017. All rights reserved.
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License as
 *  published by the Free Software Foundation; either version 2 of the
 *  License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301 USA
 */

#pragma once

#include <string>
#include <cassert>
#include <sstream>
#include <map>

namespace scsynth { namespace ErrorMessage {

/// String used to indent all messages.
std::string const indent = "    ";

/** \brief Formats a helpful multiline error string in the case of a plugin API mismatch.
 *
 * Extra lines are added to help clarify problems caused by the recent upgrade to API
 * version 3 in SuperCollider 3.9.
 *
 * \param utf8Filename the filename of the plugin represented as a UTF-8 string.
 * \param expectedVersion the API version of this server
 * \param actualVersion the API version reported by the plugin
 * \returns A multiline string with info about how to correct the error.
 */
std::string apiVersionMismatch(std::string const& utf8Filename, int const expectedVersion, int const actualVersion) {
    using namespace std;
    assert(expectedVersion != actualVersion);

    // both 1 and 2 were introduced in 3.6
    static map<int, string> const scVersionForAPIVersion = { { 1, "3.6.0" }, { 2, "3.6.0" }, { 3, "3.9.0" } };

    stringstream message;
    message << "ERROR: API version mismatch: " << utf8Filename << "\n";

    try {
        // actualVersion+1: the version that broke compatibility
        string const& scVersion = scVersionForAPIVersion.at(actualVersion + 1);
        message << indent << "This plugin is not compatible with SuperCollider >=" << scVersion << "\n";
        message << indent << "The plugin has not been loaded; please find or compile a newer version.\n";
    } catch (std::out_of_range const& exception) {
        message << indent << "This plugin uses an unknown version of the interface.\n";
        message << indent << "You may need to update SuperCollider in order to use it.\n";
    }

    // if it looks like sc3-plugins, give specific info to help
    if (utf8Filename.find("SC3plugins") != string::npos) {
        message << indent << "Releases of sc3-plugins can be downloaded from "
                << "https://github.com/supercollider/sc3-plugins/releases\n";
    }

    message << indent << "(Plugin's API version: " << actualVersion << ". Expected: " << expectedVersion << ")\n";

    return message.str();
}

/** \brief Formats a helpful error string in the case that the API version function wasn't found.
 *
 * \param utf8Filename the filename of the plugin represented as a UTF-8 string.
 */
std::string apiVersionNotFound(std::string const& utf8Filename) {
    return "ERROR: API version not found: " + utf8Filename + "\n" + indent
        + "This file may not be a SuperCollider plugin.\n";
}

} // namespace ErrorMessage

} // namespace scsynth
