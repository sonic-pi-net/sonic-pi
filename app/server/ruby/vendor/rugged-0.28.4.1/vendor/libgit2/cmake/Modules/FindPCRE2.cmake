# Copyright (C) 2007-2009 LuaDist.
# Created by Peter Kapec <kapecp@gmail.com>
# Redistribution and use of this file is allowed according to the terms of the MIT license.
# For details see the COPYRIGHT file distributed with LuaDist.
#	Note:
#		Searching headers and libraries is very simple and is NOT as powerful as scripts
#		distributed with CMake, because LuaDist defines directories to search for.
#		Everyone is encouraged to contact the author with improvements. Maybe this file
#		becomes part of CMake distribution sometimes.

# - Find pcre
# Find the native PCRE2 headers and libraries.
#
# PCRE2_INCLUDE_DIRS	- where to find pcre.h, etc.
# PCRE2_LIBRARIES	- List of libraries when using pcre.
# PCRE2_FOUND	- True if pcre found.

# Look for the header file.
FIND_PATH(PCRE2_INCLUDE_DIR NAMES pcre2posix.h)

# Look for the library.
FIND_LIBRARY(PCRE2_LIBRARY NAMES pcre2-8)

# Handle the QUIETLY and REQUIRED arguments and set PCRE2_FOUND to TRUE if all listed variables are TRUE.
INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(PCRE2 DEFAULT_MSG PCRE2_LIBRARY PCRE2_INCLUDE_DIR)

# Copy the results to the output variables.
IF(PCRE2_FOUND)
	SET(PCRE2_LIBRARIES ${PCRE2_LIBRARY})
	SET(PCRE2_INCLUDE_DIRS ${PCRE2_INCLUDE_DIR})
ELSE(PCRE2_FOUND)
	SET(PCRE2_LIBRARIES)
	SET(PCRE2_INCLUDE_DIRS)
ENDIF(PCRE2_FOUND)

MARK_AS_ADVANCED(PCRE2_INCLUDE_DIRS PCRE2_LIBRARIES)
