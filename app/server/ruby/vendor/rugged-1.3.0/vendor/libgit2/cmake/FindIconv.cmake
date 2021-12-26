# - Try to find Iconv
# Once done this will define
#
# ICONV_FOUND - system has Iconv
# ICONV_INCLUDE_DIR - the Iconv include directory
# ICONV_LIBRARIES - Link these to use Iconv
#

IF(ICONV_INCLUDE_DIR AND ICONV_LIBRARIES)
	# Already in cache, be silent
	SET(ICONV_FIND_QUIETLY TRUE)
ENDIF()

FIND_PATH(ICONV_INCLUDE_DIR iconv.h)
CHECK_FUNCTION_EXISTS(iconv_open libc_has_iconv)
FIND_LIBRARY(iconv_lib NAMES iconv libiconv libiconv-2 c)

IF(ICONV_INCLUDE_DIR AND libc_has_iconv)
	SET(ICONV_FOUND TRUE)
	SET(ICONV_LIBRARIES "")
	IF(NOT ICONV_FIND_QUIETLY)
		MESSAGE(STATUS "Found Iconv: provided by libc")
	ENDIF(NOT ICONV_FIND_QUIETLY)
ELSEIF(ICONV_INCLUDE_DIR AND iconv_lib)
	SET(ICONV_FOUND TRUE)
	# split iconv into -L and -l linker options, so we can
	# set them for pkg-config
	GET_FILENAME_COMPONENT(iconv_path ${iconv_lib} PATH)
	GET_FILENAME_COMPONENT(iconv_name ${iconv_lib} NAME_WE)
	STRING(REGEX REPLACE "^lib" "" iconv_name ${iconv_name})
	SET(ICONV_LIBRARIES "-L${iconv_path} -l${iconv_name}")

	IF(NOT ICONV_FIND_QUIETLY)
		MESSAGE(STATUS "Found Iconv: ${ICONV_LIBRARIES}")
	ENDIF(NOT ICONV_FIND_QUIETLY)
ELSE()
	IF(Iconv_FIND_REQUIRED)
		MESSAGE(FATAL_ERROR "Could not find Iconv")
	ENDIF(Iconv_FIND_REQUIRED)
ENDIF()

MARK_AS_ADVANCED(
	ICONV_INCLUDE_DIR
	ICONV_LIBRARIES
)
