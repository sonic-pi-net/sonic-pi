INCLUDE(EnableWarnings)

IF (APPLE)
	# We cannot simply CHECK_FUNCTION_EXISTS on macOS because
	# MACOSX_DEPLOYMENT_TARGET may be set to a version in the past
	# that doesn't have futimens.  Instead we need to enable warnings
	# as errors, then check for the symbol existing in `sys/stat.h`,
	# then reset warnings as errors.
	ENABLE_WARNINGS(error)
	CHECK_SYMBOL_EXISTS(futimens sys/stat.h HAVE_FUTIMENS)
	DISABLE_WARNINGS(error)
ELSE ()
	CHECK_FUNCTION_EXISTS(futimens HAVE_FUTIMENS)
ENDIF ()
