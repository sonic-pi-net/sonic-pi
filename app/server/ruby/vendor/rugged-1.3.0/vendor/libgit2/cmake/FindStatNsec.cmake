INCLUDE(FeatureSummary)

CHECK_STRUCT_HAS_MEMBER ("struct stat" st_mtim "sys/types.h;sys/stat.h"
	HAVE_STRUCT_STAT_ST_MTIM LANGUAGE C)
CHECK_STRUCT_HAS_MEMBER ("struct stat" st_mtimespec "sys/types.h;sys/stat.h"
	HAVE_STRUCT_STAT_ST_MTIMESPEC LANGUAGE C)
CHECK_STRUCT_HAS_MEMBER("struct stat" st_mtime_nsec sys/stat.h
	HAVE_STRUCT_STAT_MTIME_NSEC LANGUAGE C)

IF (HAVE_STRUCT_STAT_ST_MTIM)
	CHECK_STRUCT_HAS_MEMBER("struct stat" st_mtim.tv_nsec sys/stat.h
		HAVE_STRUCT_STAT_NSEC LANGUAGE C)
ELSEIF (HAVE_STRUCT_STAT_ST_MTIMESPEC)
	CHECK_STRUCT_HAS_MEMBER("struct stat" st_mtimespec.tv_nsec sys/stat.h
		HAVE_STRUCT_STAT_NSEC LANGUAGE C)
ELSE ()
	SET( HAVE_STRUCT_STAT_NSEC ON )
ENDIF()

IF (HAVE_STRUCT_STAT_NSEC OR WIN32)
	OPTION( USE_NSEC		"Care about sub-second file mtimes and ctimes"	ON  )
ELSE()
	SET(USE_NSEC OFF)
ENDIF()

ADD_FEATURE_INFO(nanoseconds USE_NSEC "whether to use sub-second file mtimes and ctimes")
