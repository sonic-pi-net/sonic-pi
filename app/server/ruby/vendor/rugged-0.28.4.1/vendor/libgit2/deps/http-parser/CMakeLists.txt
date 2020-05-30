FILE(GLOB SRC_HTTP "*.c" "*.h")

ADD_LIBRARY(http-parser OBJECT ${SRC_HTTP})

ENABLE_WARNINGS(implicit-fallthrough=1)
