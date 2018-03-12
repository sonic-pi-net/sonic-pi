FILE(GLOB SRC_HTTP "*.c" "*.h")

ADD_LIBRARY(http-parser OBJECT ${SRC_HTTP})
