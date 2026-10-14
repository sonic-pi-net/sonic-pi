# The share link's codec (sharelink.h): native Sonic Pi links it, and the web's page runs it compiled to wasm
# (app/web/scripts/build-runtime.sh), so a link either writes the other reads. Format 1's raw DEFLATE is zlib's: the
# system's on macOS and Linux, vcpkg's on Windows. Format 2's zstd is vendored (zstd/, BSD), one C file. A link as a
# QR code is Project Nayuki's QR Code generator's (qrcodegen/, MIT).
project(sharelink)

find_package(ZLIB REQUIRED)

add_library(sharelink STATIC sharelink.cpp sharelink_table1.cpp sharelink_table2.cpp sharelink_qr.cpp qrcodegen/qrcodegen.cpp zstd/zstd.c)
target_include_directories(sharelink PUBLIC ${CMAKE_CURRENT_LIST_DIR})
target_compile_features(sharelink PUBLIC cxx_std_17)
target_link_libraries(sharelink PRIVATE ZLIB::ZLIB)
