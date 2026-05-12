# Vendored Spout2 (DirectX 11 sender subset). Windows only.
# Exposes the `spout_vendored` static target. See README.md for
# upstream version and local patches.

if(NOT WIN32)
    return()
endif()

set(SPOUTSDK_DIR ${CMAKE_CURRENT_SOURCE_DIR}/SPOUTSDK)

add_library(spout_vendored STATIC
    ${SPOUTSDK_DIR}/SpoutGL/SpoutCopy.cpp
    ${SPOUTSDK_DIR}/SpoutGL/SpoutDirectX.cpp
    ${SPOUTSDK_DIR}/SpoutGL/SpoutFrameCount.cpp
    ${SPOUTSDK_DIR}/SpoutGL/SpoutSenderNames.cpp
    ${SPOUTSDK_DIR}/SpoutGL/SpoutSharedMemory.cpp
    ${SPOUTSDK_DIR}/SpoutGL/SpoutUtils.cpp
    ${SPOUTSDK_DIR}/SpoutDirectX/SpoutDX/SpoutDX.cpp
)

# PUBLIC — both paths are needed:
#  - SPOUTSDK/ resolves SpoutDX.h's `#include "SpoutGL\X.h"`
#  - SPOUTSDK/SpoutDirectX/SpoutDX resolves `#include "SpoutDX.h"`
target_include_directories(spout_vendored PUBLIC
    ${SPOUTSDK_DIR}
    ${SPOUTSDK_DIR}/SpoutDirectX/SpoutDX
)

# SPOUT_BUILD_DLL selects static-archive symbol decoration. Without it,
# some Spout headers default to __declspec(dllimport) depending on
# include order.
target_compile_definitions(spout_vendored PRIVATE SPOUT_BUILD_DLL)

# PUBLIC — system libs flow up to consumers.
target_link_libraries(spout_vendored PUBLIC
    d3d11 dxgi dxguid
    ole32 oleaut32 uuid shlwapi advapi32 Version
)

target_compile_features(spout_vendored PUBLIC cxx_std_17)

if(MSVC)
    target_compile_options(spout_vendored PRIVATE
        /W3 /D_CRT_SECURE_NO_WARNINGS)
endif()
