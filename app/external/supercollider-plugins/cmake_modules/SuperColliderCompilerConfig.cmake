# Brian Heim
# 2018-08-26
#
# Compiler configuration help for server plugins

function(sc_do_initial_compiler_config)
    # assume we are not mixing C and C++ compiler vendors
    if(CMAKE_VERSION VERSION_LESS 3.10)
        # slower/more complicated way
        include(CheckCCompilerFlag)
        include(CheckCXXCompilerFlag)
        if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|AppleClang|GNU")
            CHECK_CXX_COMPILER_FLAG(-msse has_sse)
            CHECK_CXX_COMPILER_FLAG(-msse2 has_sse2)
            CHECK_CXX_COMPILER_FLAG(-mfpmath=sse has_sse_fp)
        elseif(CMAKE_CXX_COMPILER_ID MATCHES "MSVC")
            CHECK_CXX_COMPILER_FLAG(/arch:SSE has_sse)
            CHECK_CXX_COMPILER_FLAG(/arch:SSE2 has_sse2)
        else()
            message(WARNING "Unknown compiler: ${CMAKE_CXX_COMPILER_ID}. You may want to modify SuperColliderCompilerConfig.cmake to add checks for SIMD flags and other optimizations.")
        endif()
    else()
        cmake_host_system_information(RESULT has_sse QUERY HAS_SSE)
        cmake_host_system_information(RESULT has_sse2 QUERY HAS_SSE2)
        cmake_host_system_information(RESULT has_sse_fp QUERY HAS_SSE_FP)
    endif()
endfunction()

function(sc_config_compiler_flags target)
    if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|AppleClang|GNU")
        target_compile_options(${target} PUBLIC
            $<$<BOOL:${has_sse}>:-msse>
            $<$<BOOL:${has_sse2}>:-msse2>
            $<$<BOOL:${has_sse_fp}>:-mfpmath=sse>
            $<$<BOOL:${NATIVE}>:-march=native>
            $<$<BOOL:${STRICT}>:-Wall -Wextra -Werror -Wpedantic>
            )
    elseif(CMAKE_CXX_COMPILER_ID MATCHES "MSVC")
        # these options only apply if we're doing a 32-bit build, otherwise they cause a diagnostic
        # https://stackoverflow.com/questions/1067630/sse2-option-in-visual-c-x64
        if(CMAKE_SIZEOF_VOID_P EQUAL 4)
            target_compile_options(${target} PUBLIC
                $<$<BOOL:${has_sse}>:/arch:SSE>
                $<$<BOOL:${has_sse2}>:/arch:SSE2>
                )
        endif()
        if(NATIVE)
            message(WARNING "-DNATIVE is not supported with MSVC")
        endif()
        # C4514: inline function not used
        # C4625: copy ctor implicitly deleted
        # C4626: copy assign implicitly deleted
        # C4820: padding added after member
        # C5026: move ctor implicitly deleted
        # C5027: move assign implicitly deleted
        target_compile_options(${target} PUBLIC
            $<$<BOOL:${STRICT}>:-Wall -WX -wd4820 -wd4514 -wd5026 -wd5027 -wd4626 -wd4625>
            )
    else()
        message(WARNING "Unknown compiler: ${CMAKE_CXX_COMPILER_ID}. You may want to modify SuperColliderCompilerConfig.cmake to add checks for SIMD flags and other optimizations.")
    endif()
endfunction()
