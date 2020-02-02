add_executable(test_libFLAC++
    decoders.cpp
    encoders.cpp
    main.cpp
    metadata.cpp
    metadata_manip.cpp
    metadata_object.cpp)
target_link_libraries(test_libFLAC++ FLAC++ test_libs_common grabbag)
