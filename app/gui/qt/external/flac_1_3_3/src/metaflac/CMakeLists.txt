add_executable(metaflac
    main.c
    operations.c
    operations_shorthand_cuesheet.c
    operations_shorthand_picture.c
    operations_shorthand_seektable.c
    operations_shorthand_streaminfo.c
    operations_shorthand_vorbiscomment.c
    options.c
    usage.c
    utils.c)
target_link_libraries(metaflac FLAC getopt utf8)
if(TARGET win_utf8_io)
    target_link_libraries(metaflac win_utf8_io)
endif()

install(TARGETS metaflac EXPORT targets
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
