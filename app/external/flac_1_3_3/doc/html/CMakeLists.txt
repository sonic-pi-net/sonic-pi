add_subdirectory(images)

install(FILES
    changelog.html
    developers.html
    documentation.html
    documentation_bugs.html
    documentation_example_code.html
    documentation_format_overview.html
    documentation_tools.html
    documentation_tools_flac.html
    documentation_tools_metaflac.html
    faq.html
    favicon.ico
    features.html
    flac.css
    format.html
    id.html
    index.html
    license.html
    ogg_mapping.html
DESTINATION "${CMAKE_INSTALL_DOCDIR}/html")
