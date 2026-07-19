cmake_minimum_required(VERSION 3.13)

message(STATUS "CMakeLists: Sonic Pi Qt GUI")

if(APPLE)
  # Kept in sync with the parent app/CMakeLists.txt and with
  # LSMinimumSystemVersion in mac-release-03-info-plist.sh.
  set(CMAKE_OSX_DEPLOYMENT_TARGET "14")
endif()

project("Sonic Pi"
  LANGUAGES CXX C
  DESCRIPTION "A code-based music creation and performance tool"
  VERSION ${SONIC_PI_VERSION_NUMERIC}
  HOMEPAGE_URL "https://sonic-pi.net"
)


set_property(GLOBAL PROPERTY USE_FOLDERS ON)

set(QTAPP_ROOT ${CMAKE_CURRENT_LIST_DIR})

list(APPEND CMAKE_MODULE_PATH ${QTAPP_ROOT}/cmake)

# Append Qt's install location to CMAKE_PREFIX_PATH for find_package(Qt6).
# Use list(APPEND), not bare set() — set() clobbers the vcpkg toolchain's
# prefix-path additions and breaks find_package for any vcpkg dep
# referenced later in this file.
set(QT_INSTALL_PREFIX "")
if((${BUILD_32BIT}) AND (DEFINED ENV{QT_INSTALL_LOCATION32}))
  message(STATUS "Setting up a 32 bit Qt Build")
  message(STATUS "Setting prefix to user-defined Qt (32-bit) install location: $ENV{QT_INSTALL_LOCATION32}")
  set(QT_INSTALL_PREFIX $ENV{QT_INSTALL_LOCATION32})
elseif(DEFINED ENV{QT_INSTALL_LOCATION})
  message(STATUS "Setting prefix to user-defined Qt install location: $ENV{QT_INSTALL_LOCATION}")
  set(QT_INSTALL_PREFIX $ENV{QT_INSTALL_LOCATION})
elseif(APPLE AND EXISTS /opt/homebrew/opt/qt@6)
  # Homebrew installs Qt to /opt/homebrew/opt/qt@6
  message(STATUS "Setting prefix to Homebrew's Qt install location: /opt/homebrew/opt/qt@6")
  set(QT_INSTALL_PREFIX /opt/homebrew/opt/qt@6)
endif()
if(QT_INSTALL_PREFIX)
  list(APPEND CMAKE_PREFIX_PATH ${QT_INSTALL_PREFIX})
endif()

# Qt Setup
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

set(QT_TOOLS_DIR ${QT_INSTALL_PREFIX}/bin)

set_property(GLOBAL PROPERTY AUTOMOC_FOLDER Automoc)
set_property(GLOBAL PROPERTY AUTOGEN_TARGETS_FOLDER Automoc)

find_package(Qt6)

if(Qt6_FOUND)
  find_package(Qt6 COMPONENTS Core Widgets Gui Concurrent Network OpenGLWidgets PrintSupport Xml Svg LinguistTools REQUIRED)
  if(Qt6_VERSION VERSION_LESS 6.8)
    message(WARNING "Qt ${Qt6_VERSION} < 6.8: screen-reader announcements (QAccessibleAnnouncementEvent) will be compiled out. Build against Qt 6.8+ for full accessibility support.")
  endif()
  if(Qt6_VERSION VERSION_LESS 6.10)
    message(WARNING "Qt ${Qt6_VERSION} < 6.10: automatic OS high-contrast detection (QAccessibilityHints) will be compiled out; the high-contrast theme remains available as a manual pick in Preferences.")
  endif()
endif()

find_package(Threads REQUIRED)

if(NOT (APPLE OR WIN32))
  # Try to find the system-installed QScintilla without immediately failing
  find_package(QScintilla QUIET)
endif()

# If QScintilla wasn’t found use the vendored version.
if(NOT QScintilla_FOUND)
  message(STATUS "System QScintilla not found. Falling back to bundled QScintilla.")
  add_subdirectory(QScintilla_src-2.14.1)
endif()




set(APP_NAME sonic-pi)
set(MACOS_APP_NAME "Sonic Pi")

set(NEW_API_SOURCES
    ${QTAPP_ROOT}/qt_api_client.cpp
    ${QTAPP_ROOT}/qt_api_client.h
    ${QTAPP_ROOT}/visualizer/scope_window.cpp
    ${QTAPP_ROOT}/visualizer/scope_window.h
    )

set(EDITOR_SOURCES
    ${QTAPP_ROOT}/widgets/sonicpiscintilla.cpp
    ${QTAPP_ROOT}/widgets/sonicpierrorcard.cpp
    ${QTAPP_ROOT}/widgets/sonicpierrorcard.h
    ${QTAPP_ROOT}/widgets/sonicpierrorcodeline.h
    ${QTAPP_ROOT}/widgets/sonicpilexer.cpp
    ${QTAPP_ROOT}/widgets/sonicpilexer.h
    ${QTAPP_ROOT}/widgets/sonicpiscintilla.h
    ${QTAPP_ROOT}/widgets/completionpopup.cpp
    ${QTAPP_ROOT}/widgets/completionpopup.h
    ${QTAPP_ROOT}/widgets/findpopup.cpp
    ${QTAPP_ROOT}/widgets/findpopup.h
    )

set(QT_SOURCES
    ${QTAPP_ROOT}/widgets/infowidget.cpp
    ${QTAPP_ROOT}/widgets/settingswidget.cpp
    ${QTAPP_ROOT}/widgets/bpmscrubwidget.cpp
    ${QTAPP_ROOT}/widgets/bpmscrubwidget.h
    ${QTAPP_ROOT}/widgets/linkaudiostreamswidget.cpp
    ${QTAPP_ROOT}/widgets/linkaudiostreamswidget.h
    ${QTAPP_ROOT}/widgets/linkvisibilitytoggle.cpp
    ${QTAPP_ROOT}/widgets/linkvisibilitytoggle.h
    ${QTAPP_ROOT}/widgets/sonicpitooltip.cpp
    ${QTAPP_ROOT}/widgets/sonicpitooltip.h
    ${QTAPP_ROOT}/mainwindow.cpp
    ${QTAPP_ROOT}/mainwindow.h
    ${QTAPP_ROOT}/model/sonicpitheme.cpp
    ${QTAPP_ROOT}/model/sonicpitheme.h
    ${QTAPP_ROOT}/widgets/infowidget.h
    ${QTAPP_ROOT}/widgets/settingswidget.h
    ${QTAPP_ROOT}/widgets/devicelistwidget.cpp
    ${QTAPP_ROOT}/widgets/devicelistwidget.h
    ${QTAPP_ROOT}/widgets/chevronbutton.h
    ${QTAPP_ROOT}/widgets/logpanel.cpp
    ${QTAPP_ROOT}/widgets/logpanel.h
    ${QTAPP_ROOT}/widgets/metricspanel.cpp
    ${QTAPP_ROOT}/widgets/metricspanel.h
    ${QTAPP_ROOT}/widgets/nodetreegraph.cpp
    ${QTAPP_ROOT}/widgets/nodetreegraph.h
    ${QTAPP_ROOT}/widgets/tutorialpane.cpp
    ${QTAPP_ROOT}/widgets/tutorialpane.h
    ${QTAPP_ROOT}/widgets/tutorialwidgets.cpp
    ${QTAPP_ROOT}/widgets/tutorialwidgets.h
    ${QTAPP_ROOT}/widgets/welcomewidget.cpp
    ${QTAPP_ROOT}/widgets/welcomewidget.h
    ${QTAPP_ROOT}/widgets/splashwidget.cpp
    ${QTAPP_ROOT}/widgets/splashwidget.h
    ${QTAPP_ROOT}/widgets/quickstartpane.cpp
    ${QTAPP_ROOT}/widgets/quickstartpane.h
    ${QTAPP_ROOT}/widgets/zoombar.cpp
    ${QTAPP_ROOT}/widgets/zoombar.h
    ${QTAPP_ROOT}/widgets/tutscope.h
    ${QTAPP_ROOT}/widgets/icontabbar.h
    ${QTAPP_ROOT}/utils/flash_style.h
    ${QTAPP_ROOT}/utils/tutorialdocs.cpp
    ${QTAPP_ROOT}/utils/tutorialdocs.h
    )

set(SOURCES
    ${QTAPP_ROOT}/main.cpp
    ${QTAPP_ROOT}/utils/scintilla_api.cpp
    ${QTAPP_ROOT}/utils/setbundle.cpp
    ${QTAPP_ROOT}/utils/setbundle.h
    ${QTAPP_ROOT}/utils/completion_context.cpp
    ${QTAPP_ROOT}/utils/reducedmotion.cpp
    ${QTAPP_ROOT}/utils/reducedmotion.h
    ${QTAPP_ROOT}/utils/sonicpi_i18n.cpp
    ${QTAPP_ROOT}/widgets/sonicpilog.cpp
    ${QTAPP_ROOT}/widgets/sonicpilog.h
    ${QTAPP_ROOT}/widgets/sonicpicontext.cpp
    ${QTAPP_ROOT}/widgets/sonicpicontext.h
    ${QTAPP_ROOT}/widgets/sonicpimetro.cpp
    ${QTAPP_ROOT}/widgets/sonicpimetro.h
    ${QTAPP_ROOT}/widgets/timewarpedit.h
    ${QTAPP_ROOT}/widgets/timewarpedit.cpp
    ${QTAPP_ROOT}/widgets/sonicpieditor.cpp
    ${QTAPP_ROOT}/widgets/sonicpieditor.h
    ${QTAPP_ROOT}/utils/scintilla_api.h
    ${QTAPP_ROOT}/utils/sonicpi_i18n.h
    ${QTAPP_ROOT}/utils/ruby_help.h
    ${QTAPP_ROOT}/utils/lang_list.h
    ${QTAPP_ROOT}/model/settings.h
    )


SET(RESOURCES
    ${QTAPP_ROOT}/SonicPi.qrc
    ${QTAPP_ROOT}/help_files.qrc
    ${QTAPP_ROOT}/info_files.qrc
    ${QTAPP_ROOT}/SonicPi.rc
    ${QTAPP_ROOT}/images/app.icns
    ${QTAPP_ROOT}/images/set.icns
    )

if(APPLE)
  # Copied during the build proper (unlike RESOURCE-property files, which
  # land after POST_BUILD) so the re-sign at the end seals it correctly.
  set_source_files_properties(${QTAPP_ROOT}/images/set.icns PROPERTIES
    MACOSX_PACKAGE_LOCATION Resources)
endif()

SET(TS_FILES
    ${QTAPP_ROOT}/lang/sonic-pi_ar.ts
    ${QTAPP_ROOT}/lang/sonic-pi_bg.ts
    ${QTAPP_ROOT}/lang/sonic-pi_bn.ts
    ${QTAPP_ROOT}/lang/sonic-pi_bs.ts
    ${QTAPP_ROOT}/lang/sonic-pi_ca.ts
    ${QTAPP_ROOT}/lang/sonic-pi_ca@valencia.ts
    ${QTAPP_ROOT}/lang/sonic-pi_cs.ts
    ${QTAPP_ROOT}/lang/sonic-pi_da.ts
    ${QTAPP_ROOT}/lang/sonic-pi_de.ts
    ${QTAPP_ROOT}/lang/sonic-pi_el.ts
    ${QTAPP_ROOT}/lang/sonic-pi_en_AU.ts
    ${QTAPP_ROOT}/lang/sonic-pi_en_US.ts
    ${QTAPP_ROOT}/lang/sonic-pi_eo.ts
    ${QTAPP_ROOT}/lang/sonic-pi_es.ts
    ${QTAPP_ROOT}/lang/sonic-pi_et.ts
    ${QTAPP_ROOT}/lang/sonic-pi_eu.ts
    ${QTAPP_ROOT}/lang/sonic-pi_fa.ts
    ${QTAPP_ROOT}/lang/sonic-pi_fi.ts
    ${QTAPP_ROOT}/lang/sonic-pi_fr.ts
    ${QTAPP_ROOT}/lang/sonic-pi_ga.ts
    ${QTAPP_ROOT}/lang/sonic-pi_gl.ts
    ${QTAPP_ROOT}/lang/sonic-pi_he.ts
    ${QTAPP_ROOT}/lang/sonic-pi_hi.ts
    ${QTAPP_ROOT}/lang/sonic-pi_hu.ts
    ${QTAPP_ROOT}/lang/sonic-pi_hy.ts
    ${QTAPP_ROOT}/lang/sonic-pi_id.ts
    ${QTAPP_ROOT}/lang/sonic-pi_is.ts
    ${QTAPP_ROOT}/lang/sonic-pi_it.ts
    ${QTAPP_ROOT}/lang/sonic-pi_ja.ts
    ${QTAPP_ROOT}/lang/sonic-pi_ka.ts
    ${QTAPP_ROOT}/lang/sonic-pi_ko.ts
    ${QTAPP_ROOT}/lang/sonic-pi_nb.ts
    ${QTAPP_ROOT}/lang/sonic-pi_nl.ts
    ${QTAPP_ROOT}/lang/sonic-pi_pl.ts
    ${QTAPP_ROOT}/lang/sonic-pi_pt_BR.ts
    ${QTAPP_ROOT}/lang/sonic-pi_pt.ts
    ${QTAPP_ROOT}/lang/sonic-pi_ro.ts
    ${QTAPP_ROOT}/lang/sonic-pi_ru.ts
    ${QTAPP_ROOT}/lang/sonic-pi_si.ts
    ${QTAPP_ROOT}/lang/sonic-pi_sk.ts
    ${QTAPP_ROOT}/lang/sonic-pi_sl.ts
    ${QTAPP_ROOT}/lang/sonic-pi_sv.ts
    ${QTAPP_ROOT}/lang/sonic-pi_sw.ts
    ${QTAPP_ROOT}/lang/sonic-pi_th.ts
    ${QTAPP_ROOT}/lang/sonic-pi_tr.ts
    ${QTAPP_ROOT}/lang/sonic-pi_ug.ts
    ${QTAPP_ROOT}/lang/sonic-pi_uk.ts
    ${QTAPP_ROOT}/lang/sonic-pi_vi.ts
    ${QTAPP_ROOT}/lang/sonic-pi_zh_HK.ts
    ${QTAPP_ROOT}/lang/sonic-pi_zh_TW.ts
    ${QTAPP_ROOT}/lang/sonic-pi_zh-Hans.ts
    ${QTAPP_ROOT}/lang/sonic-pi_zh.ts
)

if(WIN32)
  SET (SOURCES ${SOURCES} ${QTAPP_ROOT}/platform/windows.h)
  SET (SOURCES ${SOURCES} ${QTAPP_ROOT}/platform/windows_a11y.cpp)
  SET (SOURCES ${SOURCES} ${QTAPP_ROOT}/platform/windows_a11y.h)
  SET (SOURCES ${SOURCES} ${QTAPP_ROOT}/platform/spout_publisher.cpp)
  SET (SOURCES ${SOURCES} ${QTAPP_ROOT}/platform/recorder_win.cpp)
  SET (SOURCES ${SOURCES} ${QTAPP_ROOT}/platform/mp4_soft_remux.cpp)
elseif(APPLE)
  SET (SOURCES ${SOURCES} ${QTAPP_ROOT}/platform/macos.mm)
  SET (SOURCES ${SOURCES} ${QTAPP_ROOT}/platform/macos.h)
  SET (SOURCES ${SOURCES} ${QTAPP_ROOT}/platform/syphon_publisher.mm)
  SET (SOURCES ${SOURCES} ${QTAPP_ROOT}/platform/recorder.mm)

  # Syphon-Framework — vendored, server-side Metal subset only (no GL,
  # no client). Compiled into the app binary as ARC Objective-C.
  set(SYPHON_DIR ${APP_ROOT}/external/Syphon-Framework)
  set(SYPHON_SOURCES
    ${SYPHON_DIR}/SyphonServerBase.m
    ${SYPHON_DIR}/SyphonMetalServer.m
    ${SYPHON_DIR}/SyphonServerConnectionManager.m
    ${SYPHON_DIR}/SyphonServerDirectory.m
    ${SYPHON_DIR}/SyphonServerRendererMetal.m
    ${SYPHON_DIR}/SyphonMessaging.m
    ${SYPHON_DIR}/SyphonMessageQueue.m
    ${SYPHON_DIR}/SyphonMessageReceiver.m
    ${SYPHON_DIR}/SyphonMessageSender.m
    ${SYPHON_DIR}/SyphonCFMessageReceiver.m
    ${SYPHON_DIR}/SyphonCFMessageSender.m
    ${SYPHON_DIR}/SyphonDispatch.c
    ${SYPHON_DIR}/SyphonPrivate.m
  )
  SET (SOURCES ${SOURCES} ${SYPHON_SOURCES})

  # Syphon sources expect Syphon_Prefix.pch; we substitute syphon_prefix.h.
  set_source_files_properties(${SYPHON_SOURCES}
    PROPERTIES COMPILE_FLAGS "-fobjc-arc -include ${QTAPP_ROOT}/platform/syphon_prefix.h")
  set_source_files_properties(${QTAPP_ROOT}/platform/syphon_publisher.mm
    PROPERTIES COMPILE_FLAGS "-fobjc-arc")
  set_source_files_properties(${QTAPP_ROOT}/platform/recorder.mm
    PROPERTIES COMPILE_FLAGS "-fobjc-arc")
endif()

set(ALL_SOURCES ${QT_SOURCES} ${SOURCES} ${EDITOR_SOURCES} ${OLD_API_SOURCES} ${NEW_API_SOURCES})

# Create the Qt version of the app
if (APPLE)
  add_executable(${APP_NAME} MACOSX_BUNDLE ${ALL_SOURCES} ${RESOURCES} )
else()
  add_executable(${APP_NAME} WIN32 ${ALL_SOURCES} ${RESOURCES}) # Win32 ignored on non-windows
endif()

# Translations: Convert the .ts files into .qm files
set_source_files_properties(${TS_FILES} PROPERTIES OUTPUT_LOCATION ${QTAPP_ROOT}/lang)



if (${Qt6Core_VERSION} VERSION_LESS "6.7.0")
  qt_add_lrelease(${APP_NAME} TS_FILES ${TS_FILES})
else()
  # Qt 6.7.0 and later...
  set(LRELEASE_TARGET "${APP_NAME}_lrelease")
  qt_add_lrelease(TS_FILES ${TS_FILES} LRELEASE_TARGET ${LRELEASE_TARGET})
endif()



if (APPLE)
  # ─── Codesign identity detection ─────────────────────────────────────────
  # Auto-detect a "Developer ID Application" identity from the user's
  # login keychain. Falls back to ad-hoc ("-") if none is installed.
  # Override with -DSONIC_PI_CODESIGN_IDENTITY="..." on the cmake command line.
  #
  # WHY: ad-hoc signatures have a cdhash that depends on binary contents,
  # so every rebuild produces a new TCC identity → macOS keeps re-prompting
  # for screen-recording / mic permissions. Dev-ID signatures bind TCC to
  # the team identity, which is stable across rebuilds — permissions stick.
  # Cached so reconfigures skip the `security` spawn. Pass
  # -DSONIC_PI_CODESIGN_IDENTITY=... to override, or remove the cache entry
  # (e.g. delete build/CMakeCache.txt) to re-detect after installing a new
  # Developer ID cert.
  if (NOT DEFINED CACHE{SONIC_PI_CODESIGN_IDENTITY})
    execute_process(
      COMMAND security find-identity -v -p codesigning
      OUTPUT_VARIABLE _sonic_pi_codesign_identities
      OUTPUT_STRIP_TRAILING_WHITESPACE
      ERROR_QUIET
    )
    # Match the 40-char SHA-1 cert hash from a "Developer ID Application"
    # line — passing the hash to codesign avoids the shell-quoting nightmare
    # the full quoted identity string would create (it contains spaces
    # and parens that mangle through CMake → make → /bin/sh).
    string(REGEX MATCH "[0-9]+\\) ([A-F0-9]+) \"Developer ID Application:"
      _sonic_pi_dev_id_match "${_sonic_pi_codesign_identities}")
    if (CMAKE_MATCH_1)
      set(SONIC_PI_CODESIGN_IDENTITY "${CMAKE_MATCH_1}" CACHE STRING
          "Codesign identity (Developer ID cert SHA-1 hash, or '-' for ad-hoc)")
      message(STATUS "Codesign identity: ${SONIC_PI_CODESIGN_IDENTITY} (Developer ID)")
    else()
      set(SONIC_PI_CODESIGN_IDENTITY "-" CACHE STRING
          "Codesign identity (Developer ID cert SHA-1 hash, or '-' for ad-hoc)")
      message(STATUS "Codesign identity: ad-hoc (no 'Developer ID Application' in keychain — TCC permissions WILL reset on every rebuild)")
    endif()
  endif()

  set_target_properties(
    ${APP_NAME} PROPERTIES
    OUTPUT_NAME "Sonic Pi"
    MACOSX_RPATH TRUE
    XCODE_ATTRIBUTE_ENABLE_HARDENED_RUNTIME TRUE
    MACOSX_BUNDLE_BUNDLE_NAME ${PROJECT_NAME}
    MACOSX_BUNDLE_GUI_IDENTIFIER "net.sonic-pi.app"
    MACOSX_BUNDLE_INFO_STRING "A code-based music creation and performance tool"
    MACOSX_BUNDLE_ICON_FILE "app.icns"
    MACOSX_BUNDLE_LONG_VERSION_STRING ${PROJECT_VERSION}
    MACOSX_BUNDLE_SHORT_VERSION_STRING ${PROJECT_VERSION}
    MACOSX_BUNDLE_BUNDLE_VERSION ${PROJECT_VERSION}
    RESOURCE "images/app.icns"
    LINK_FLAGS "-framework AppKit -framework AVFoundation -framework ScreenCaptureKit -framework Metal -framework IOSurface -framework CoreMedia -framework CoreVideo -framework OpenGL -framework VideoToolbox -framework IOKit"
    OUTPUT_NAME ${MACOS_APP_NAME}
    )
  # Locate macdeployqt. Prefer the Qt6 install we configured against
  # (QT_INSTALL_PREFIX), then fall back to whatever is on PATH (which historically
  # found qt@5 from Homebrew). Hard-coding /usr/local/opt/qt@5/bin breaks
  # builds against a stand-alone Qt6 install where macdeployqt isn't on PATH.
  if(QT_INSTALL_PREFIX AND EXISTS "${QT_INSTALL_PREFIX}/bin/macdeployqt")
    set(MACDEPLOYQT_BIN "${QT_INSTALL_PREFIX}/bin/macdeployqt")
  else()
    set(MACDEPLOYQT_BIN "macdeployqt")
  endif()
  message(STATUS "macdeployqt: ${MACDEPLOYQT_BIN}")
  file(
    GENERATE OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/run-macdeployqt.sh"
    CONTENT "PATH=$PATH:/usr/local/opt/qt@5/bin \"${MACDEPLOYQT_BIN}\" \"$1\""
    )

  add_custom_command(
    TARGET ${APP_NAME}
    POST_BUILD
    COMMAND bash "${CMAKE_CURRENT_BINARY_DIR}/run-macdeployqt.sh" ${MACOS_APP_NAME}.app
    )
  add_custom_command(
    TARGET ${APP_NAME}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E create_symlink ../../../../../../app app
    POST_BUILD
    WORKING_DIRECTORY ${MACOS_APP_NAME}.app/Contents/Resources
    )
  add_custom_command(
    TARGET ${APP_NAME}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E create_symlink ../../../../../server server
    POST_BUILD
    WORKING_DIRECTORY ${MACOS_APP_NAME}.app/Contents/Resources
    )
  add_custom_command(
    TARGET ${APP_NAME}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E create_symlink ../../../../../../etc etc
    POST_BUILD
    WORKING_DIRECTORY ${MACOS_APP_NAME}.app/Contents/Resources
    )
  add_custom_command(
    TARGET ${APP_NAME}
    POST_BUILD
    COMMAND plutil -replace NSHighResolutionCapable -bool true ${MACOS_APP_NAME}.app/Contents/Info.plist
  )
  add_custom_command(
    TARGET ${APP_NAME}
    POST_BUILD
    COMMAND plutil -replace NSMicrophoneUsageDescription -string "Sonic Pi needs microphone access for its live audio input features." ${MACOS_APP_NAME}.app/Contents/Info.plist
  )
  add_custom_command(
    TARGET ${APP_NAME}
    POST_BUILD
    COMMAND plutil -replace NSScreenCaptureUsageDescription -string "Sonic Pi captures its own window when you publish it as a Syphon video feed for VJ apps like Resolume Avenue." ${MACOS_APP_NAME}.app/Contents/Info.plist
  )
  # Register the .sonicpi set format so Finder double-clicks open Sonic Pi.
  add_custom_command(
    TARGET ${APP_NAME}
    POST_BUILD
    COMMAND plutil -replace UTExportedTypeDeclarations -json [=[[{"UTTypeIdentifier":"net.sonic-pi.set","UTTypeDescription":"Sonic Pi Set","UTTypeConformsTo":["public.data"],"UTTypeIconFile":"set.icns","UTTypeTagSpecification":{"public.filename-extension":["sonicpi"]}}]]=] ${MACOS_APP_NAME}.app/Contents/Info.plist
    COMMAND plutil -replace CFBundleDocumentTypes -json [=[[{"CFBundleTypeName":"Sonic Pi Set","CFBundleTypeRole":"Editor","LSHandlerRank":"Owner","CFBundleTypeIconFile":"set.icns","LSItemContentTypes":["net.sonic-pi.set"]}]]=] ${MACOS_APP_NAME}.app/Contents/Info.plist
    VERBATIM
  )

  # Compile Syphon's Metal shader at build time and embed the resulting
  # default.metallib in the bundle Resources. Upstream Syphon resolves it
  # via [NSBundle bundleForClass:[SyphonServerRendererMetal class]], which
  # — because the Syphon source is statically linked into Sonic Pi — is
  # the main app bundle. So Resources/default.metallib is the right path.
  #
  # Requires the Metal Toolchain. On Xcode 15+ it's a downloadable
  # component, free from Apple:
  #     xcodebuild -downloadComponent MetalToolchain
  #
  # `xcrun -f metal` returns 0 with a path even when the toolchain is
  # missing (the binary is a stub wrapper). Actually invoking it is the
  # only reliable probe — without the toolchain it exits non-zero with
  # the install hint. Fail at configure rather than 73% through the build.
  execute_process(
    COMMAND xcrun -sdk macosx metal --version
    RESULT_VARIABLE _metal_probe_result
    OUTPUT_QUIET ERROR_QUIET
  )
  if(NOT _metal_probe_result EQUAL 0)
    message(FATAL_ERROR
      "Metal Toolchain is required to compile Syphon's Metal shader but "
      "is not installed. On Xcode 15+ it is a separate downloadable "
      "component, free from Apple:\n"
      "    xcodebuild -downloadComponent MetalToolchain\n"
      "Then re-run CMake.")
  endif()

  set(SYPHON_METALLIB_AIR ${CMAKE_CURRENT_BINARY_DIR}/SyphonMetalShaders.air)
  set(SYPHON_METALLIB     ${CMAKE_CURRENT_BINARY_DIR}/default.metallib)
  add_custom_command(
    OUTPUT  ${SYPHON_METALLIB_AIR}
    COMMAND xcrun -sdk macosx metal -c
            ${SYPHON_DIR}/SyphonMetalShaders.metal
            -I ${SYPHON_DIR}
            -o ${SYPHON_METALLIB_AIR}
    DEPENDS ${SYPHON_DIR}/SyphonMetalShaders.metal
            ${SYPHON_DIR}/SyphonServerMetalTypes.h
    COMMENT "Compiling SyphonMetalShaders.metal -> .air"
  )
  add_custom_command(
    OUTPUT  ${SYPHON_METALLIB}
    COMMAND xcrun -sdk macosx metallib ${SYPHON_METALLIB_AIR}
            -o ${SYPHON_METALLIB}
    DEPENDS ${SYPHON_METALLIB_AIR}
    COMMENT "Linking default.metallib"
  )
  add_custom_target(syphon_metallib DEPENDS ${SYPHON_METALLIB})
  add_dependencies(${APP_NAME} syphon_metallib)
  add_custom_command(
    TARGET ${APP_NAME}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy ${SYPHON_METALLIB}
            ${MACOS_APP_NAME}.app/Contents/Resources/default.metallib
    COMMENT "Embedding Syphon default.metallib in app bundle"
  )
  # Re-sign the whole bundle after the plutil edits above, otherwise the
  # Info.plist is no longer bound to the signature (codesign -dv reports
  # "Info.plist=not bound"). macOS silently ignores the TCC usage-
  # description keys in an unbound Info.plist, so NSMicrophoneUsageDescription
  # is present but the mic prompt never fires — live audio then delivers
  # zero-filled buffers forever. A deep re-sign rebinds the plist.
  #
  # Uses SONIC_PI_CODESIGN_IDENTITY (detected above) — a Developer ID
  # if available so TCC permissions persist across rebuilds, otherwise
  # ad-hoc with the unfortunate consequence that grants reset each build.
  add_custom_command(
    TARGET ${APP_NAME}
    POST_BUILD
    COMMAND codesign --force --deep --sign "${SONIC_PI_CODESIGN_IDENTITY}" ${MACOS_APP_NAME}.app
    COMMENT "Re-signing bundle (${SONIC_PI_CODESIGN_IDENTITY}) so Info.plist edits are bound to the signature (required for TCC)"
  )
endif()

message(STATUS "App Root: ${APP_ROOT}")

set_property(TARGET ${APP_NAME} PROPERTY POSITION_INDEPENDENT_CODE ON) # Qt requires this?
target_include_directories(${APP_NAME}
    PRIVATE
    ${QTAPP_ROOT}
    ${APP_ROOT}/external
    ${QTAPP_ROOT}/osc
    ${QTAPP_ROOT}/model
    ${QTAPP_ROOT}/visualizer
    ${QTAPP_ROOT}/widgets
    ${QTAPP_ROOT}/Utils
    ${CMAKE_BINARY_DIR}
    include)

if(APPLE)
    # Syphon's own headers use two import styles:
    #   - "Foo.h"          — quote form, resolves against the source dir.
    #   - <Syphon/Foo.h>   — angle form, expects a Syphon/ directory on the
    #                        include path. In Xcode's framework build path
    #                        this is satisfied implicitly by the .framework
    #                        bundle structure; for our static-vendor build
    #                        we synthesise it: a build-tree directory
    #                        containing a Syphon -> ../../external/Syphon-Framework
    #                        symlink, which we then put on the include path.
    # The vendored source tree itself stays byte-identical to upstream.
    set(SYPHON_FRAMEWORK_INCLUDE_DIR ${CMAKE_CURRENT_BINARY_DIR}/syphon-framework-include)
    add_custom_command(
        OUTPUT ${SYPHON_FRAMEWORK_INCLUDE_DIR}/Syphon
        COMMAND ${CMAKE_COMMAND} -E make_directory ${SYPHON_FRAMEWORK_INCLUDE_DIR}
        COMMAND ${CMAKE_COMMAND} -E create_symlink ${APP_ROOT}/external/Syphon-Framework
                                                  ${SYPHON_FRAMEWORK_INCLUDE_DIR}/Syphon
        COMMENT "Synthesising <Syphon/...> include path for in-tree build"
    )
    add_custom_target(syphon_include_shim
        DEPENDS ${SYPHON_FRAMEWORK_INCLUDE_DIR}/Syphon)
    add_dependencies(${APP_NAME} syphon_include_shim)
    target_include_directories(${APP_NAME} PRIVATE
        ${APP_ROOT}/external/Syphon-Framework
        ${SYPHON_FRAMEWORK_INCLUDE_DIR})
endif()

if(WIN32)
    # windowsapp.lib: WinRT runtime for Windows.Graphics.Capture interop.
    # D3D11 / DXGI / IPC libs come in transitively via spout_vendored.
    # mf*: Media Foundation libs used by the session recorder
    # (recorder_win.cpp) to mux H.264 + AAC into .mp4 via IMFSinkWriter.
    # ole32/oleaut32: COM + BSTR for the UIA accessibility self-test
    # (windows_a11y.cpp).
    target_link_libraries(${APP_NAME} PRIVATE
        spout_vendored
        windowsapp
        mf
        mfplat
        mfreadwrite
        mfuuid
        ole32
        oleaut32)
endif()

target_link_libraries(${APP_NAME}
    PRIVATE
    SonicPi::API
    QScintilla
    Threads::Threads)


target_link_libraries(${APP_NAME}
    PRIVATE
    Qt::Core
    Qt::Gui
    Qt::Widgets
    Qt::OpenGL
    Qt::Concurrent
    Qt::Network
    Qt::OpenGLWidgets
    Qt::Svg)


# Compile options and OS specific libraries
if(WIN32)
    # Workaround Qt + MSVC 19 compile issue in release build.
    target_compile_options(${APP_NAME} PRIVATE $<$<CXX_COMPILER_ID:MSVC>:/bigobj /wd4005 /W3 /D_CRT_SECURE_NO_WARNINGS /D_WINSOCK_DEPRECATED_NO_WARNINGS /DBOOST_DATE_TIME_NO_LIB>)
elseif(${CMAKE_SYSTEM_NAME} MATCHES Linux)
    # Link librt
    target_link_libraries(${APP_NAME} PRIVATE rt)
endif()

# Deploy Qt binaries to the output on windows, and copy the CRT to the release folder
if(WIN32)
    # Visual Studio
    set(CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_SKIP true)
    include(InstallRequiredSystemLibraries)
    file(COPY ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS} DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/Release)

    # Copy kissfft.dll from the API build directory to the GUI executable directory
    add_custom_command(TARGET ${APP_NAME} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${API_VENDORED_KISSFFT_DLL}
            $<TARGET_FILE_DIR:${APP_NAME}>)

    # Locate windeployqt: prefer the Qt we configured against (QT_INSTALL_PREFIX),
    # else fall back to PATH — same resolution as macdeployqt above.
    if(QT_INSTALL_PREFIX AND EXISTS "${QT_INSTALL_PREFIX}/bin/windeployqt.exe")
        set(WINDEPLOYQT_BIN "${QT_INSTALL_PREFIX}/bin/windeployqt")
    else()
        set(WINDEPLOYQT_BIN "windeployqt")
    endif()
    message(STATUS "windeployqt: ${WINDEPLOYQT_BIN}")
    add_custom_command(TARGET ${APP_NAME} POST_BUILD
        COMMAND ${WINDEPLOYQT_BIN} $<TARGET_FILE:${APP_NAME}>)
endif() # Win32

# Make convenient source groups in the IDE
source_group(SonicPi FILES ${SOURCES})
source_group(Osc FILES ${OSC_SOURCES})
source_group(Scintilla FILES ${EDITOR_SOURCES})
source_group(Qt FILES ${QT_SOURCES})
source_group(OldApi FILES ${OLD_API_SOURCES})
source_group(NewApi FILES ${NEW_API_SOURCES})

source_group(Qt\\AutoMoc FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}_automoc.cpp )
source_group(Qt\\AutoMoc REGULAR_EXPRESSION "(mocs_*|qrc_.*|QtAwesome.*)" )
