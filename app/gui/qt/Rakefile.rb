# Sonic Pi Qt GUI Rakefile
# Builds the C++ Qt GUI for the current platform

# encoding: utf-8
require 'rake'

require 'fileutils'
require_relative "../../../build_scripts/utils.rb"
#require "../../../build_scripts/runtime_dependencies"
#require "../../../build_scripts/Dependencies"


namespace "qt_gui" do
  desc "Build Sonic Pi Qt docs"
  task :build_qt_docs do
    info("Building Sonic Pi QT docs...")
    FileUtils.cp(File.join(SPI_QT_GUI_PATH, "utils", "ruby_help.tmpl"), File.join(SPI_QT_GUI_PATH, "utils", "ruby_help.h"))
    ruby "#{SPI_SERVER_PATH}/ruby/build_scripts/qt-doc.rb" + ' -o ' + File.join(SPI_QT_GUI_PATH, "utils", 'ruby_help.h')
  end

  desc "Build Sonic Pi Qt GUI (using the generated makefile)"
  task :build, [:make_jobs] => ["#{SPI_QT_GUI_PATH}/Makefile"] do |t, args|
  	args.with_defaults(:make_jobs => 1)
    case OS
    when :linux, :raspberry, :macos
      # UNTESTED ON MACOS!
      exec_sh_commands([
        %Q(cd #{SPI_QT_GUI_PATH}),
        %Q(lrelease SonicPi.pro),
        %Q(make -j#{args.make_jobs})
      ])
    when :windows
      # UNTESTED!
      exec_win_commands([
        %Q(cd #{SPI_QT_GUI_PATH}),
        %Q(lrelease.exe SonicPi.pro),
        %Q(nmake),
        %Q(cd release),
        %Q(windeployqt sonic-pi.exe -printsupport)
      ])
    end
  end

  desc "Configure the Sonic Pi Qt GUI, and generate the makefile"
  task :configure, [:debug] do
    args.with_defaults(:debug => false)
    Rake::FileTask["#{SPI_QT_GUI_PATH}/Makefile"].invoke(args.debug)
  end

  file "#{SPI_QT_GUI_PATH}/Makefile", [:debug] => ["#{SPI_QT_GUI_PATH}/SonicPi.pro"] do |t, args|
    #qmake_args = "-qt=qt5"
    if (args.debug)
      qmake_args += " -Ddebug"
    end

    case OS
    when :linux, :raspberry, :macos
      exec_sh_commands([
        %Q(cd #{SPI_QT_GUI_PATH}),
        %Q(qmake SonicPi.pro #{qmake_args})
      ])
    when :windows
      exec_win_commands([
        %Q(cd #{SPI_QT_GUI_PATH}),
        %Q(qmake.exe SonicPi.pro #{qmake_args})
      ])
    end
  end
  #
  # desc "Build Sonic Pi QT GUI"
  # task :build_gui, [:make_jobs, :sonic_pi_root] do |t, args|
  # 	args.with_defaults(:make_jobs => 1)
  #   args.with_defaults(:sonic_pi_root => File.join(File.expand_path(Dir.pwd), "build", "sonic-pi"))
  #
  #   OS = ask_if_raspbian if (OS == :linux_arm)
  #
  #   case OS
  #   when :raspberry
  #   when :linux
  #     install_packages(Dependencies::Linux.gui, SPI_BUILD_CONFIG.pkg_manager) if (all_dependencies_installed == false)
  #     info("Building QT GUI...")
  #     exec_sh_commands([
  #       %Q(cd #{args.sonic_pi_root}/app/gui/qt),
  #       %Q(lrelease SonicPi.pro),
  #       %Q(qmake -qt=qt5 SonicPi.pro),
  #       %Q(make -j#{args.make_jobs})
  #     ])
  #
  #   when :windows
  #   	exec_win_commands([
  # 			%Q(cd #{args.sonic_pi_root}\\app\\gui\\qt),
  # 			%Q(c:\\Qt\\5.5\\msvc2013\\bin\\lrelease.exe SonicPi.pro),
  # 			%Q(c:\\Qt\\5.5\\msvc2013\\bin\\qmake.exe SonicPi.pro),
  # 			%Q(nmake),
  #
  # 			%Q(cd release),
  # 			%Q(c:\\Qt\\5.5\\msvc2013\\bin\\windeployqt sonic-pi.exe -printsupport),
  # 			# Dynamic libraries to link to at runtime
  # 			%Q(copy c:\\qwt-6.1.3\\lib\\qwt.dll .\\),
  # 			%Q(copy c:\\QScintilla_gpl-2.9.3\\Qt4Qt5\\release\\qscintilla2.dll .\\),
  # 			%Q(copy c:\\Qt\\5.5\\msvc2013\\bin\\Qt5OpenGL.dll .\\)
  # 		])
  #   when :macos
  #   end
  # end
  #
  # desc "Clean build folder"
  # task :clean_build_folder do
  #   info("Deleting Qt GUI source code from build folder...")
  #   make_clean(File.expand_path("."))
  #   FileUtils.rm_rf(Dir[File.join('.','**','*.cpp')])
  #   FileUtils.rm_rf(Dir[File.join('.','**','*.h')])
  #   FileUtils.rm_rf(Dir[File.join(".","**","*.hpp")])
  # end



end
#
# LIBS = []
# CC = "cc"
# CXXFLAGS = ""
#
# SOURCES = [
#   "main.cpp",
#   "mainwindow.cpp",
#   "sonicpilexer.cpp",
#   "sonicpiapis.cpp",
#   "sonicpiscintilla.cpp",
#   "oschandler.cpp",
#   "oscsender.cpp",
#   "sonicpilog.cpp",
#   "sonic_pi_osc_server.cpp",
#   "sonic_pi_udp_osc_server.cpp",
#   "sonic_pi_tcp_osc_server.cpp",
#   "sonicpitheme.cpp",
#   "scope.cpp",
#   "infowidget.cpp"
# ]
#
# HEADERS = [
#   "mainwindow.h",
#   "oscpkt.hh",
#   "udp.hh",
#   "sonicpilexer.h",
#   "sonicpilog.h",
#   "sonicpiapis.h",
#   "sonicpiscintilla.h",
#   "oschandler.h",
#   "oscsender.h",
#   "sonic_pi_osc_server.h",
#   "sonic_pi_udp_osc_server.h",
#   "sonic_pi_tcp_osc_server.h",
#   "ruby_help.h",
#   "sonicpitheme.h",
#   "scope.h",
#   "infowidget.h"
# ]
#
# TRANSLATIONS = [
#   "lang/sonic-pi_bs.ts",
#   "lang/sonic-pi_ca.ts",
#   "lang/sonic-pi_cs.ts",
#   "lang/sonic-pi_da.ts",
#   "lang/sonic-pi_de.ts",
#   "lang/sonic-pi_el.ts",
#   "lang/sonic-pi_en_US.ts",
#   "lang/sonic-pi_es.ts",
#   "lang/sonic-pi_et.ts",
#   "lang/sonic-pi_fi.ts",
#   "lang/sonic-pi_fr.ts",
#   "lang/sonic-pi_hi.ts",
#   "lang/sonic-pi_hu.ts",
#   "lang/sonic-pi_id.ts",
#   "lang/sonic-pi_is.ts",
#   "lang/sonic-pi_it.ts",
#   "lang/sonic-pi_ja.ts",
#   "lang/sonic-pi_ko.ts",
#   "lang/sonic-pi_nb.ts",
#   "lang/sonic-pi_nl.ts",
#   "lang/sonic-pi_pl.ts",
#   "lang/sonic-pi_pt.ts",
#   "lang/sonic-pi_pt_BR.ts",
#   "lang/sonic-pi_ro.ts",
#   "lang/sonic-pi_ru.ts",
#   "lang/sonic-pi_sv.ts",
#   "lang/sonic-pi_tr.ts",
#   "lang/sonic-pi_uk.ts",
#   "lang/sonic-pi_zh-Hans.ts",
#   "lang/sonic-pi_zh.ts",
#   "lang/sonic-pi_zh_HK.ts",
#   "lang/sonic-pi_zh_TW.ts"
# ]
#
# OTHER_FILES = [
#   "images/copy.png",
#   "images/cut.png",
#   "images/new.png",
#   "images/save.png",
#   "images/rec.png",
#   "images/recording_a.png",
#   "images/recording_b.png"
# ]
#
# RESOURCES = [
#   "SonicPi.qrc",
#   "help_files.qrc",
#   "info_files.qrc"
# ]
#
# RC_FILE = "SonicPi.rc"
# ICON = "images/app.icns"
#
# #all_dependencies_installed = false
# # task build: %w[install_all_dependency_packages supercollider build_aubio build_osmid build_erlang_files compile_extensions build_documentation build_qt_docs build_gui]
# #task :default => ["build"]
