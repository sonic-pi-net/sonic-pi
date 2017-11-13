# Copyright (C) the Rugged contributors.  All rights reserved.
#
# This file is part of Rugged, distributed under the MIT license.
# For full terms see the included LICENSE file.

require 'mkmf'

RbConfig::MAKEFILE_CONFIG['CC'] = ENV['CC'] if ENV['CC']

$CFLAGS << " #{ENV["CFLAGS"]}"
$CFLAGS << " -g"
$CFLAGS << " -O3" unless $CFLAGS[/-O\d/]
$CFLAGS << " -Wall -Wno-comment"

cmake_flags = [ ENV["CMAKE_FLAGS"] ]
cmake_flags << "-DUSE_SHA1DC=ON" if with_config("sha1dc")

def sys(cmd)
  puts " -- #{cmd}"
  unless ret = xsystem(cmd)
    raise "ERROR: '#{cmd}' failed"
  end
  ret
end

MAKE = if Gem.win_platform?
  # On Windows, Ruby-DevKit only has 'make'.
  find_executable('make')
else
  find_executable('gmake') || find_executable('make')
end

if !MAKE
  abort "ERROR: GNU make is required to build Rugged."
end

CWD = File.expand_path(File.dirname(__FILE__))
LIBGIT2_DIR = File.join(CWD, '..', '..', 'vendor', 'libgit2')

if arg_config("--use-system-libraries", !!ENV['RUGGED_USE_SYSTEM_LIBRARIES'])
  puts "Building Rugged using system libraries.\n"

  dir_config('git2').any? or pkg_config('libgit2')

  major = minor = nil

  File.readlines(File.join(LIBGIT2_DIR, "include", "git2", "version.h")).each do |line|
    if !major && (matches = line.match(/^#define LIBGIT2_VER_MAJOR ([0-9]+)$/))
      major = matches[1]
      next
    end

    if !minor && (matches = line.match(/^#define LIBGIT2_VER_MINOR ([0-9]+)$/))
      minor = matches[1]
      next
    end

    break if major && minor
  end

  try_compile(<<-SRC) or abort "libgit2 version is not compatible, expected ~> #{major}.#{minor}.0"
#include <git2/version.h>

#if LIBGIT2_VER_MAJOR != #{major} || LIBGIT2_VER_MINOR != #{minor}
#error libgit2 version is not compatible
#endif
  SRC
else
  if !find_executable('cmake')
    abort "ERROR: CMake is required to build Rugged."
  end

  if !Gem.win_platform? && !find_executable('pkg-config')
    abort "ERROR: pkg-config is required to build Rugged."
  end

  Dir.chdir(LIBGIT2_DIR) do
    Dir.mkdir("build") if !Dir.exists?("build")

    Dir.chdir("build") do
      # On Windows, Ruby-DevKit is MSYS-based, so ensure to use MSYS Makefiles.
      generator = "-G \"MSYS Makefiles\"" if Gem.win_platform?
      sys("cmake .. -DBUILD_CLAR=OFF -DTHREADSAFE=ON -DBUILD_SHARED_LIBS=OFF -DCMAKE_C_FLAGS=-fPIC -DCMAKE_BUILD_TYPE=RelWithDebInfo #{cmake_flags.join(' ')} #{generator}")
      sys(MAKE)

      # "normal" libraries (and libgit2 builds) get all these when they build but we're doing it
      # statically so we put the libraries in by hand. It's important that we put the libraries themselves
      # in $LIBS or the final linking stage won't pick them up
      if Gem.win_platform?
        $LDFLAGS << " " + "-L#{Dir.pwd}/deps/winhttp"
        $LIBS << " -lwinhttp -lcrypt32 -lrpcrt4 -lole32 -lz"
      else
        pcfile = File.join(LIBGIT2_DIR, "build", "libgit2.pc")
        $LDFLAGS << " " + `pkg-config --libs --static #{pcfile}`.strip
      end
    end
  end

  # Prepend the vendored libgit2 build dir to the $DEFLIBPATH.
  #
  # By default, $DEFLIBPATH includes $(libpath), which usually points
  # to something like /usr/lib for system ruby versions (not those
  # installed through rbenv or rvm).
  #
  # This was causing system-wide libgit2 installations to be preferred
  # over of our vendored libgit2 version when building rugged.
  #
  # By putting the path to the vendored libgit2 library at the front of
  # $DEFLIBPATH, we can ensure that our bundled version is always used.
  $DEFLIBPATH.unshift("#{LIBGIT2_DIR}/build")
  dir_config('git2', "#{LIBGIT2_DIR}/include", "#{LIBGIT2_DIR}/build")
end

unless have_library 'git2' and have_header 'git2.h'
  abort "ERROR: Failed to build libgit2"
end

create_makefile("rugged/rugged")
