require 'mkmf'

RbConfig::MAKEFILE_CONFIG['CC'] = ENV['CC'] if ENV['CC']

$CFLAGS << " #{ENV["CFLAGS"]}"
$CFLAGS << " -g"
$CFLAGS << " -O3" unless $CFLAGS[/-O\d/]
$CFLAGS << " -Wall -Wno-comment"

def sys(cmd)
  puts " -- #{cmd}"
  unless ret = xsystem(cmd)
    raise "ERROR: '#{cmd}' failed"
  end
  ret
end

if !find_executable('cmake')
  abort "ERROR: CMake is required to build Rugged."
end

if !(MAKE = find_executable('gmake') || find_executable('make'))
  abort "ERROR: GNU make is required to build Rugged."
end

if !find_executable('pkg-config')
  abort "ERROR: pkg-config is required to build Rugged."
end

CWD = File.expand_path(File.dirname(__FILE__))
LIBGIT2_DIR = File.join(CWD, '..', '..', 'vendor', 'libgit2')

Dir.chdir(LIBGIT2_DIR) do
  Dir.mkdir("build") if !Dir.exists?("build")

  Dir.chdir("build") do
    sys("cmake .. -DBUILD_CLAR=OFF -DTHREADSAFE=ON -DBUILD_SHARED_LIBS=OFF -DCMAKE_C_FLAGS=-fPIC -DCMAKE_BUILD_TYPE=RelWithDebInfo")
    sys(MAKE)

    pcfile = File.join(LIBGIT2_DIR, "build", "libgit2.pc")
    $LDFLAGS << " " + `pkg-config --libs --static #{pcfile}`.strip
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

unless have_library 'git2' and have_header 'git2.h'
  abort "ERROR: Failed to build libgit2"
end

create_makefile("rugged/rugged")
