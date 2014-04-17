#! /usr/bin/env ruby
#
# usage: install.rb [(--install-dir | -i) install_directory]
#
# This script installs midilib into the Ruby site-local library directory.
#
# Author:: Jim Menard (mailto:jim@jimmenard.com)
# Copyright:: Copyright (c) 2003-2013 by Jim Menard
# License:: Distributed under the same license as Ruby.
#

require 'getoptlong'
require 'ftools'
require 'find'

SOURCE_DIR = 'lib'
LIB_DIR = 'midilib'
IO_DIR = File.join(LIB_DIR, 'io')

def instdir
  g = GetoptLong.new(['--install-dir', '-i', GetoptLong::REQUIRED_ARGUMENT])
  g.each do |name, arg|
    if name == '--install-dir'
      return arg
    else
      $stderr.puts "usage: $0 [--install-dir dir]"
    end
  end

  begin
    require 'rbconfig'
    libdir = Config::CONFIG['sitedir'] + "/" + 
      Config::CONFIG['MAJOR'] + "." +
      Config::CONFIG['MINOR']
  rescue ScriptError
    $LOAD_PATH.each do |l|
      if l =~ /site_ruby/ && l =~ /\d$/ && l !~ /#{PLATFORM}/
        libdir = l
        break
      end
    end
    STDERR.puts "Can't find required file `rbconfig.rb'."
    STDERR.puts "The `midilib' files need to be installed manually in #{libdir}"
  end
  return libdir
end

INSTALL_DIR = instdir()
files = Dir.chdir('lib') { Dir['**/*.rb'] }

files.each do |f|
  dir = File.dirname(f)
  target_dir = File.join(INSTALL_DIR, dir)
  File.makedirs(target_dir) unless File.exist?(target_dir)
  File.install(File.join('lib', f), File.join(INSTALL_DIR, f), 0644, true)
end
