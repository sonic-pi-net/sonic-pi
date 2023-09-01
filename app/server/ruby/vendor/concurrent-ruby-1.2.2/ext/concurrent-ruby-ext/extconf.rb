require 'fileutils'
require 'mkmf'

unless RUBY_ENGINE == "ruby"
  File.write("Makefile", dummy_makefile($srcdir).join(""))
  exit
end

extension_name = 'concurrent_ruby_ext'

dir_config(extension_name)
have_header "libkern/OSAtomic.h"

compiler_is_gcc = (CONFIG["GCC"] && !CONFIG["GCC"].empty?) ||
    # This could stand to be more generic...  but I am afraid.
    CONFIG["CC"] =~ /\bgcc\b/

if compiler_is_gcc
  case CONFIG["arch"]
  when /mswin32|mingw|solaris/
    $CFLAGS += " -march=native"
  when 'i686-linux'
    $CFLAGS += " -march=i686"
  end
end

create_makefile File.join('concurrent', extension_name)
