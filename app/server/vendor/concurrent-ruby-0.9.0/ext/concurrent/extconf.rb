require 'fileutils'

$:.unshift(File.expand_path('../../../lib', __FILE__))
require 'concurrent/utility/native_extension_loader'

EXTENSION_NAME = 'extension'

def create_dummy_makefile
  File.open('Makefile', 'w') do |f|
    f.puts 'all:'
    f.puts 'install:'
  end
end

if Concurrent.on_jruby? || ! Concurrent.allow_c_extensions?
  create_dummy_makefile
  warn 'C optimizations are not supported on this version of Ruby.'
else
  begin

    require 'mkmf'
    dir_config(EXTENSION_NAME)

    have_header "libkern/OSAtomic.h"

    def compiler_is_gcc
      if CONFIG["GCC"] && CONFIG["GCC"] != ""
        return true
      elsif ( # This could stand to be more generic...  but I am afraid.
             CONFIG["CC"] =~ /\bgcc\b/
            )
        return true
      end
      return false
    end

    if compiler_is_gcc
      case CONFIG["arch"]
      when /mswin32|mingw|solaris/
        $CFLAGS += " -march=native"
      when 'i686-linux'
        $CFLAGS += " -march=i686"
      end
    end

    try_run(<<CODE,$CFLAGS) && ($defs << '-DHAVE_GCC_SYNC')
      int main() {
        __sync_synchronize();
        return 0;
      }
CODE

    create_makefile('concurrent/' + EXTENSION_NAME)
  rescue
    create_dummy_makefile
    warn 'C optimizations cannot be compiled on this version of Ruby.'
  end
end
