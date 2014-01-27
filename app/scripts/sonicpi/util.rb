require 'cgi'

module SonicPi
  module Util
    def os
      case RUBY_PLATFORM
      when /.*linux.*/
        :linux
      when /.*darwin.*/
        :osx
      when /.*mswin.*/
        :windows
      else
        raise "Unsupported platform #{RUBY_PLATFORM}"
      end
    end

    def root_path
      File.absolute_path("#{File.dirname(__FILE__)}/../../../")
    end

    def etc_path
      File.absolute_path("#{root_path}/etc")
    end

    def log_path
      File.absolute_path("#{root_path}/log")
    end

    def tmp_path
      File.absolute_path("#{root_path}/tmp")
    end

    def synthdef_path
      File.absolute_path("#{etc_path}/synthdefs")
    end

    def samples_path
      File.absolute_path("#{etc_path}/samples")
    end

    def media_path
      File.absolute_path("#{root_path}/public/media")
    end

    def log(message)
      File.open("#{log_path}/sonicpi.log", 'a') {|f| f.write("#{Time.now.strftime("%Y-%m-%d %H:%M:%S")} #{message}\n")}
    end

    def debug_mode
      false
    end

  end
end
