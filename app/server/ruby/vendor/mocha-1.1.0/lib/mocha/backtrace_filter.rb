module Mocha

  class BacktraceFilter

    LIB_DIRECTORY = File.expand_path(File.join(File.dirname(__FILE__), "..")) + File::SEPARATOR

    def initialize(lib_directory = LIB_DIRECTORY)
      @path_pattern = Regexp.new(lib_directory)
    end

    def filtered(backtrace)
      backtrace.reject { |location| @path_pattern.match(File.expand_path(location)) }
    end

  end

end
