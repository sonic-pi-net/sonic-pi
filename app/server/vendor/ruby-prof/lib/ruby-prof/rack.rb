# encoding: utf-8
require 'tmpdir'

module Rack
  class RubyProf
    def initialize(app, options = {})
      @app = app
      @options = options
      @options[:min_percent] ||= 1

      @tmpdir = options[:path] || Dir.tmpdir
      FileUtils.mkdir_p(@tmpdir)

      @printer_klasses = @options[:printers]  || {::RubyProf::FlatPrinter => 'flat.txt',
                                                  ::RubyProf::GraphPrinter => 'graph.txt',
                                                  ::RubyProf::GraphHtmlPrinter => 'graph.html',
                                                  ::RubyProf::CallStackPrinter => 'call_stack.html'}

      @skip_paths = options[:skip_paths] || [%r{^/assets}, %r{\.css$}, %r{\.js$}, %r{\.png$}, %r{\.jpeg$}, %r{\.jpg$}, %r{\.gif$}]
    end

    def call(env)
      request = Rack::Request.new(env)

      if @skip_paths.any? {|skip_path| skip_path =~ request.path}
        @app.call(env)
      else
        result = nil
        data = ::RubyProf::Profile.profile do
          result = @app.call(env)
        end

        path = request.path.gsub('/', '-')
        path.slice!(0)

        print(data, path)
        result
      end
    end

    def print(data, path)
      @printer_klasses.each do |printer_klass, base_name|
        printer = printer_klass.new(data)
        file_name = ::File.join(@tmpdir, "#{path}-#{base_name}")
        ::File.open(file_name, 'wb') do |file|
          printer.print(file, @options)
        end
      end
    end
  end
end