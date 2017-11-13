require 'benchmark'
require 'find'
require 'optparse'

require 'parser'

module Parser

  class Runner
    def self.go(options)
      new.execute(options)
    end

    def initialize
      @option_parser = OptionParser.new { |opts| setup_option_parsing(opts) }
      @parser_class  = nil
      @parser        = nil
      @files         = []
      @fragments     = []
      @warnings      = false
      @benchmark     = false

      @source_count = 0
      @source_size  = 0
    end

    def execute(options)
      parse_options(options)
      prepare_parser

      process_all_input
    end

    private

    def runner_name
      raise NotImplementedError, "implement #{self.class}##{__callee__}"
    end

    def setup_option_parsing(opts)
      opts.banner = "Usage: #{runner_name} [options] FILE|DIRECTORY..."

      opts.on_tail '-h', '--help', 'Display this help message and exit' do
        puts opts.help
        puts <<-HELP

  If you specify a DIRECTORY, then all *.rb files are fetched
  from it recursively and appended to the file list.

  The default parsing mode is for current Ruby (#{RUBY_VERSION}).
        HELP
        exit
      end

      opts.on_tail '-V', '--version', 'Output version information and exit' do
        puts "#{runner_name} based on parser version #{Parser::VERSION}"
        exit
      end

      opts.on '--18', 'Parse as Ruby 1.8.7 would' do
        require 'parser/ruby18'
        @parser_class = Parser::Ruby18
      end

      opts.on '--19', 'Parse as Ruby 1.9.3 would' do
        require 'parser/ruby19'
        @parser_class = Parser::Ruby19
      end

      opts.on '--20', 'Parse as Ruby 2.0 would' do
        require 'parser/ruby20'
        @parser_class = Parser::Ruby20
      end

      opts.on '--21', 'Parse as Ruby 2.1 would' do
        require 'parser/ruby21'
        @parser_class = Parser::Ruby21
      end

      opts.on '--22', 'Parse as Ruby 2.2 would' do
        require 'parser/ruby22'
        @parser_class = Parser::Ruby22
      end

      opts.on '-w', '--warnings', 'Enable warnings' do |w|
        @warnings = w
      end

      opts.on '-B',  '--benchmark', 'Benchmark the processor' do |b|
        @benchmark = b
      end

      opts.on '-e fragment', 'Process a fragment of Ruby code' do |fragment|
        @fragments << fragment
      end
    end

    def parse_options(options)
      @option_parser.parse!(options)

      # Slop has just removed recognized options from `options`.
      options.each do |file_or_dir|
        if File.directory?(file_or_dir)
          Find.find(file_or_dir) do |path|
            @files << path if path.end_with? '.rb'
          end
        else
          @files << file_or_dir
        end
      end

      if @files.empty? && @fragments.empty?
        $stderr.puts 'Need something to parse!'
        exit 1
      end

      if @parser_class.nil?
        require 'parser/current'
        @parser_class = Parser::CurrentRuby
      end
    end

    def prepare_parser
      @parser = @parser_class.new

      @parser.diagnostics.all_errors_are_fatal = true
      @parser.diagnostics.ignore_warnings      = !@warnings

      @parser.diagnostics.consumer = lambda do |diagnostic|
        puts(diagnostic.render)
      end
    end

    def input_size
      @files.size + @fragments.size
    end

    def process_all_input
      parsing_time =
        Benchmark.measure do
          process_fragments
          process_files
        end

      if @benchmark
        report_with_time(parsing_time)
      end
    end

    def process_fragments
      @fragments.each_with_index do |fragment, index|
        if fragment.respond_to? :force_encoding
          fragment = fragment.dup.force_encoding(@parser.default_encoding)
        end

        buffer = Source::Buffer.new("(fragment:#{index})")
        buffer.source = fragment

        process_buffer(buffer)
      end
    end

    def process_files
      @files.each do |filename|
        source = File.read(filename)
        if source.respond_to? :force_encoding
          source.force_encoding(@parser.default_encoding)
        end

        buffer = Parser::Source::Buffer.new(filename)

        if @parser.class.name == 'Parser::Ruby18'
          buffer.raw_source = source
        else
          buffer.source     = source
        end

        process_buffer(buffer)
      end
    end

    def process_buffer(buffer)
      @parser.reset

      process(buffer)

      @source_count += 1
      @source_size  += buffer.source.size

    rescue Parser::SyntaxError
      # skip

    rescue StandardError
      $stderr.puts("Failed on: #{buffer.name}")
      raise
    end

    def process(buffer)
      raise NotImplementedError, "implement #{self.class}##{__callee__}"
    end

    def report_with_time(parsing_time)
      cpu_time = parsing_time.utime

      speed = '%.3f' % (@source_size / cpu_time / 1000)
      puts "Parsed #{@source_count} files (#{@source_size} characters)" \
           " in #{'%.2f' % cpu_time} seconds (#{speed} kchars/s)."

      if defined?(RUBY_ENGINE)
        engine = RUBY_ENGINE
      else
        engine = 'ruby'
      end

      puts "Running on #{engine} #{RUBY_VERSION}."
    end
  end

end
