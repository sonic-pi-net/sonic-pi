# encoding: utf-8

module RubyProf
  class AbstractPrinter
    # Create a new printer.
    #
    # result should be the output generated from a profiling run
    def initialize(result)
      @result = result
      @output = nil
    end

    # Specify print options.
    #
    # options - Hash table
    #   :min_percent - Number 0 to 100 that specifes the minimum
    #                  %self (the methods self time divided by the
    #                  overall total time) that a method must take
    #                  for it to be printed out in the report.
    #                  Default value is 0.
    #
    #   :print_file  - True or false. Specifies if a method's source
    #                  file should be printed.  Default value if false.
    #
    #   :sort_method - Specifies method used for sorting method infos.
    #                  Available values are :total_time, :self_time,
    #                  :wait_time, :children_time
    #                  Default value is :total_time
    def setup_options(options = {})
      @options = options
    end

    def min_percent
      @options[:min_percent] || 0
    end

    def print_file
      @options[:print_file] || false
    end

    def sort_method
      @options[:sort_method] || :total_time
    end

    def method_name(method)
      name = method.full_name
      if print_file
        name += " (#{method.source_file}:#{method.line}}"
      end
      name
    end

    # Print a profiling report to the provided output.
    #
    # output - Any IO object, including STDOUT or a file.
    # The default value is STDOUT.
    #
    # options - Hash of print options.  See #setup_options
    # for more information.  Note that each printer can
    # define its own set of options.
    def print(output = STDOUT, options = {})
      @output = output
      setup_options(options)
      print_threads
    end

    def print_threads
      @result.threads.each do |thread|
        print_thread(thread)
      end
    end

    def print_thread(thread)
      print_header(thread)
      print_methods(thread)
      print_footer(thread)
    end

    def print_header(thread)
    end

    def print_footer(thread)
    end
  end
end