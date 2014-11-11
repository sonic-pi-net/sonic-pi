# encoding: utf-8

module RubyProf
  # Helper class to simplify printing profiles of several types from
  # one profiling run. Currently prints a flat profile, a callgrind
  # profile, a call stack profile and a graph profile.
  class MultiPrinter
    def initialize(result)
      @stack_printer = CallStackPrinter.new(result)
      @graph_printer = GraphHtmlPrinter.new(result)
      @tree_printer = CallTreePrinter.new(result)
      @flat_printer = FlatPrinter.new(result)
    end

    # create profile files under options[:path] or the current
    # directory. options[:profile] is used as the base name for the
    # pofile file, defaults to "profile".
    def print(options)
      @profile = options.delete(:profile) || "profile"
      @directory = options.delete(:path) || File.expand_path(".")
      File.open(stack_profile, "w") do |f|
        @stack_printer.print(f, options.merge(:graph => "#{@profile}.graph.html"))
      end
      File.open(graph_profile, "w") do |f|
        @graph_printer.print(f, options)
      end
      File.open(tree_profile, "w") do |f|
        @tree_printer.print(f, options)
      end
      File.open(flat_profile, "w") do |f|
        @flat_printer.print(f, options)
      end
    end

    # the name of the call stack profile file
    def stack_profile
      "#{@directory}/#{@profile}.stack.html"
    end

    # the name of the graph profile file
    def graph_profile
      "#{@directory}/#{@profile}.graph.html"
    end

    # the name of the callgrind profile file
    def tree_profile
      "#{@directory}/#{@profile}.grind.dat"
    end

    # the name of the flat profile file
    def flat_profile
      "#{@directory}/#{@profile}.flat.txt"
    end

  end
end
