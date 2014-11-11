#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)
require 'stringio'
require 'fileutils'

# --  Tests ----
class PrintersTest < Test::Unit::TestCase
  def setup
    # WALL_TIME so we can use sleep in our test and get same measurements on linux and windows
    RubyProf::measure_mode = RubyProf::WALL_TIME
    @result = RubyProf.profile do
      run_primes(200)
    end
  end

  def test_printers
    assert_nothing_raised do
      output = ENV['SHOW_RUBY_PROF_PRINTER_OUTPUT'] == "1" ? STDOUT : StringIO.new('')

      printer = RubyProf::CallInfoPrinter.new(@result)
      printer.print(output)

      printer = RubyProf::CallTreePrinter.new(@result)
      printer.print(output)

      printer = RubyProf::FlatPrinter.new(@result)
      printer.print(output)

      printer = RubyProf::FlatPrinterWithLineNumbers.new(@result)
      printer.print(output)

      printer = RubyProf::GraphHtmlPrinter.new(@result)
      printer.print(output)

      printer = RubyProf::GraphPrinter.new(@result)
      printer.print(output)
    end
  end

  def test_print_to_files
    assert_nothing_raised do
      output_dir = 'examples2'

      if ENV['SAVE_NEW_PRINTER_EXAMPLES']
        output_dir = 'examples'
      end
      FileUtils.mkdir_p output_dir

      printer = RubyProf::DotPrinter.new(@result)
      File.open("#{output_dir}/graph.dot", "w") {|f| printer.print(f)}

      printer = RubyProf::CallStackPrinter.new(@result)
      File.open("#{output_dir}/stack.html", "w") {|f| printer.print(f, :application => "primes")}

      printer = RubyProf::MultiPrinter.new(@result)
      printer.print(:path => "#{output_dir}", :profile => "multi", :application => "primes")
      for file in ['empty.png', 'graph.dot', 'minus.png', 'multi.flat.txt', 'multi.graph.html', 'multi.grind.dat', 'multi.stack.html', 'plus.png', 'stack.html']
        existant_file = output_dir + '/' + file
        assert File.size(existant_file) > 0
      end
    end
  end

  def test_flat_string
    output = helper_test_flat_string(RubyProf::FlatPrinter)
    assert_no_match(/prime.rb/, output)
  end

  def helper_test_flat_string(klass)
    output = ''

    printer = klass.new(@result)
    printer.print(output)

    assert_match(/Thread ID: -?\d+/i, output)
    assert_match(/Fiber ID: -?\d+/i, output)
    assert_match(/Total: \d+\.\d+/i, output)
    assert_match(/Object#run_primes/i, output)
    output
  end

  def test_flat_string_with_numbers
    output = helper_test_flat_string RubyProf::FlatPrinterWithLineNumbers
    assert_match(/prime.rb/, output)
    assert_no_match(/ruby_runtime:0/, output)
    assert_match(/called from/, output)

    # should combine common parents
    # 1.9 inlines it's  Fixnum#- so we don't see as many
    assert_equal(2, output.scan(/Object#is_prime/).length)
    assert_no_match(/\.\/test\/prime.rb/, output) # don't use relative paths
  end

  def test_graph_html_string
    output = ''
    printer = RubyProf::GraphHtmlPrinter.new(@result)
    printer.print(output)

    assert_match(/DTD HTML 4\.01/i, output)
    assert_match( %r{<th>Total Time</th>}i, output)
    assert_match(/Object#run_primes/i, output)
  end

  def test_graph_string
    output = ''
    printer = RubyProf::GraphPrinter.new(@result)
    printer.print(output)

    assert_match(/Thread ID: -?\d+/i, output)
    assert_match(/Fiber ID: -?\d+/i, output)
    assert_match(/Total Time: \d+\.\d+/i, output)
    assert_match(/Object#run_primes/i, output)
  end

  def test_call_tree_string
    output = ''
    printer = RubyProf::CallTreePrinter.new(@result)
    printer.print(output)
    assert_match(/fn=Object#find_primes/i, output)
    assert_match(/events: wall_time/i, output)
    assert_no_match(/d\d\d\d\d\d/, output) # old bug looked [in error] like Object::run_primes(d5833116)
  end

  def do_nothing
    start = Time.now
    while(Time.now == start)
    end
  end

  def test_all_with_small_percentiles
    result = RubyProf.profile do
      sleep 2
      do_nothing
    end

    # RubyProf::CallTreePrinter doesn't "do" a min_percent
    # RubyProf::FlatPrinter only outputs if self time > percent...
    # RubyProf::FlatPrinterWithLineNumbers same
    for klass in [ RubyProf::GraphPrinter, RubyProf::GraphHtmlPrinter]
      printer = klass.new(result)
      out = ''
      printer.print(out, :min_percent => 0.00000001)
      assert_match(/do_nothing/, out)
    end

  end

  def test_flat_result_sorting_by_self_time_is_default
    printer = RubyProf::FlatPrinter.new(@result)

    printer.print(output = '')
    self_times = flat_output_nth_column_values(output, 3)

    assert_sorted self_times
  end

  def test_flat_result_sorting
    printer = RubyProf::FlatPrinter.new(@result)

    sort_method_with_column_number = {:total_time => 2, :self_time => 3, :wait_time => 4, :children_time => 5}

    sort_method_with_column_number.each_pair do |sort_method, n|
      printer.print(output = '', :sort_method => sort_method)
      times = flat_output_nth_column_values(output, n)
      assert_sorted times
    end
  end

  def test_flat_result_with_line_numbers_sorting_by_self_time_is_default
    printer = RubyProf::FlatPrinterWithLineNumbers.new(@result)

    printer.print(output = '')
    self_times = flat_output_nth_column_values(output, 3)

    assert_sorted self_times
  end

  def test_flat_with_line_numbers_result_sorting
    printer = RubyProf::FlatPrinterWithLineNumbers.new(@result)

    sort_method_with_column_number = {:total_time => 2, :self_time => 3, :wait_time => 4, :children_time => 5}

    sort_method_with_column_number.each_pair do |sort_method, n|
      printer.print(output = '', :sort_method => sort_method)
      times = flat_output_nth_column_values(output, n)
      assert_sorted times
    end
  end

  def test_graph_result_sorting_by_total_time_is_default
    printer = RubyProf::GraphPrinter.new(@result)
    printer.print(output = '')
    total_times = graph_output_nth_column_values(output, 3)

    assert_sorted total_times
  end

  def test_graph_results_sorting
    printer = RubyProf::GraphPrinter.new(@result)

    sort_method_with_column_number = {:total_time => 3, :self_time => 4, :wait_time => 5, :children_time => 6}

    sort_method_with_column_number.each_pair do |sort_method, n|
      printer.print(output = '', :sort_method => sort_method)
      times = graph_output_nth_column_values(output, n)
      assert_sorted times
    end
  end

  def test_graph_html_result_sorting_by_total_time_is_default
    printer = RubyProf::GraphHtmlPrinter.new(@result)
    printer.print(output = '')
    total_times = graph_html_output_nth_column_values(output, 3)

    assert_sorted total_times
  end

  def test_graph_html_result_sorting
    printer = RubyProf::GraphHtmlPrinter.new(@result)

    sort_method_with_column_number = {:total_time => 3, :self_time => 4, :wait_time => 5, :children_time => 6}

    sort_method_with_column_number.each_pair do |sort_method, n|
      printer.print(output = '', :sort_method => sort_method)
      times = graph_html_output_nth_column_values(output, n)
      assert_sorted times
    end
  end

  private
  def flat_output_nth_column_values(output, n)
    only_method_calls = output.split("\n").select { |line| line =~ /^ +\d+/ }
    only_method_calls.collect { |line| line.split(/ +/)[n] }
  end

  def graph_output_nth_column_values(output, n)
    only_root_calls = output.split("\n").select { |line| line =~ /^ +[\d\.]+%/ }
    only_root_calls.collect { |line| line.split(/ +/)[n] }
  end

  def graph_html_output_nth_column_values(output, n)
    only_root_calls = output.split('<tr class="method">')
    only_root_calls.delete_at(0)
    only_root_calls.collect {|line| line.scan(/[\d\.]+/)[n - 1] }
  end

  def assert_sorted array
    array = array.map{|n| n.to_f} # allow for > 10s times to sort right, since lexographically 4.0 > 10.0
    assert_equal array, array.sort.reverse, "Array #{array.inspect} is not sorted"
  end
end
