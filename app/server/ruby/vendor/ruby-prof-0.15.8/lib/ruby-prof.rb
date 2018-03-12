# encoding: utf-8

# Load the C-based binding.
begin
  RUBY_VERSION =~ /(\d+.\d+)/
  require "#{$1}/ruby_prof"
rescue LoadError
  # Start modifications
  #
  # Original code:
  begin
    require "ruby_prof"
  rescue LoadError
    # Modifications made for Sonic Pi multi-platform compatibility:
    require 'rbconfig'
    ruby_api = RbConfig::CONFIG['ruby_version']
    require_relative "../../../rb-native/#{ruby_api}/ruby_prof"
  end
  # End modifications

end

require 'ruby-prof/version'
require 'ruby-prof/aggregate_call_info'
require 'ruby-prof/call_info'
require 'ruby-prof/call_info_visitor'
require 'ruby-prof/compatibility'
require 'ruby-prof/method_info'
require 'ruby-prof/profile'
require 'ruby-prof/rack'
require 'ruby-prof/thread'

require 'ruby-prof/printers/abstract_printer'
require 'ruby-prof/printers/call_info_printer'
require 'ruby-prof/printers/call_stack_printer'
require 'ruby-prof/printers/call_tree_printer'
require 'ruby-prof/printers/dot_printer'
require 'ruby-prof/printers/flat_printer'
require 'ruby-prof/printers/flat_printer_with_line_numbers'
require 'ruby-prof/printers/graph_html_printer'
require 'ruby-prof/printers/graph_printer'
require 'ruby-prof/printers/multi_printer'

module RubyProf
  # Checks if the user specified the clock mode via
  # the RUBY_PROF_MEASURE_MODE environment variable
  def self.figure_measure_mode
    case ENV["RUBY_PROF_MEASURE_MODE"]
    when "wall", "wall_time"
      RubyProf.measure_mode = RubyProf::WALL_TIME
    when "cpu", "cpu_time"
      RubyProf.measure_mode = RubyProf::CPU_TIME
    when "allocations"
      RubyProf.measure_mode = RubyProf::ALLOCATIONS
    when "memory"
      RubyProf.measure_mode = RubyProf::MEMORY
    when "process", "process_time"
      RubyProf.measure_mode = RubyProf::PROCESS_TIME
    when "gc_time"
      RubyProf.measure_mode = RubyProf::GC_TIME
    when "gc_runs"
      RubyProf.measure_mode = RubyProf::GC_RUNS
    else
      # the default is defined in the measure_mode reader
    end
  end
end

RubyProf::figure_measure_mode
