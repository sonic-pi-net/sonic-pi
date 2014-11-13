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
    os = case RUBY_PLATFORM
         when /.*arm.*-linux.*/
           :raspberry
         when /.*linux.*/
           :linux
         when /.*darwin.*/
           :osx
         when /.*mingw.*/
           :windows
         else
           RUBY_PLATFORM
         end
    require_relative "../../../rb-native/#{os}/#{RUBY_VERSION}p#{RUBY_PATCHLEVEL}/ruby_prof"
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
    when "wall" || "wall_time"
      RubyProf.measure_mode = RubyProf::WALL_TIME
    when "cpu" || "cpu_time"
      if ENV.key?("RUBY_PROF_CPU_FREQUENCY")
        RubyProf.cpu_frequency = ENV["RUBY_PROF_CPU_FREQUENCY"].to_f
      else
        begin
          open("/proc/cpuinfo") do |f|
            f.each_line do |line|
              s = line.slice(/cpu MHz\s*:\s*(.*)/, 1)
              if s
                RubyProf.cpu_frequency = s.to_f * 1000000
                break
              end
            end
          end
        rescue Errno::ENOENT
        end
      end
      RubyProf.measure_mode = RubyProf::CPU_TIME
    when "allocations"
      RubyProf.measure_mode = RubyProf::ALLOCATIONS
    when "memory"
      RubyProf.measure_mode = RubyProf::MEMORY
    else
      # the default...
      RubyProf.measure_mode = RubyProf::PROCESS_TIME
    end
  end
end

RubyProf::figure_measure_mode
