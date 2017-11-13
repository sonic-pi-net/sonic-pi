# encoding: utf-8
require 'benchmark/timing'
require 'benchmark/compare'
require 'benchmark/ips/report'
require 'benchmark/ips/job'

# Performance benchmarking library
module Benchmark
  # Benchmark in iterations per second, no more guessing!
  # @see https://github.com/evanphx/benchmark-ips
  module IPS

    # Benchmark-ips Gem version.
    VERSION = "2.3.0"

    # CODENAME of current version.
    CODENAME = "Monsoon BBQ"

    # Measure code in block, each code's benchmarked result will display in
    # iteration per second with standard deviation in given time.
    # @param time [Integer] Specify how long should benchmark your code in seconds.
    # @param warmup [Integer] Specify how long should Warmup time run in seconds.
    # @return [Report]
    def ips(*args)
      if args[0].is_a?(Hash)
        time, warmup, quiet = args[0].values_at(:time, :warmup, :quiet)
      else
        time, warmup, quiet = args
      end

      suite = nil

      sync, $stdout.sync = $stdout.sync, true

      if defined? Benchmark::Suite and Suite.current
        suite = Benchmark::Suite.current
      end

      quiet ||= (suite && suite.quiet?)

      job = Job.new({:suite => suite,
                     :quiet => quiet
      })

      job_opts = {}
      job_opts[:time] = time unless time.nil?
      job_opts[:warmup] = warmup unless warmup.nil?

      job.config job_opts

      yield job

      $stdout.puts "Calculating -------------------------------------" unless quiet

      job.run_warmup

      $stdout.puts "-------------------------------------------------" unless quiet

      job.run

      $stdout.sync = sync

      if job.compare?
        job.run_comparison
      end

      if job.json?
        job.generate_json
      end

      return job.full_report
    end

    # Set options for running the benchmarks.
    # :format => [:human, :raw]
    #    :human format narrows precision and scales results for readability
    #    :raw format displays 6 places of precision and exact iteration counts
    def self.options
      @options ||= {:format => :human}
    end

    module Helpers
      def scale(value)
        scale = (Math.log10(value) / 3).to_i
        suffix = case scale
        when 1; 'k'
        when 2; 'M'
        when 3; 'B'
        when 4; 'T'
        when 5; 'Q'
        else
          # < 1000 or > 10^15, no scale or suffix
          scale = 0
          ' '
        end
        "%10.3f#{suffix}" % (value.to_f / (1000 ** scale))
      end
      module_function :scale
    end
  end

  extend Benchmark::IPS # make ips available as module-level method
end
