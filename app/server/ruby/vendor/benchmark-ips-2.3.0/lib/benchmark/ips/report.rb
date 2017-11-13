# encoding: utf-8

module Benchmark
  module IPS

    # Report contains benchamrking entries.
    # Perform operations like add new entry, run comparison between entries.
    class Report

      # Represents benchmarking code data for Report.
      class Entry
        # Instantiate the Benchmark::IPS::Report::Entry.
        # @param [#to_s] label Label of entry.
        # @param [Integer] us Measured time in microsecond.
        # @param [Integer] iters Iterations.
        # @param [Float] ips Iterations per second.
        # @param [Float] ips_sd Standard deviation of iterations per second.
        # @param [Integer] cycles Number of Cycles.
        def initialize(label, us, iters, ips, ips_sd, cycles)
          @label = label
          @microseconds = us
          @iterations = iters
          @ips = ips
          @ips_sd = ips_sd
          @measurement_cycle = cycles
          @show_total_time = false
        end

        # Label of entry.
        # @return [String] the label of entry.
        attr_reader :label

        # Measured time in microsecond.
        # @return [Integer] number of microseconds.
        attr_reader :microseconds

        # Number of Iterations.
        # @return [Integer] number of iterations.
        attr_reader :iterations

        # Iterations per second.
        # @return [Float] number of iterations per second.
        attr_reader :ips

        # Standard deviation of iteration per second.
        # @return [Float] standard deviation of iteration per second.
        attr_reader :ips_sd

        # Number of Cycles.
        # @return [Integer] number of cycles.
        attr_reader :measurement_cycle

        # Control if the total time the job took is reported.
        # Typically this value is not significant because it's very
        # close to the expected time, so it's supressed by default.
        def show_total_time!
          @show_total_time = true
        end

        # Return entry's microseconds in seconds.
        # @return [Float] +@microseconds+ in seconds.
        def seconds
          @microseconds.to_f / 1_000_000.0
        end

        # Return entry's standard deviation of iteration per second in percentage.
        # @return [Float] +@ips_sd+ in percentage.
        def stddev_percentage
          100.0 * (@ips_sd.to_f / @ips.to_f)
        end

        alias_method :runtime, :seconds

        # Return Entry body text with left padding.
        # Body text contains information of iteration per second with
        # percentage of standard deviation, iterations in runtime.
        # @return [String] Left justified body.
        def body
          case Benchmark::IPS.options[:format]
          when :human
            left = "%s (±%4.1f%%) i/s" % [Helpers.scale(ips), stddev_percentage]
            iters = Helpers.scale(@iterations)

            if @show_total_time
              left.ljust(20) + (" - %s in %10.6fs" % [iters, runtime])
            else
              left.ljust(20) + (" - %s" % iters)
            end
          else
            left = "%10.1f (±%.1f%%) i/s" % [ips, stddev_percentage]

            if @show_total_time
              left.ljust(20) + (" - %10d in %10.6fs" % [@iterations, runtime])
            else
              left.ljust(20) + (" - %10d" % @iterations)
            end
          end
        end

        # Return header with padding if +@label+ is < length of 20.
        # @return [String] Right justified header (+@label+).
        def header
          @label.to_s.rjust(20)
        end

        # Return string repesentation of Entry object.
        # @return [String] Header and body.
        def to_s
          "#{header} #{body}"
        end

        # Print entry to current standard output ($stdout).
        def display
          $stdout.puts to_s
        end
      end # End of Entry

      # class Report

      # Entry to represent each benchamarked code in Report.
      # @return [Array<Entry>] Entries in Report.
      attr_reader :entries

      # Instantiate the Report.
      def initialize
        @entries = []
        @data = nil
      end

      # Add entry to report.
      # @param label [String] Entry label.
      # @param microseconds [Integer] Measured time in microsecond.
      # @param iters [Integer] Iterations.
      # @param ips [Float] Average Iterations per second.
      # @param ips_sd [Float] Standard deviation of iterations per second.
      # @param measurement_cycle [Integer] Number of cycles.
      # @return [Entry] Last added entry.
      def add_entry label, microseconds, iters, ips, ips_sd, measurement_cycle
        @entries << Entry.new(label, microseconds, iters, ips, ips_sd, measurement_cycle)
        @entries.last
      end

      # Entries data in array for generate json.
      # Each entry is a hash, consists of:
      #   name:   Entry#label
      #   ips:    Entry#ips
      #   stddev: Entry#ips_sd
      # @return [Array] Array of entries
      def data
        @data ||= @entries.collect do |entry|
          {
            :name => entry.label,
            :ips =>  entry.ips,
            :stddev => entry.ips_sd
          }
        end
      end

      # Run comparison of entries.
      def run_comparison
        Benchmark.compare(*@entries)
      end

      # Generate json from Report#data to given path.
      # @param path [String] path to generate json.
      def generate_json(path)
        File.open path, "w" do |f|
          require "json"
          f.write JSON.pretty_generate(data)
        end
      end
    end
  end
end
