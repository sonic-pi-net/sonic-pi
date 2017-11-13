module Benchmark
  # Functionality of performaing comparison between reports.
  #
  # Usage:
  #
  # Add +x.compare!+ to perform comparison between reports.
  #
  # Example:
  #   > Benchmark.ips do |x|
  #     x.report('Reduce using tag')     { [*1..10].reduce(:+) }
  #     x.report('Reduce using to_proc') { [*1..10].reduce(&:+) }
  #     x.compare!
  #   end
  #
  #   Calculating -------------------------------------
  #       Reduce using tag     19216 i/100ms
  #   Reduce using to_proc     17437 i/100ms
  #   -------------------------------------------------
  #       Reduce using tag   278950.0 (±8.5%) i/s -    1402768 in   5.065112s
  #   Reduce using to_proc   247295.4 (±8.0%) i/s -    1238027 in   5.037299s
  #
  #   Comparison:
  #       Reduce using tag:   278950.0 i/s
  #   Reduce using to_proc:   247295.4 i/s - 1.13x slower
  #
  # Besides regular Calculating report, this will also indicates which one is slower.
  module Compare

    # Compare between reports, prints out facts of each report:
    # runtime, comparative speed difference.
    # @param reports [Array<Report>] Reports to compare.
    def compare(*reports)
      return if reports.size < 2

      iter = false
      sorted = reports.sort do |a,b|
        if a.respond_to? :ips
          iter = true
          b.ips <=> a.ips
        else
          a.runtime <=> b.runtime
        end
      end

      best = sorted.shift

      $stdout.puts "\nComparison:"

      if iter
        $stdout.printf "%20s: %10.1f i/s\n", best.label, best.ips
      else
        $stdout.puts "#{best.rjust(20)}: #{best.runtime}s"
      end

      sorted.each do |report|
        name = report.label.to_s

        if iter
          x = (best.ips.to_f / report.ips.to_f)
          $stdout.printf "%20s: %10.1f i/s - %.2fx slower\n", name, report.ips, x
        else
          x = "%.2f" % (report.ips.to_f / best.ips.to_f)
          $stdout.puts "#{name.rjust(20)}: #{report.runtime}s - #{x}x slower"
        end
      end

      $stdout.puts
    end
  end

  extend Benchmark::Compare
end
