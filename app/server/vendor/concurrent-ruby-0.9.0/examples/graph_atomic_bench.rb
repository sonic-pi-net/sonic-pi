#!/usr/bin/env ruby

$: << File.expand_path('../../lib', __FILE__)

require 'optparse'

conf = {
  :vary => "threads",
  :lock => "atomic"
}

OptionParser.new do |opts|
  opts.on("-l", "--lock atomic|mutex") do |l|
    conf[:lock] = l
  end
  opts.on("-v", "--vary threads|speed") do |v|
    conf[:vary] = v
  end
  opts.on("-h", "--help"){ puts opts; exit }
end.parse!(ARGV)

result = File.open("results_#{conf[:lock]}_#{conf[:vary]}.csv", "w")


if conf[:vary] == "threads"
  # Vary the number of concurrent threads that update the value.
  #
  # There is a total count of 1mio updates that is distributed
  # between the number of threads.
  #
  # A pair number of threads is used so that even add and odd substract 1.
  # This avoid creating instances for Bignum since the number should
  # stay in the Fixnum range.
  #
  (1..100).each do |i|
    i = i * 2

    ret = []
    10.times do
      ret << `ruby #{File.dirname(__FILE__)}/bench_atomic_1.rb -l #{conf[:lock]} -t #{i}`.to_f
    end

    line = ([i] + ret).join(', ')

    puts line
    result.puts line
  end
elsif conf[:vary] == "speed"
  # Varies the execution time of the update block
  # by using long calulation (MD5)
  #
  # NOTE: Thread.pass and sleep() are not usable by the atomic
  #       lock. It needs to run the whole block without hitting
  #       another atomic update otherwise it has to retry
  #
  # The expected result is that the atomic lock's performance
  # will hit a certain threshold where it will be worse than mutexes.
  #
  (1..30).each do |i|

    ret = []
    10.times do
      ret << `ruby #{File.dirname(__FILE__)}/bench_atomic_1.rb -l #{conf[:lock]} -s #{i}`.to_f
    end

    line = ([i] + ret).join(', ')

    puts line
    result.puts line
  end
end
