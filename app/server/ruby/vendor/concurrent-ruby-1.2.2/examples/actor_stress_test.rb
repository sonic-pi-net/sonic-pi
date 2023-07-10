#!/usr/bin/env ruby

#$: << File.expand_path('../../lib', __FILE__)

require 'benchmark'
require 'optparse'
require 'thread'
require 'rspec/expectations'

require 'concurrent/actor'

class ActorStressTester
  include ::RSpec::Matchers

  TESTS_PER_RUN = 5
  THREADS_PER_TEST = 10
  LOOPS_PER_THREAD = 25

  class Ping < Concurrent::Actor::Context
    def initialize(queue)
      @queue = queue
    end

    def on_message(message)
      case message
      when :child
        Concurrent::Actor::Utils::AdHoc.spawn(:pong, @queue) do |queue|
          -> m { queue << m }
        end
      else
        @queue << message
        message
      end
    end
  end

  def initialize(opts = {})
    @tests = opts.fetch(:tests, TESTS_PER_RUN)
    @threads = opts.fetch(:threads, THREADS_PER_TEST)
    @loops = opts.fetch(:loops, LOOPS_PER_THREAD)
  end

  def run
    plural = ->(number){ number == 1 ? '' : 's' }

    puts "Running #{@tests} test#{plural.call(@tests)} " +
      "with #{@threads} thread#{plural.call(@threads)} each " +
      "and #{@loops} loop#{plural.call(@loops)} per thread..."

    Benchmark.bmbm do |bm|
      @tests.times do
        bm.report do
          test(@threads, @loops)
        end
      end
    end
  end

  def test(threads, loops)
    (1..threads).collect do
      Thread.new do
        loops.times do

          queue = Queue.new
          actor = Ping.spawn(:ping, queue)

          core = Concurrent::Actor.root.send(:core)
          children = core.instance_variable_get(:@children)
          expect(children).to include(actor)

          actor << 'a' << 1
          expect(queue.pop).to eq 'a'
          expect(actor.ask(2).value).to eq 2

          expect(actor.parent).to eq Concurrent::Actor.root
          expect(Concurrent::Actor.root.path).to eq '/'
          expect(actor.path).to eq '/ping'

          child = actor.ask(:child).value
          expect(child.path).to eq '/ping/pong'

          queue.clear
          child.ask(3)
          expect(queue.pop).to eq 3

          actor << :terminate!
          #expect(actor.ask(:blow_up).wait).to be_rejected
          expect(actor.ask(:blow_up).wait).to be_failed
          terminate_actors(actor, child)
        end
      end
    end.each(&:join)
  end

  def terminate_actors(*actors)
    actors.each do |actor|
      unless actor.ask!(:terminated?)
        actor.ask!(:terminate!)
      end
    end
  end
end

# def trace!
#   set_trace_func proc { |event, file, line, id, binding, classname|
#     # thread = eval('Thread.current', binding).object_id.to_s(16)
#     printf "%8s %20s %20s %s %s:%-2d\n", event, id, classname, nil, file, line
#   }
#   yield
# ensure
#   set_trace_func nil
# end

if $0 == __FILE__

  options = {}

  OptionParser.new do |opts|
    opts.banner = "Usage: #{File.basename(__FILE__)} [options]"

    opts.on("--tests=TESTS", "Number of tests per run") do |value|
      options[:tests] = value.to_i
    end

    opts.on("--threads=THREADS", "Number of threads per test") do |value|
      options[:threads] = value.to_i
    end

    opts.on("--loops=LOOPS", "Number of loops per thread") do |value|
      options[:loops] = value.to_i
    end

    opts.on("-h", "--help", "Prints this help") do
      puts opts
      exit
    end
  end.parse!

  ActorStressTester.new(options).run
end
