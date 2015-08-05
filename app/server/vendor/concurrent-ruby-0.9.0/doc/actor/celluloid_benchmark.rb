require 'benchmark'
require 'concurrent/actor'

require 'celluloid'
require 'celluloid/autostart'

# require 'stackprof'
# require 'profiler'

logger                          = Logger.new($stderr)
logger.level                    = Logger::INFO
Concurrent.configuration.logger = lambda do |level, progname, message = nil, &block|
  logger.add level, message, progname, &block
end

scale       = 1
ADD_TO      = (100 * scale).to_i
counts_size = (500 * scale).to_i
adders_size = (500 * scale).to_i

class Counter
  include Celluloid

  def initialize(adders, i)
    @adders = adders
    @i      = i
  end

  def counting(count, ivar)
    if count < ADD_TO
      @adders[(@i+1) % @adders.size].counting count+1, ivar
    else
      ivar.set count
    end
  end
end if defined? Celluloid

threads = []

# Profiler__.start_profile
# StackProf.run(mode:     :cpu,
#               interval: 10,
#               out:      File.join(File.dirname(__FILE__), 'stackprof-cpu-myapp.dump')) do
Benchmark.bmbm(10) do |b|
  [2, adders_size, adders_size*2, adders_size*3].each do |adders_size|

    b.report(format('%5d %4d %s', ADD_TO*counts_size, adders_size, 'concurrent')) do
      counts = Array.new(counts_size) { [0, Concurrent::IVar.new] }
      adders = Array.new(adders_size) do |i|
        Concurrent::Actor::Utils::AdHoc.spawn("adder#{i}") do
          lambda do |(count, ivar)|
            if count < ADD_TO
              adders[(i+1) % adders_size].tell [count+1, ivar]
            else
              ivar.set count
            end
          end
        end
      end

      counts.each_with_index do |count, i|
        adders[i % adders_size].tell count
      end

      counts.each do |count, ivar|
        raise unless ivar.value >= ADD_TO
      end

      threads << Thread.list.size

      adders.each { |a| a << :terminate! }
    end

    if defined? Celluloid
      b.report(format('%5d %4d %s', ADD_TO*counts_size, adders_size, 'celluloid')) do
        counts = []
        counts_size.times { counts << [0, Concurrent::IVar.new] }

        adders = []
        adders_size.times do |i|
          adders << Counter.new(adders, i)
        end

        counts.each_with_index do |count, i|
          adders[i % adders_size].counting *count
        end

        counts.each do |count, ivar|
          raise unless ivar.value >= ADD_TO
        end

        threads << Thread.list.size

        adders.each(&:terminate)
      end
    end
  end
end
# end
# Profiler__.stop_profile

p threads

# Profiler__.print_profile $stdout

