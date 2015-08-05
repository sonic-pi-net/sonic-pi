require 'concurrent'                               # => false

# logger = Logger.new(STDOUT)
# Concurrent.configuration.logger = logger.method(:add)

# First option is to use operation pool

class ActorDoingIO < Concurrent::Actor::RestartingContext
  def on_message(message)
    # do IO operation
  end

  def default_executor
    Concurrent.configuration.global_operation_pool
  end
end 

actor_doing_io = ActorDoingIO.spawn :actor_doing_io
    # => #<Concurrent::Actor::Reference:0x7fb6fc906068 /actor_doing_io (ActorDoingIO)>
actor_doing_io.executor == Concurrent.configuration.global_operation_pool
    # => true

# It can be also built into a pool so there is not too many IO operations

class IOWorker < Concurrent::Actor::Utils::AbstractWorker
  def work(io_job)
    # do IO work
    sleep 0.1
    puts "#{path} second:#{(Time.now.to_f*100).floor} message:#{io_job}"
  end

  def default_executor
    Concurrent.configuration.global_operation_pool
  end
end 

pool = Concurrent::Actor::Utils::Pool.spawn('pool', 2) do |balancer, index|
  IOWorker.spawn(name: "worker-#{index}", args: [balancer])
end
    # => #<Concurrent::Actor::Reference:0x7fb6fc964190 /pool (Concurrent::Actor::Utils::Pool)>

pool << 1 << 2 << 3 << 4 << 5 << 6
    # => #<Concurrent::Actor::Reference:0x7fb6fc964190 /pool (Concurrent::Actor::Utils::Pool)>

# prints two lines each second
# /pool/worker-0 second:1414677666 message:1
# /pool/worker-1 second:1414677666 message:2
# /pool/worker-0 second:1414677667 message:3
# /pool/worker-1 second:1414677667 message:4
# /pool/worker-0 second:1414677668 message:5
# /pool/worker-1 second:1414677668 message:6

sleep 1                                            # => 1
