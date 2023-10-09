require 'concurrent'

# logger = Logger.new(STDOUT)
# Concurrent.configuration.logger = logger.method(:add)

# First option is to use operation pool

class ActorDoingIO < Concurrent::Actor::RestartingContext
  def on_message(message)
    # do IO operation
  end

  def default_executor
    Concurrent.global_io_executor
  end
end #

actor_doing_io = ActorDoingIO.spawn :actor_doing_io
actor_doing_io.executor == Concurrent.global_io_executor

# It can be also built into a pool so there is not too many IO operations

class IOWorker < Concurrent::Actor::Context
  def on_message(io_job)
    # do IO work
    sleep 0.1
    puts "#{path} second:#{(Time.now.to_f*100).floor} message:#{io_job}"
  end

  def default_executor
    Concurrent.global_io_executor
  end
end #

pool = Concurrent::Actor::Utils::Pool.spawn('pool', 2) do |index|
  IOWorker.spawn(name: "worker-#{index}")
end

pool << 1 << 2 << 3 << 4 << 5 << 6

# prints two lines each second
# /pool/worker-0 second:1414677666 message:1
# /pool/worker-1 second:1414677666 message:2
# /pool/worker-0 second:1414677667 message:3
# /pool/worker-1 second:1414677667 message:4
# /pool/worker-0 second:1414677668 message:5
# /pool/worker-1 second:1414677668 message:6

sleep 1
