
lib = File.expand_path '../../../lib/concurrent-ruby/'
$LOAD_PATH.push lib unless $LOAD_PATH.include? lib

require 'concurrent'

executors = [Concurrent::CachedThreadPool.new, Concurrent::SingleThreadExecutor.new, Concurrent::FixedThreadPool.new(1)]
executors.each do |executor|
  executor.post do
    sleep # sleep indefinitely
  end
end

# the process main thread should quit out which should kill the daemon CachedThreadPool
