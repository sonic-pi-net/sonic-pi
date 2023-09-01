require 'concurrent-edge'

def do_stuff(*args)
  :stuff
end

Concurrent.use_simple_logger Logger::DEBUG
