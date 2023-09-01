require 'concurrent-edge'

include Concurrent::ErlangActor::EnvironmentConstants

def do_stuff(*args)
  sleep 0.01
  :stuff
end

