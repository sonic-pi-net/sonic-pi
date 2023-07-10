#!/usr/bin/env ruby

$: << File.expand_path('../../../lib', __FILE__)
require 'concurrent-edge'
Channel = Concurrent::Channel

## Go by Example: Channel Synchronizatio
# https://gobyexample.com/channel-synchronization

def worker(done_channel)
  print "working...\n"
  sleep(1)
  print "done\n"

  done_channel << true # alias for `#put`
end

done = Channel.new(capacity: 1) # buffered
Channel.go{ worker(done) }

~done # alias for `#take`

__END__
working...
done
