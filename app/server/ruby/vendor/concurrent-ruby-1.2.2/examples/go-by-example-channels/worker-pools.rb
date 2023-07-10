#!/usr/bin/env ruby

$: << File.expand_path('../../../lib', __FILE__)
require 'concurrent-edge'
Channel = Concurrent::Channel

## Go by Example: Go by Example: Worker Pools
# https://gobyexample.com/worker-pools

def worker(id, jobs, results)
  jobs.each do |j|
    print "worker #{id} processing job #{j}\n"
    sleep(1)
    results << j * 2
  end
end

jobs    = Channel.new(buffer: :buffered, capacity: 100)
results = Channel.new(buffer: :buffered, capacity: 100)

(1..3).each do |w|
  Channel.go { worker(w, jobs, results) }
end

(1..9).each do |j|
  jobs << j
end
jobs.close

(1..9).each do
  ~results
end

__END__
worker 1 processing job 1
worker 2 processing job 2
worker 3 processing job 3
worker 1 processing job 4
worker 2 processing job 5
worker 3 processing job 6
worker 1 processing job 7
worker 2 processing job 8
worker 3 processing job 9
