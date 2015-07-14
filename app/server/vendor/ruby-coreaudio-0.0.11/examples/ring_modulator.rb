
require "thread"
require "coreaudio"

Thread.abort_on_exception = true

inbuf = CoreAudio.default_input_device.input_buffer(1024)
outbuf = CoreAudio.default_output_device.output_buffer(1024)

queue = Queue.new
output_th = Thread.start do
  filter = NArray.float(2, 1024)
  filter.shape[1].times do |i|
    filter[true, i] = (i % 256 - 128) / 128.0
  end

  while w = queue.pop
    outbuf << w * filter
  end
end

input_th = Thread.start do
  loop do
    wav = inbuf.read(1024)
    queue.push(wav)
  end
end

inbuf.start
outbuf.start
$stdout.print "loopback..."
$stdout.flush
sleep 10;
queue.push(nil)
inbuf.stop
outbuf.stop
$stdout.puts "done."
input_th.kill.join
output_th.kill.join

puts "#{inbuf.dropped_frame} frame dropped at input buffer."
puts "#{outbuf.dropped_frame} frame dropped at output buffer."
