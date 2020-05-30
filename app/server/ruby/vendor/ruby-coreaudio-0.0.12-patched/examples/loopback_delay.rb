
require "coreaudio"

inbuf = CoreAudio.default_input_device.input_buffer(1024)
outbuf = CoreAudio.default_output_device.output_buffer(1024)

th = Thread.start do
  ary = []
  loop do
    wav = inbuf.read(1024)
    ary.push(wav)
    # 1024*43 frames delayed. about 1 sec when sample rate = 44100Hz
    if ary.size > 43
      outbuf << ary.shift
    end
  end
end

inbuf.start
outbuf.start
$stdout.print "loopback..."
$stdout.flush
sleep 10;
inbuf.stop
outbuf.stop
$stdout.puts "done."
th.kill.join

puts "#{inbuf.dropped_frame} frame dropped at input buffer."
puts "#{outbuf.dropped_frame} frame dropped at output buffer."
