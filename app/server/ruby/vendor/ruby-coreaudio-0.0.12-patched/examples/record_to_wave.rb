
require "coreaudio"

dev = CoreAudio.default_input_device
buf = dev.input_buffer(1024)

wav = CoreAudio::AudioFile.new("sample.wav", :write, :format => :wav,
                               :rate => dev.nominal_rate,
                               :channels => dev.input_stream.channels)

samples = 0
th = Thread.start do
  loop do
    w = buf.read(4096)
    samples += w.size / dev.input_stream.channels
    wav.write(w)
  end
end

buf.start;
$stdout.print "RECORDING..."
$stdout.flush
sleep 5;
buf.stop
$stdout.puts "done."
th.kill.join

wav.close

puts "#{samples} samples read."
puts "#{buf.dropped_frame} frame dropped."
