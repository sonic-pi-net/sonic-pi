
require "thread"
require "fftw3"
require "coreaudio"

Thread.abort_on_exception = true

inbuf = CoreAudio.default_input_device.input_buffer(1024)
outbuf = CoreAudio.default_output_device.output_buffer(1024)

queue = Queue.new
pitch_shift_th = Thread.start do
  while w = queue.pop
    half = w.shape[1] / 2
    f = FFTW3.fft(w, 1)
    shift = 12
    f.shape[0].times do |ch|
      f[ch, (shift+1)...half] = f[ch, 1...(half-shift)]
      f[ch, 1..shift] = 0
      f[ch, (half+1)...(w.shape[1]-shift)] = f[ch, (half+shift+1)..-1]
      f[ch, -shift..-1] = 0
    end
    outbuf << FFTW3.ifft(f, 1) / w.shape[1]
  end
end

th = Thread.start do
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
th.kill.join
pitch_shift_th.kill.join

puts "#{inbuf.dropped_frame} frame dropped at input buffer."
puts "#{outbuf.dropped_frame} frame dropped at output buffer."
