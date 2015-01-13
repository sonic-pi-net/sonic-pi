require "coreaudio"

dev = CoreAudio.default_output_device
buf = dev.output_loop(44000)

phase = Math::PI * 2.0 * 440.0 / 44000.0
44000.times do |i|
  buf[i] = ((0.4 * Math.sin(phase*i)) * 0x7FFF).round
end

buf.start
sleep 2
buf.stop

