require 'wavefile'
include WaveFile

Writer.new("square.wav", Format.new(:mono, :pcm_16, 44100)) do |writer|
  samples = ([0.5] * 100) + ([-0.5] * 100)
  
  buffer_format = Format.new(:mono, :float, 44100)
  220.times do
    buffer = Buffer.new(samples, buffer_format)
    writer.write(buffer)
  end
end
