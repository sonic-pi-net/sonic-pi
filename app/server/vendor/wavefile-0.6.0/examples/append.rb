require 'wavefile'
include WaveFile

BUFFER_SIZE = 4096

Writer.new("append.wav", Format.new(:mono, :pcm_16, 44100)) do |writer|
  ARGV.each do |file_name|
    Reader.new(file_name).each_buffer(BUFFER_SIZE) do |buffer|
      writer.write(buffer)
    end
  end
end
