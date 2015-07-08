#!/usr/bin/env ruby

require "coreaudio"

wav = CoreAudio::AudioFile.new("sample.wav", :read)

m4a = CoreAudio::AudioFile.new("sample.m4a", :write, :format => :m4a,
                               :rate => wav.rate,
                               :channels => wav.channels)

loop do
  buf = wav.read(1024)
  break if buf.nil?
  m4a.write(buf)
end

wav.close
m4a.close

