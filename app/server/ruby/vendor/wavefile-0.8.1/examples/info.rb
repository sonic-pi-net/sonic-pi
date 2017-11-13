require 'wavefile'
include WaveFile

file_name = ARGV[0]
puts "Metadata for #{file_name}:"

begin
  reader = Reader.new(file_name)

  puts "  Readable by this gem?  #{reader.readable_format? ? 'Yes' : 'No'}"
  puts "  Audio Format:          #{reader.native_format.audio_format}"
  puts "  Channels:              #{reader.native_format.channels}"
  puts "  Bits per sample:       #{reader.native_format.bits_per_sample}"
  puts "  Samples per second:    #{reader.native_format.sample_rate}"
  puts "  Bytes per second:      #{reader.native_format.byte_rate}"
  puts "  Block align:           #{reader.native_format.block_align}"
  puts "  Sample frame count:    #{reader.total_sample_frames}"

  duration = reader.total_duration
  formatted_duration = duration.hours.to_s.rjust(2, "0") << ":" <<
                       duration.minutes.to_s.rjust(2, "0") << ":" <<
                       duration.seconds.to_s.rjust(2, "0") << ":" <<
                       duration.milliseconds.to_s.rjust(3, "0")
  puts "  Play time:             #{formatted_duration}"
rescue InvalidFormatError
  puts "  Not a valid Wave file!"
end
