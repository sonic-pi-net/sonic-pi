require 'wavefile'
include WaveFile

info = Reader.info(ARGV[0])

puts "Metadata for #{info.file_name}:"
puts "  Audio Format:        #{info.audio_format}"
puts "  Channels:            #{info.channels}"
puts "  Bits per sample:     #{info.bits_per_sample}"
puts "  Samples per second:  #{info.sample_rate}"
puts "  Bytes per second:    #{info.byte_rate}"
puts "  Block align:         #{info.block_align}"
puts "  Sample frame count:  #{info.sample_frame_count}"

duration = info.duration
formatted_duration = duration.hours.to_s.rjust(2, "0") << ":" <<
                     duration.minutes.to_s.rjust(2, "0") << ":" <<
                     duration.seconds.to_s.rjust(2, "0") << ":" <<
                     duration.milliseconds.to_s.rjust(3, "0")
puts "  Play time:           #{formatted_duration}"
