require "coreaudio"

# Select option name device as default output device
def set_nominal_rate(rate)
  available_rates = CoreAudio.default_output_device.available_sample_rate.flatten.uniq
  rate = rate.to_f
  if (!rate || !available_rates.member?(rate))
    puts "Please enter a valid sample rate. Choose one of the following: #{available_rates.join(', ')}"
    return -1
  end

  CoreAudio.default_output_device(nominal_rate: rate)
  puts "Output device sample rate set to #{rate}"
end

set_nominal_rate(ARGV[0])
