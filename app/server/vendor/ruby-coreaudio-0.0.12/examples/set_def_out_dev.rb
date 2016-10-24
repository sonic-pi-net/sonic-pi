require "coreaudio"

# Select option name device as default output device 
def set_interface(name)
	if !name
		puts 'please enter audio output interface name.'
		return -1
	end

	devs = CoreAudio.devices

	tgt = devs.find{|dev| dev.name.index(name)}
	if !tgt
		p "no match interface #{name}"
		return -1
	end

	CoreAudio.set_default_output_device(tgt)
	p "select default output audio interface #{tgt.name}"
end

set_interface(ARGV[0])
