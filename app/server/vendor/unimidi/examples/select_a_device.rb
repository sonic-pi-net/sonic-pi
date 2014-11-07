#!/usr/bin/env ruby
$:.unshift File.join( File.dirname( __FILE__ ), '../lib')

require "unimidi"

#
# This is an example that explains how to select an output.
# It's not really meant to be run.
#

# The simplest way to select an output is to prompt the user for selection in 
# the Ruby console

output = UniMIDI::Output.gets

# the user will see a list that represents their personal MIDI configuration like:

# Select a MIDI output
# 1) IAC Device
# 2) Roland UM-2 (1)
# 3) Roland UM-2 (2)
# >

# and be prompted to select a number

# once they've selected, the device that corresponds with their selection is returned

# and it's returned open so you don't need to call output.open ever

# you can also hard code the selection like this

output = UniMIDI::Output.use(:first)
output = UniMIDI::Output.use(0)

# or 

output = UniMIDI::Output.open(:first)
output = UniMIDI::Output.open(0)

# this also returns an open device 

# if you want to wait to open the device, you can select it with any of these "finder" methods

output = UniMIDI::Output.first
output = UniMIDI::Output[0]
output = UniMIDI::Output.all[0]
output = UniMIDI::Output.all.first
output = UniMIDI::Device.all_by_type(:output)[0]
output = UniMIDI::Device.all_by_type(:output).first

# but again, you'll need to call open on it before you use it or you'll get an exception

output.open

