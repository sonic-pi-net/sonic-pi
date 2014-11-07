dir = File.dirname(File.expand_path(__FILE__))
$LOAD_PATH.unshift dir + '/../lib'

require 'midi-winmm'

# this program selects the first midi input and sends an inspection of the first 10 messages 
# messages it receives to standard out

num_messages = 10

# MIDIWinMM::Input.all.to_s will list your midi inputs

MIDIWinMM::Input.first.open do |input|

  $>.puts "send some MIDI to your input now..."

  num_messages.times do
    m = input.gets
    $>.puts(m)
  end

end

$>.puts "finished"
