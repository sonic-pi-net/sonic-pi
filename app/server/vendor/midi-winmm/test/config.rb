module TestHelper::Config
  
  include MIDIWinMM
  
  # adjust these constants to suit your hardware configuration 
  # before running tests
  
  NumDevices = 4 # this is the total number of MIDI devices that are connected to your system
  TestInput = Input.first # this is the device you wish to use to test input
  TestOutput = Output.all[1] # likewise for output
  
end