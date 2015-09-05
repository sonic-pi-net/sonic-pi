require 'osc-ruby'

def client
  client ||= OSC::Client.new("localhost", 4557)
end

Dir.glob("./etc/examples/*/*.rb").each do |ex_path|
  puts ex_path
  client.send OSC::Message.new('/run-code', "examplerenderer", "recording_start; sleep 1;")
  sleep 1
  # Unify the sched_ahead time on all platforms
  client.send OSC::Message.new('/run-code', "examplerenderer", "set_sched_ahead_time! 2.0")
  sleep 1
  client.send OSC::Message.new('/run-code', "examplerenderer", IO.read(ex_path))
  sleep 30 # length of the rendering
  client.send OSC::Message.new('/stop-all-jobs', "examplerenderer")
  sleep 1
  client.send OSC::Message.new('/run-code', "examplerenderer", "recording_stop; sleep 1; recording_save(\"/tmp/#{File.basename(ex_path)}.wav\")")
  sleep 5
end
