#!/usr/bin/env ruby
require_relative "core.rb"

require_relative "sonicpi/scsynth"
require_relative "sonicpi/studio"
require_relative "sonicpi/spider"
require_relative "sonicpi/server"
require_relative "sonicpi/util"
require_relative "sonicpi/rcv_dispatch"

#Thread.abort_on_exception=true

include SonicPi::Util

ws_out = Queue.new
$scsynth = SonicPi::SCSynth.instance

$c = OSC::Client.new("localhost", 4556)

at_exit do
  $c.send(OSC::Message.new("/quit"))
end


$c.send(OSC::Message.new("/d_loadDir", synthdef_path))
sleep 2

$sp =  SonicPi::Spider.new "localhost", 4556, ws_out, 5

$rd = SonicPi::RcvDispatch.new($sp, ws_out)
$clients = []

# Send stuff out from Sonic Pi jobs out to GUI
out_t = Thread.new do
  continue = true
  while continue
    begin
      message = ws_out.pop
      message[:ts] = Time.now.strftime("%H:%M:%S")

      if message[:type] == :exit
        continue = false
      else
        puts message
      end
    rescue Exception => e
      puts "Exception!"
      puts e.message
      puts e.backtrace.inspect
    end
  end
end

Thread.new do
  f = File.open("/tmp/gc.txt", 'w')
  loop do
    f.puts GC.stat
    f.flush
    sleep 2
  end
end

def test_simple
  $rd.dispatch({:cmd => "run-code",
                :val => "play 60"})
end

def test_multi_osc
  $rd.dispatch({:cmd => "run-code",
                :val => "loop do ; status ; sleep 0.025 ; end"})
end

def test_multi_play
  $rd.dispatch({:cmd => "run-code",
                :val => "loop do ; play 60 ; sleep 0.025 ; end"})
end

def test_multi_threads
  $rd.dispatch({:cmd => "run-code",
                :val => "loop do ; in_thread do ; play 60 ; sleep 3 ; end ; sleep 0.025 ; end"})
end

def test_multi_jobs
  loop do
    $rd.dispatch({:cmd => "run-code",
                  :val => "play 60"})
    sleep 0.025
  end
end

def test_exception_throwing
  loop do
    $rd.dispatch({:cmd => "run-code",
                  :val => "play 60 ; 1/0"})
    sleep 0.025
  end
end

def test_exception_throwing_within_subthread
  loop do
    $rd.dispatch({:cmd => "run-code",
                  :val => "play 60 ; in_thread do ; 1/0 ; end"})
    sleep 0.025
  end
end

def test_all_jobs_stopping
  loop do
    $rd.dispatch({:cmd => "run-code",
                   :val => "loop do ; play 60 ; sleep 0.025 ; end"})
    sleep 3
    $rd.dispatch({:cmd => "stop-jobs"})
    sleep 1
  end
end

def test_blue_monday
  $rd.dispatch({:cmd => "run-code",
                :val => "

load_samples :heavy_kick, :snare_soft

def drums
  6.times do
    sample :heavy_kick, :rate, 0.8
    sleep 0.5
  end

  8.times do
    sample :heavy_kick, :rate, 0.8
    sleep 0.125
  end
end

def snare
  sample :snare_soft
  sleep 1
end

def synths
  with_synth \"saw_beep\"
  notes = [:F, :C, :D, :D, :G, :C, :D, :D]
  notes.each do |n|
    2.times do
      play note(n, 1), :amp, 0.5, :attack, 0.01, :release, 0.5
      play note(n, 2), :amp, 0.5, :attack, 0.01, :release, 0.75

      sleep 0.25
      play note(n, 2), :amp, 0.5, :attack, 0.01, :release, 0.5
      play note(n, 3), :amp, 0.5, :attack, 0.01, :release, 0.75

      sleep 0.25
    end
  end
end


in_thread do
  loop{drums}
end

in_thread do
  sleep 6
  loop{synths}
end

in_thread do
  sleep 12.5
  loop{snare}
end"})

end

test_blue_monday

out_t.join
