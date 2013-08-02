#!/usr/bin/env ruby

$:.unshift File.expand_path("../vendor/osc-ruby/lib", __FILE__)
load(File.absolute_path("#{File.dirname(__FILE__)}/util.rb"))

require 'osc-ruby'
require 'thread'

STDOUT.sync
Thread.abort_on_exception = true

class Spider
  attr_accessor :current_synth_id, :bpm, :client, :debug, :current_pad, :mouse_y, :mouse_x

  ID_SEM = Mutex.new
  PAD_SEM = Mutex.new
  OSC_SEM = Mutex.new
  X_SEM = Mutex.new
  Y_SEM = Mutex.new
  PRINT_SEM = Mutex.new
  ROOT_GROUP = 0
  SYNTH_GROUP = 1
  MIXER_GROUP = 2
  MIXER_ID = 3
  PAD_SYNTH_ID = 4
  INITIAL_SYNTH_ID = 5
  MIXER_BUS = 10
  SYNTHS = ["beep", "fm", "pretty_bell", "dull_bell", "saw_beep"]
  PAD_SYNTHS = ["babbling", "woah", "saws"]

  def initialize
    spider_log "Inintialising Spider"
    self.client = OSC::Client.new('localhost', 4556)
    self.current_synth = "pretty_bell"
    self.current_synth_id = INITIAL_SYNTH_ID
    self.bpm = 60
    self.debug = false
    self.current_pad = ""
    self.mouse_x = 0
    self.mouse_y = 0
    reset_scsynth!



    threads = []
    Dir["/dev/input/event*"].each do |input|
      threads << Thread.new do
        f = File.open(input)
        loop do
          binary = f.read 16
          tv_sec, tv_usec, type, code, value = binary.unpack "llSSl"
#          desc = key_codes[code]
          desc = "TODO"
          handle_raw_event(type, code, value, desc)
        end
      end
    end


  end

  def current_synth
    Thread.current[:current_synth]
  end

  def current_synth=(name)
    Thread.current[:current_synth] = name
  end

  def handle_raw_event(type, code, value, desc)
    # puts event
    case
    when type == 2 && code == 0
      mouse_x_update(value)
    when type == 2 && code == 1
      mouse_y_update(value)
    when type == 1 && value == 1
      #handle_key_press(code_str)
    else
    # puts event
    end
  end

  def key_note_str(n_str)
    # note = case n_str
    #        when "A"; 48 #C3
    #        when "W"; 49 #C#3
    #        when "S"; 50 #D3
    #        when "E"; 51 #D#3
    #        when "D"; 52 #E3
    #        when "F"; 53 #F3
    #        when "T"; 54 #F#3
    #        when "G"; 55 #G3
    #        when "Y"; 56 #Ab3
    #        when "H"; 57 #A4
    #        when "U"; 58 #Bb4
    #        when "J"; 59 #B4
    #        when "K"; 60 #C4
    #        when "O"; 61 #C#4
    #        when "L"; 62 #D4
    #        when "P"; 63 #D#4
    #        when "Semicolon"; 64 #E4
    #        when "Apostrophe"; 65 #F4
    #        end

    #  key_note(note)
  end

  def handle_key_press(key_str)
    case key_str
    when "A", "S", "D", "F", "G", "H", "J", "K", "L", "Semicolon", "Apostrophe", "W", "E", "T", "Y", "U", "O", "P"
      key_note_str(key_str)
    end
  end

  def mouse_x_update(val)
    X_SEM.synchronize do
      self.mouse_x += val
      osc "/n_set", PAD_SYNTH_ID, "x", self.mouse_x
    end
  end

  def mouse_y_update(val)
    Y_SEM.synchronize do
      self.mouse_y += val
      osc "/n_set", PAD_SYNTH_ID, "y", self.mouse_y
    end
  end

  def reset_scsynth!
    load_synthdefs
    clear_scsynth
    create_groups
    start_mixer
  end

  def create_groups
    message "Creating mixer and synth groups" if debug
    osc "/g_new", MIXER_GROUP, 0, 0
    osc "/g_new", SYNTH_GROUP, 2, MIXER_GROUP
  end

  def load_synthdefs
    osc "/d_loadDir", sp_synthdefs_path
  end

  def clear_scsynth
    message "Clearing scsynth" if debug
    osc "/clearSched"
    osc "/g_freeAll", 0
    osc "/g_deepFree", 0
  end

  def start_mixer
    message "Starting mixer" if debug
    osc "/s_new", "sp/mixer", MIXER_ID, 0, MIXER_GROUP, "in-bus", MIXER_BUS, "pan", 0
  end

  def trigger_synth(synth_name, *args)
    if SYNTHS.include? synth_name
      message "Triggering synth #{synth_name}" if debug
      osc "/s_new", "sp/#{synth_name}", new_synth_id, 1, SYNTH_GROUP, "out-bus", MIXER_BUS, *args
    else
      raise "No triggerable synth named: #{synth_name}"
    end
  end

  def stop_scsynth
    message "Stopping scsynth" if debug
    osc "/g_freeAll", SYNTH_GROUP
  end

  def beat_s()
    60.0 / bpm
  end

  def osc(*args)
    OSC_SEM.synchronize do
      message "--> osc: #{args}" if debug
      client.send(OSC::Message.new(*args))
    end
  end

  def new_synth_id()
    ID_SEM.synchronize do
      self.current_synth_id += 1
    end
  end

  def switch_to_pad(name, *args)
    if PAD_SYNTHS.include? name
      PAD_SEM.synchronize do
        osc "/n_free", PAD_SYNTH_ID
        message "Switching to pad #{name} with args: #{args}"
        osc "/s_new", "sp/#{name}", PAD_SYNTH_ID, 1, SYNTH_GROUP, "out-bus", MIXER_BUS, *args
        self.current_pad = name
      end
    else
      raise "No pad synth named: #{name}"
    end
  end

  def control_pad(*args)
    PAD_SEM.synchronize do
      message "Controlling pad #{self.current_pad}: #{args}"
      osc "/n_set", PAD_SYNTH_ID, *args
    end
  end

  def volume=(vol)
    message "Setting volume to: #{vol}" if debug
    osc "/n_set", MIXER_ID, "amp", vol
  end

  def message(s)
    PRINT_SEM.synchronize { puts s ; STDOUT.flush ; STDOUT.flush }
  end
end

class SpiderLang

  attr_accessor :spider

  def initialize()
    self.spider = Spider.new
    spider.message "Starting..."
  end

  def with_synth(synth_name)
    spider.current_synth = synth_name
  end

  def play_synth(synth_name, *args)
    spider.message "playing #{synth_name} with: #{args}"
    STDOUT.flush
    STDOUT.flush
    spider.trigger_synth synth_name, *args
  end

  def play(note, *args)
    play_synth spider.current_synth, "note", note, *args
  end

  def repeat(&block)
    while true
      block.call
    end
  end

  def with_tempo(n)
    spider.bpm = n
  end

  def current_tempo
    spider.beat_s
  end

  def play_pattern(notes, *args)
    notes.each{|note| play(note, *args) ; sleep(spider.beat_s)}
  end

  def play_pattern_timed(notes, times, *args)
    notes.each_with_index{|note, idx| play(note, *args) ; sleep(times[idx % times.size])}
  end

  def play_chord(notes, *args)
    notes.each{|note| play(note, *args)}
  end

  def stop
    spider_log "Stopping..."
    spider.stop_scsynth
  end

  def play_pad(name, *args)
    if args.size == 1
      spider.switch_to_pad(name, "note", args[0])
    else
      spider.switch_to_pad(name, *args)
    end
  end

  def control_pad(*args)
    spider.control_pad(*args)
  end

  def spider_eval(code)
    eval(code)
    STDOUT.flush
    STDOUT.flush
    Thread.list.map {|t| t.join 60}
  end

  def debug!
    spider.debug = true
  end

  def debug_off!
    spider.debug = false
  end

  def in_thread(&block)
    Thread.new do
      with_synth "pretty_bell"
      block.call
    end
  end

  def with_volume(vol)
    if (vol < 0)
      spider.volume = 0
    elsif (vol > 3)
      spider.volume = 3
    else
      spider.volume = vol
    end
  end
end

puts SpiderLang.new.spider_eval(ARGF.read + "\nnil")
