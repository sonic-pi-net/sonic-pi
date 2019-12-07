#!/usr/bin/env ruby
#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

$:.unshift File.expand_path("../../vendor/osc-ruby/lib", __FILE__)
require 'osc-ruby'
require_relative '../util.rb'

STDOUT.sync
Thread.abort_on_exception = true

$mouse_x = 0
$mouse_y = 0
$mixer_id = 9
$synth_id = 10
$synth_g = 1
$mixer_g = 2
$mixer_b = 10
$octave = 3
$current_note = 40
$system_vol = 1

$client = OSC::Client.new('127.0.0.1', 4556)
$debug = false

def debug(msg)
  # if msg.is_a?(String)
  #   puts msg if $debug
  # else
  #   p msg if $debug
  # end
end

def sc_osc(*args)
  debug [*args]
  $client.send(OSC::Message.new(*args))
end

# create group for synths and group for mixer
# mixer_g will be placed after the synth_g
sc_osc "/g_new", $synth_g, 0, 0
sc_osc "/g_new", $mixer_g, 3, $synth_g

sc_osc "/b_alloc", 1, 5, 1
sleep 1
sc_osc "/b_setn", 1, 0, 5, 51.0, 54.0, 58.0, 51.0, 54,0

def stop_synth
  sc_osc("/n_free", $synth_id)
end

def reset_mouse_pos
  $mouse_x = 0
  $mouse_y = 0
end

def start_synth(name, id=$synth_id, group=$synth_g, out_bus=$mixer_b, in_bus=0, *rest)
  sc_osc("/n_set", $mixer_g, "amp", 0)
  sleep 0.02
  reset_mouse_pos
  stop_synth
  sc_osc("/n_set", $mixer_g, "amp", $system_vol)
  sc_osc("/s_new", name, id, 0, group, "out_bus", out_bus, "in_bus", in_bus, *rest)
  $synth_id
end

def load_synthdefs(dir)
  puts "loading synthdefs from: #{dir}"
  sc_osc "/d_loadDir", dir
end

load_synthdefs sp_synthdefs_path
sleep 2
# os2 = new_synth("beans")
# sleep 2
# sc_osc "/n_free", os2
# os3 = new_synth("saws")
# sleep 2
#sc_osc "/n_set", os3, "note", 52

def mouse_x_update(val)
  $mouse_x += val
  sc_osc "/n_set", $synth_g, "x", $mouse_x
end

def mouse_y_update(val)
#  puts "mouse y #{val}"
  $mouse_y += val
  sc_osc "/n_set", $synth_g, "y", $mouse_y
end

def key_number(n)
  $trigger_synth = nil
  case n
  when 1; start_synth("saws")
  when 2; start_synth("saws")
  when 3; start_synth("fm") && $trigger_synth = "fm"
  when 4; start_synth("woah")
  when 5; start_synth("arpeg-click")
  when 6; start_synth("pretty_bell") && $trigger_synth = "pretty_bell"
  when 7; start_synth("dull_bell") && $trigger_synth = "dull_bell"
  when 0; stop_synth
  end
end


def key_note(n)
  $current_note = n
  if($trigger_synth == "fm")
    start_synth $trigger_synth, $synth_id, $synth_g, $mixer_b, 0, "note", (n + (($octave - 4) * 12)), "divisor", rand(8), "depth", rand(8)
  elsif $trigger_synth
    start_synth $trigger_synth, $synth_id, $synth_g, $mixer_b, 0, "note", (n + (($octave - 4) * 12))
  else
    sc_osc "/n_set", $synth_g, "note", (n + (($octave - 4) * 12))
  end
end

def key_note_str(n_str)
  note = case n_str
  when "A"; 48 #C3
  when "W"; 49 #C#3
  when "S"; 50 #D3
  when "E"; 51 #D#3
  when "D"; 52 #E3
  when "F"; 53 #F3
  when "T"; 54 #F#3
  when "G"; 55 #G3
  when "Y"; 56 #Ab3
  when "H"; 57 #A4
  when "U"; 58 #Bb4
  when "J"; 59 #B4
  when "K"; 60 #C4
  when "O"; 61 #C#4
  when "L"; 62 #D4
  when "P"; 63 #D#4
  when "SEMICOLON"; 64 #E4
  when "APOSTROPHE"; 65 #F4
  end

  key_note(note)
end

def key_octave(oct)
  $octave = oct
  key_note($current_note)
end

def key_vol(key_str)
  case key_str
  when "Up"; $system_vol += 0.1
  when "Down"; $system_vol -= 0.1
  end

  if $system_vol < 0
    $system_vol = 0
  elsif $system_vol > 3
    $system_vol = 3
  end

  sc_osc "/n_set", $mixer_g, "amp", $system_vol
end

def handle_key_press(key_str)
  case key_str
  when "KEY_1", "KEY_2", "KEY_3", "KEY_4", "KEY_5", "KEY_6", "KEY_7", "KEY_8", "KEY_9", "KEY_0"
    key_number(key_str[4..-1].to_i)
  when "KEY_A", "KEY_S", "KEY_D", "KEY_F", "KEY_G", "KEY_H", "KEY_J", "KEY_K", "KEY_L", "KEY_SEMICOLON", "KEY_APOSTROPHE", "KEY_W", "KEY_E", "KEY_T", "KEY_Y", "KEY_U", "KEY_O", "KEY_P"
    key_note_str(key_str[4..-1])
  when "KEY_KP1", "KEY_KP2", "KEY_KP3", "KEY_KP4", "KEY_KP5", "KEYKP6", "KEY_KP7", "KEY_KP8", "KEY_KP9"
    key_octave(key_str[6..6].to_i)
  when "KEY_UP", "KEY_DOWN"
    key_vol(key_str)
  else
    debug key_str
  end
end

def boot
  start_synth "sp/mixer", $mixer_id, $mixer_g, 0, $mixer_b
  key_number 1
end

boot

def handle_raw_event(type, code, value, desc)
  puts desc
    case
    when type == 2 && code == 0
      mouse_x_update(value)
    when type == 2 && code == 1
      mouse_y_update(value)
    when type == 1 && value == 1
      handle_key_press(desc)
    else
    end
end

inputs = Dir["/dev/input/event*"]

key_codes = {
        0 => "KEY_RESERVED",
        1 => "KEY_ESC",
        2 => "KEY_1",
        3 => "KEY_2",
        4 => "KEY_3",
        5 => "KEY_4",
        6 => "KEY_5",
        7 => "KEY_6",
        8 => "KEY_7",
        9 => "KEY_8",
        10 => "KEY_9",
        11 => "KEY_0",
        12 => "KEY_MINUS",
        13 => "KEY_EQUAL",
        14 => "KEY_BACKSPACE",
        15 => "KEY_TAB",
        16 => "KEY_Q",
        17 => "KEY_W",
        18 => "KEY_E",
        19 => "KEY_R",
        20 => "KEY_T",
        21 => "KEY_Y",
        22 => "KEY_U",
        23 => "KEY_I",
        24 => "KEY_O",
        25 => "KEY_P",
        26 => "KEY_LEFTBRACE",
        27 => "KEY_RIGHTBRACE",
        28 => "KEY_ENTER",
        29 => "KEY_LEFTCTRL",
        30 => "KEY_A",
        31 => "KEY_S",
        32 => "KEY_D",
        33 => "KEY_F",
        34 => "KEY_G",
        35 => "KEY_H",
        36 => "KEY_J",
        37 => "KEY_K",
        38 => "KEY_L",
        39 => "KEY_SEMICOLON",
        40 => "KEY_APOSTROPHE",
        41 => "KEY_GRAVE",
        42 => "KEY_LEFTSHIFT",
        43 => "KEY_BACKSLASH",
        44 => "KEY_Z",
        45 => "KEY_X",
        46 => "KEY_C",
        47 => "KEY_V",
        48 => "KEY_B",
        49 => "KEY_N",
        50 => "KEY_M",
        51 => "KEY_COMMA",
        52 => "KEY_DOT",
        53 => "KEY_SLASH",
        54 => "KEY_RIGHTSHIFT",
        55 => "KEY_KPASTERISK",
        56 => "KEY_LEFTALT",
        57 => "KEY_SPACE",
        58 => "KEY_CAPSLOCK",
        59 => "KEY_F1",
        60 => "KEY_F2",
        61 => "KEY_F3",
        62 => "KEY_F4",
        63 => "KEY_F5",
        64 => "KEY_F6",
        65 => "KEY_F7",
        66 => "KEY_F8",
        67 => "KEY_F9",
        68 => "KEY_F10",
        69 => "KEY_NUMLOCK",
        70 => "KEY_SCROLLLOCK",
        71 => "KEY_KP7",
        72 => "KEY_KP8",
        73 => "KEY_KP9",
        74 => "KEY_KPMINUS",
        75 => "KEY_KP4",
        76 => "KEY_KP5",
        77 => "KEY_KP6",
        78 => "KEY_KPPLUS",
        79 => "KEY_KP1",
        80 => "KEY_KP2",
        81 => "KEY_KP3",
        82 => "KEY_KP0",
        83 => "KEY_KPDOT",
        84 => "KEY_103RD",
        85 => "KEY_F13",
        86 => "KEY_102ND",
        87 => "KEY_F11",
        88 => "KEY_F12",
        89 => "KEY_F14",
        90 => "KEY_F15",
        91 => "KEY_F16",
        92 => "KEY_F17",
        93 => "KEY_F18",
        94 => "KEY_F19",
        95 => "KEY_F20",
        96 => "KEY_KPENTER",
        97 => "KEY_RIGHTCTRL",
        98 => "KEY_KPSLASH",
        99 => "KEY_SYSRQ",
        100 => "KEY_RIGHTALT",
        101 => "KEY_LINEFEED",
        102 => "KEY_HOME",
        103 => "KEY_UP",
        104 => "KEY_PAGEUP",
        105 => "KEY_LEFT",
        106 => "KEY_RIGHT",
        107 => "KEY_END",
        108 => "KEY_DOWN",
        109 => "KEY_PAGEDOWN",
        110 => "KEY_INSERT",
        111 => "KEY_DELETE",
        112 => "KEY_MACRO",
        113 => "KEY_MUTE",
        114 => "KEY_VOLUMEDOWN",
        115 => "KEY_VOLUMEUP",
        116 => "KEY_POWER",
        117 => "KEY_KPEQUAL",
        118 => "KEY_KPPLUSMINUS",
        119 => "KEY_PAUSE",
        120 => "KEY_F21",
        121 => "KEY_F22",
        122 => "KEY_F23",
        123 => "KEY_F24",
        124 => "KEY_KPCOMMA",
        125 => "KEY_LEFTMETA",
        126 => "KEY_RIGHTMETA",
        127 => "KEY_COMPOSE",
        128 => "KEY_STOP",
        129 => "KEY_AGAIN",
        130 => "KEY_PROPS",
        131 => "KEY_UNDO",
        132 => "KEY_FRONT",
        133 => "KEY_COPY",
        134 => "KEY_OPEN",
        135 => "KEY_PASTE",
        136 => "KEY_FIND",
        137 => "KEY_CUT",
        138 => "KEY_HELP",
        139 => "KEY_MENU",
        140 => "KEY_CALC",
        141 => "KEY_SETUP",
        142 => "KEY_SLEEP",
        143 => "KEY_WAKEUP",
        144 => "KEY_FILE",
        145 => "KEY_SENDFILE",
        146 => "KEY_DELETEFILE",
        147 => "KEY_XFER",
        148 => "KEY_PROG1",
        149 => "KEY_PROG2",
        150 => "KEY_WWW",
        151 => "KEY_MSDOS",
        152 => "KEY_COFFEE",
        153 => "KEY_DIRECTION",
        154 => "KEY_CYCLEWINDOWS",
        155 => "KEY_MAIL",
        156 => "KEY_BOOKMARKS",
        157 => "KEY_COMPUTER",
        158 => "KEY_BACK",
        159 => "KEY_FORWARD",
        160 => "KEY_CLOSECD",
        161 => "KEY_EJECTCD",
        162 => "KEY_EJECTCLOSECD",
        163 => "KEY_NEXTSONG",
        164 => "KEY_PLAYPAUSE",
        165 => "KEY_PREVIOUSSONG",
        166 => "KEY_STOPCD",
        167 => "KEY_RECORD",
        168 => "KEY_REWIND",
        169 => "KEY_PHONE",
        170 => "KEY_ISO",
        171 => "KEY_CONFIG",
        172 => "KEY_HOMEPAGE",
        173 => "KEY_REFRESH",
        174 => "KEY_EXIT",
        175 => "KEY_MOVE",
        176 => "KEY_EDIT",
        177 => "KEY_SCROLLUP",
        178 => "KEY_SCROLLDOWN",
        179 => "KEY_KPLEFTPAREN",
        180 => "KEY_KPRIGHTPAREN",
        181 => "KEY_INTL1",
        182 => "KEY_INTL2",
        183 => "KEY_INTL3",
        184 => "KEY_INTL4",
        185 => "KEY_INTL5",
        186 => "KEY_INTL6",
        187 => "KEY_INTL7",
        188 => "KEY_INTL8",
        189 => "KEY_INTL9",
        190 => "KEY_LANG1",
        191 => "KEY_LANG2",
        192 => "KEY_LANG3",
        193 => "KEY_LANG4",
        194 => "KEY_LANG5",
        195 => "KEY_LANG6",
        196 => "KEY_LANG7",
        197 => "KEY_LANG8",
        198 => "KEY_LANG9",
        200 => "KEY_PLAYCD",
        201 => "KEY_PAUSECD",
        202 => "KEY_PROG3",
        203 => "KEY_PROG4",
        205 => "KEY_SUSPEND",
        206 => "KEY_CLOSE",
        220 => "KEY_UNKNOWN",
        224 => "KEY_BRIGHTNESSDOWN",
        225 => "KEY_BRIGHTNESSUP",

        256 => "BTN_0",
        257 => "BTN_1",
        258 => "BTN_2",
        259 => "BTN_3"}

threads = []

inputs.each do |input|
  puts "starting thread for: #{input}"
  threads << Thread.new do
    puts "opening file"
    f = File.open(input)
    puts "file open"
    loop do
      puts "[#{input}] reading...."
      binary = f.read 16
      tv_sec, tv_usec, type, code, value = binary.unpack "llSSl"
      desc = key_codes[code]
      puts "[#{input}] got: #{desc}"
      handle_raw_event(type, code, value, desc)
    end
  end
end

threads.each {|thr| thr.join }







# this program prompts the user to select a midi input and sends an inspection of the first 10 messages
# messages it receives to standard out

# num_messages = 500

#

# # prompt the user
# UniMIDI::Input.gets do |input| # using their selection...

#   $>.puts "send some MIDI to your input now..."

#   num_messages.times do
#     m = input.gets
#     val = m[0][:data][2]
#     freq = (((val+1) / 128.0) * 800 ) + 50
#     puts val
#     puts freq
#     client.send(OSC::Message.new("/n_set", 28, "freq", freq))
#   end

#   $>.puts "finished"

# end
