#!/usr/bin/env ruby -wKU
$:.unshift File.expand_path("../vendor/osc-ruby/lib", __FILE__)
require 'osc-ruby'

SYNTH_GROUP = 1
client = OSC::Client.new('localhost', 4556)
client.send(OSC::Message.new("/g_freeAll", SYNTH_GROUP))
