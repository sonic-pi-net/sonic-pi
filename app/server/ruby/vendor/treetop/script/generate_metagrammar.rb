#!/usr/bin/env ruby

require 'rubygems'
require File.expand_path('../../lib/treetop/bootstrap_gen_1_metagrammar', __FILE__)

GENERATED_METAGRAMMAR_PATH = File.expand_path('../../lib/treetop/compiler/metagrammar.rb')

File.open(METAGRAMMAR_PATH) do |source_file|
  File.open(GENERATED_METAGRAMMAR_PATH, 'w') do |target_file|
    generated_source = Treetop::Compiler::MetagrammarParser.new.parse(source_file.read).compile
    target_file.write(generated_source)
  end
end
