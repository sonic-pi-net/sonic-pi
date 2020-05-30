require "bundler/gem_helper"

base_dir = File.join(__dir__, "..")
helper = Bundler::GemHelper.new(base_dir)
helper.install
