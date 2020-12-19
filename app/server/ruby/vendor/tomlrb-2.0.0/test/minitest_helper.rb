$LOAD_PATH.unshift File.expand_path('../../lib', __FILE__)
require 'tomlrb'

require 'minitest/autorun'
require "minitest/reporters"
Minitest::Reporters.use!
Minitest::Spec.make_my_diffs_pretty!
