require 'digest/md5'

require "codeclimate-test-reporter"
CodeClimate::TestReporter.start

require 'ruby-beautify'


BEAUTIFY_BIN =  "#{Dir.pwd}/bin/ruby-beautify"
