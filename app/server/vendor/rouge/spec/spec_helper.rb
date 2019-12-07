# -*- coding: utf-8 -*- #

require 'rubygems'
require 'bundler'
Bundler.require

require 'rouge'
require 'minitest/spec'
require 'wrong/adapters/minitest'

Wrong.config[:color] = true

Dir[File.expand_path('support/**/*.rb', File.dirname(__FILE__))].each {|f|
  require f
}
