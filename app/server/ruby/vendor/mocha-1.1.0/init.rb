# Mocha should no longer be loaded at plugin load time
# You should explicitly load Mocha *after* Test::Unit or MiniTest have been loaded
# e.g. by adding "require 'mocha'" at the bottom of test/test_helper.rb
