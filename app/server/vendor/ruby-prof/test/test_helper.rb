# encoding: UTF-8

# Make RubyMine happy
require "rubygems"
gem "minitest"

if ENV["RM_INFO"] || ENV["TEAMCITY_VERSION"]
  if RUBY_PLATFORM =~ /(win32|mingw)/
    gem "win32console"
  end
  gem "minitest-reporters"
  require 'minitest/reporters'
  MiniTest::Reporters.use!
end

require "minitest/pride" if RUBY_VERSION == "1.9.3"

# To make testing/debugging easier, test within this source tree versus an installed gem
dir = File.dirname(__FILE__)
root = File.expand_path(File.join(dir, '..'))
lib = File.expand_path(File.join(root, 'lib'))
ext = File.expand_path(File.join(root, 'ext', 'ruby_prof'))

$LOAD_PATH << lib
$LOAD_PATH << ext

require 'ruby-prof'
require 'test/unit'
require File.expand_path('../prime', __FILE__)

# Some classes used in measurement tests
module RubyProf
  class C1
    def C1.hello
      sleep(0.1)
    end

    def hello
      sleep(0.2)
    end
  end

  module M1
    def hello
      sleep(0.3)
    end
  end

  class C2
    include M1
    extend M1
  end

  class C3
    def hello
      sleep(0.4)
    end
  end

  module M4
    def hello
      sleep(0.5)
    end
  end

  module M5
    include M4
    def goodbye
      hello
    end
  end

  class C6
    include M5
    def test
      goodbye
    end
  end

  class C7
    def self.hello
      t = Time.now.to_f
      while Time.now.to_f - t < 0.1; end
    end

    def hello
      t = Time.now.to_f
      while Time.now.to_f - t < 0.2; end
    end
  end

  module M7
    def hello
      t = Time.now.to_f
      while Time.now.to_f - t < 0.3; end
    end
  end

  class C8
    include M7
    extend M7
  end

  def self.ruby_major_version
    match = RUBY_VERSION.match(/(\d)\.(\d)/)
    return Integer(match[1])
  end

  def self.ruby_minor_version
    match = RUBY_VERSION.match(/(\d)\.(\d)/)
    return Integer(match[2])
  end

  def self.parent_object
    if ruby_major_version == 1 && ruby_minor_version == 8
      Object
    else
      BasicObject
    end
  end

  def self.ruby_2?
    ruby_major_version == 2
  end

  # store printer output in this directory
  def self.tmpdir
    File.expand_path('../../tmp', __FILE__)
  end
end

module MemoryTestHelper
  def memory_test_helper
    result = RubyProf.profile {Array.new}
    total = result.threads.first.methods.inject(0) { |sum, m| sum + m.total_time }
    assert(total < 1_000_000, 'Total should not have subtract overflow error')
    total
  end
end
