# :stopdoc:

class Hoe
end

module Hoe::Minitest
  def initialize_minitest
    dir = "../../minitest/dev/lib"
    Hoe.add_include_dirs dir if File.directory? dir

    gem "minitest"
    require "minitest"
    version = Minitest::VERSION.split(/\./).first(2).join(".")

    dependency "minitest", "~> #{version}", :development unless
      self.name == "minitest" or ENV["MT_NO_ISOLATE"]
  end

  def define_minitest_tasks
    self.testlib = :minitest

    # make sure we use the gemmed minitest on 1.9
    self.test_prelude = 'gem "minitest"' unless
      self.name == "minitest" or ENV["MT_NO_ISOLATE"]
  end
end
