#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "../../setup_test"
require_relative "../../../lib/sonicpi/lang/core"

require "tempfile"

module SonicPi
  # Regression for #2336 / #2514: an error raised inside an external file run
  # via run_file (or init.rb) must report the file's path as its location, not
  # the confusing generic "eval" workspace. run_file does this by passing the
  # file path through as the eval workspace, which the error reporter prints as
  # the location ("buffer <path>, line N").
  class RunFileTester < Minitest::Test

    def setup
      @lang = SonicPi::MockLang.new
    end

    def test_run_file_passes_the_file_path_as_the_workspace
      f = Tempfile.new(["sonicpi_test", ".rb"])
      f.write("play 60\n")
      f.close
      captured = nil
      @lang.define_singleton_method(:__spider_eval) { |code, info = {}| captured = info }
      @lang.run_file(f.path)
      assert_equal File.expand_path(f.path), captured[:workspace]
    ensure
      f&.close!
    end

    def test_run_file_on_missing_path_still_raises
      assert_raises(IOError) { @lang.run_file("/no/such/sonic/pi/file.rb") }
    end

    # The workspace flows into the error location verbatim (normalise_buffer_name
    # only rewrites the built-in workspace_* names), so a file path is reported
    # as-is rather than mangled.
    def test_file_path_workspace_is_reported_verbatim
      assert_equal "/home/me/init.rb",
                   @lang.send(:normalise_buffer_name, "/home/me/init.rb")
    end
  end
end
