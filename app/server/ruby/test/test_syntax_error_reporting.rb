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

require_relative "setup_test"
require_relative "../lib/sonicpi/lang/core"

module SonicPi
  # Syntax errors must report the line of the offending code in the user's
  # buffer, not the runtime's eval call site (issue #2555: a constant line
  # number was reported regardless of where the error actually was).
  class SyntaxErrorReportingTester < Minitest::Test

    def setup
      @lang = SonicPi::MockLang.new
    end

    # Mirrors __spider_eval's eval call shape (same file-name and first-line
    # arguments) so the SyntaxError carries exactly what the real run path
    # sees, then routes it through __syntax_error as the job thread does.
    def report_for(code, workspace: "workspace_zero", first_line_num: 1)
      info = { workspace: workspace.dup.freeze, code: code.dup.freeze,
               first_line_num: first_line_num }.freeze
      lang = @lang
      err = nil
      Thread.new do
        lang.instance_eval do
          __system_thread_locals.set :sonic_pi_spider_job_info, info
          begin
            eval(code, nil, info[:workspace], first_line_num)
          rescue SyntaxError => e
            __syntax_error(e)
          end
        end
      rescue Exception => e
        err = e
      end.join
      raise err if err
      # MockLang init chatter precedes the error report on the queue.
      until (msg = lang.msg_queue.pop(true))[:type] == :syntax_error; end
      msg
    end

    def test_error_line_mid_buffer
      msg = report_for("play 60\nsleep 1\nplay ]\nsleep 1\n")
      assert_equal :syntax_error, msg[:type]
      assert_equal 3, msg[:linenum]
      assert_includes msg[:error_line], "play ]"
    end

    def test_error_line_on_first_line
      msg = report_for("play ]\nsleep 1\n")
      assert_equal :syntax_error, msg[:type]
      assert_equal 1, msg[:linenum]
    end

    def test_error_line_deep_in_buffer_not_eval_site
      code = ("sleep 0.1\n" * 40) + "play ]\n"
      msg = report_for(code)
      assert_equal :syntax_error, msg[:type]
      assert_equal 41, msg[:linenum]
      assert_includes msg[:error_line], "play ]"
    end

    # Prism's recovery phrasing must not leak into the report: the run
    # failed, so telling the user the stray token was "ignored" is false.
    # The diagnostic wording varies by Ruby parser ("unexpected ','" vs
    # "cannot parse the expression") — accept either.
    def test_no_recovery_phrasing_in_message
      msg = report_for(", play 71, amp: 100\n")
      assert_equal :syntax_error, msg[:type]
      assert msg[:val].include?("unexpected ','") || msg[:val].include?("cannot parse the expression"),
             "expected the parser's own diagnostic in: #{msg[:val]}"
      refute_includes msg[:val], "ignoring it"
    end
  end
end
