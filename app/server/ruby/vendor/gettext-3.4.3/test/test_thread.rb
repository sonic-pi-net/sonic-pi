# -*- coding: utf-8 -*-

require 'thread'

class TestThread < Test::Unit::TestCase
  include GetText

  bindtextdomain "test1", :path => "locale"

  def setup
    Locale.init(:driver => :env)
    @mutex = Mutex.new
  end

  def invoke_thread(tag, language, sleep_time)
    Thread.start do
      @mutex.synchronize {
        Thread.current["language"] = language
        GetText.current_locale = tag
      }
      (1..10).each do |v|
        @mutex.synchronize{
          assert_equal Thread.current["language"], _("language")
        }
        print "."
        $stdout.flush
        sleep sleep_time
      end
    end
  end

  def test_thread
    th1 = invoke_thread("ja_JP.eucJP", "japanese", 0.4)
    th2 = invoke_thread("fr", "french", 0.3)
    th3 = invoke_thread("en", "language", 0.1)
    th4 = invoke_thread("zh_CN", "language", 0.2) # No translation.
    th1.join
    th2.join
    th3.join
    th4.join
  end
end
