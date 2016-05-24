require 'locale'
require 'thread'
require 'test/unit'

class TestThread < Test::Unit::TestCase

  def setup
    Locale.init(:driver => :env)
    @mutex = Mutex.new
  end

  def invoke_thread(tag, sleep_time)
    Thread.start do
      @mutex.synchronize {
        ENV["LC_ALL"] = tag
        Locale.current
      }
      (1..10).each do |v|
#        puts "#{tag}: locale = #{Locale.current}"
        assert_equal tag, Locale.current.to_posix.to_s
        print "."
        $stdout.flush
        sleep sleep_time
      end
      Locale.clear # Clear this thread only.
    end
  end

  def test_thread
    th1 = invoke_thread("ja_JP.eucJP", 0.3)
    th2 = invoke_thread("zh_CN.UTF-8", 0.2)
    th3 = invoke_thread("en", 0.1)
    th1.join
    th2.join   
    th3.join
  end
end
