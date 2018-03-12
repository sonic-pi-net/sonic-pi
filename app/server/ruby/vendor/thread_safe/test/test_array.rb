require 'thread_safe'
require File.join(File.dirname(__FILE__), "test_helper")

class TestArray < Minitest::Test
  def test_concurrency
    ary = ThreadSafe::Array.new
    (1..100).map do |i|
      Thread.new do
        1000.times do
          ary << i
          ary.each {|x| x * 2}
          ary.shift
          ary.last
        end
      end
    end.map(&:join)
  end
end
