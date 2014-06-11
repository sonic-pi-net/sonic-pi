require 'thread_safe'
require File.join(File.dirname(__FILE__), "test_helper")

class TestHash < Minitest::Test
  def test_concurrency
    hsh = ThreadSafe::Hash.new
    (1..100).map do |i|
      Thread.new do
        1000.times do |j|
          hsh[i*1000+j] = i
          hsh[i*1000+j]
          hsh.delete(i*1000+j)
        end
      end
    end.map(&:join)
  end
end
