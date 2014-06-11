require 'thread_safe/synchronized_delegator.rb'
require File.join(File.dirname(__FILE__), "test_helper")

class TestSynchronizedDelegator < Minitest::Test

  def test_wraps_array
    sync_array = SynchronizedDelegator.new(array = [])

    array << 1
    assert_equal 1, sync_array[0]

    sync_array << 2
    assert_equal 2, array[1]
  end

  def test_synchronizes_access
    t1_continue, t2_continue = false, false

    hash = Hash.new do |hash, key|
      t2_continue = true
      unless hash.find { |e| e[1] == key.to_s } # just to do something
        hash[key] = key.to_s
        Thread.pass until t1_continue
      end
    end
    sync_hash = SynchronizedDelegator.new(hash)
    sync_hash[1] = 'egy'

    t1 = Thread.new do
      sync_hash[2] = 'dva'
      sync_hash[3] # triggers t2_continue
    end

    t2 = Thread.new do
      Thread.pass until t2_continue
      sync_hash[4] = '42'
    end

    sleep(0.05) # sleep some to allow threads to boot

    until t2.status == 'sleep' do
      Thread.pass
    end

    assert_equal 3, hash.keys.size

    t1_continue = true
    t1.join; t2.join

    assert_equal 4, sync_hash.size
  end

  def test_synchronizes_access_with_block
    t1_continue, t2_continue = false, false

    sync_array = SynchronizedDelegator.new(array = [])

    t1 = Thread.new do
      sync_array << 1
      sync_array.each do
        t2_continue = true
        Thread.pass until t1_continue
      end
    end

    t2 = Thread.new do
      # sleep(0.01)
      Thread.pass until t2_continue
      sync_array << 2
    end

    until t2.status == 'sleep' || t2.status == false do
      Thread.pass
    end

    assert_equal 1, array.size

    t1_continue = true
    t1.join; t2.join

    assert_equal [1, 2], array
  end

end
