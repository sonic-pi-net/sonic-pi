require "spec_helper"
require "hamster/list"
require "atomic"

describe Hamster::List do
  it "ensures each node of a lazy list will only be realized on ONE thread, even when accessed by multiple threads" do
    counter = Atomic.new(0)
    list = Hamster.list(*1..10000).map { |x| counter.update { |count| count + 1 }; x * 2 }

    threads = 10.times.collect do
      Thread.new do
        node = list
        node = node.tail until node.empty?
      end
    end
    threads.each(&:join)

    counter.get.should == 10000
    list.sum.should == 100010000
  end

  it "doesn't go into an infinite loop if lazy list block raises an exception" do
    list = Hamster.list(*1..10).map { raise "Oops!" }

    threads = 10.times.collect do
      Thread.new do
        -> { list.head }.should raise_error
      end
    end
    threads.each(&:join)
  end

  it "doesn't give horrendously bad performance if thread realizing the list sleeps" do
    start = Time.now
    list = Hamster.list(*1..100).map { |x| sleep(0.001); x * 2 }

    threads = 10.times.collect do
      Thread.new do
        node = list
        node = node.tail until node.empty?
      end
    end
    threads.each(&:join)

    elapsed = Time.now - start
    elapsed.should_not > 0.3
  end
end