require 'thread'
require 'concurrent/edge/throttle'

RSpec.describe 'Concurrent' do
  describe 'Throttle' do
    specify 'acquiring' do
      skip('flaky on truffleruby') if Concurrent.on_truffleruby?

      throttle = Concurrent::Throttle.new 2
      expect(throttle.max_capacity).to eq 2
      expect(throttle.available_capacity).to eq 2

      expect(throttle.try_acquire).to be_truthy
      expect(throttle.max_capacity).to eq 2
      expect(throttle.available_capacity).to eq 1

      thread = in_thread { throttle.acquire; throttle.release; :ok }
      expect(thread.value).to eq :ok

      expect(throttle.try_acquire).to be_truthy
      expect(throttle.max_capacity).to eq 2
      expect(throttle.available_capacity).to eq 0

      thread1 = in_thread { throttle.acquire(2); throttle.release; :ok }
      thread2 = in_thread { throttle.acquire(0.01) { :ok } }
      thread3 = in_thread { throttle.acquire(2) { :ok } }
      is_sleeping thread1
      expect(thread2.value).to be_falsey
      is_sleeping thread3

      expect(throttle.try_acquire).to be_falsey
      expect(throttle.max_capacity).to eq 2
      expect(throttle.available_capacity).to eq(0)
      expect(throttle.send(:capacity)).to eq(-3)

      throttle.release

      expect(throttle.max_capacity).to eq 2
      expect(throttle.available_capacity).to eq 0
      expect(thread1.value).to eq :ok
      expect(thread3.value).to eq :ok
    end

    specify '#to_s' do
      throttle = Concurrent::Throttle.new 2
      expect(throttle.to_s).to match(/Throttle.*available 2 of 2/)
    end

    specify '#on' do
      throttle = Concurrent::Throttle.new 2
      io_proxy = throttle.on :io
      expect(throttle.on(:io)).to eq io_proxy

      expect(io_proxy.can_overflow?).to eq Concurrent.executor(:io).can_overflow?
      expect(io_proxy.serialized?).to eq Concurrent.executor(:io).serialized?

      # cache only one proxy
      fast_proxy = throttle.on :fast
      expect(throttle.on(:io)).to eq io_proxy
      expect(throttle.on(:fast)).not_to eq fast_proxy
    end

    specify 'capacity limited' do
      limit    = 4
      throttle = Concurrent::Throttle.new limit
      counter  = Concurrent::AtomicFixnum.new
      testing  = -> i do
        counter.increment
        sleep rand * 0.02 + 0.02
        # returns less then 3 since it's throttled
        v = counter.value
        counter.decrement
        v
      end

      result = Concurrent::Promises.zip(
          *20.times.map { |i| throttle.future(i, &testing) }
      ).value!
      expect(result.all? { |v| v <= limit }).to be_truthy, result.to_s

      result = Array.new(20) do |i1|
        Thread.new(i1) { |i2| throttle.acquire { testing.call i2 } }
      end.map(&:value)
      expect(result.all? { |v| v <= limit }).to be_truthy, result.to_s

      throttled_futures = 20.times.map do |i|
        Concurrent::Promises.
            fulfilled_future(i).
            then_on(throttle.on(:io), &testing)
      end

      result = Concurrent::Promises.zip(*throttled_futures).value!
      expect(result.all? { |v| v <= limit }).to be_truthy, result.to_s
    end
  end
end
