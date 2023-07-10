require_relative 'global_thread_pool_shared'
require 'concurrent/atomic/atomic_boolean'
require 'concurrent/atomic/atomic_fixnum'
require 'timeout'

RSpec.shared_examples :executor_service do

  after(:each) do
    subject.shutdown
    expect(subject.wait_for_termination(pool_termination_timeout)).to eq true
  end

  it_should_behave_like :global_thread_pool

  context '#post' do

    it 'rejects the block while shutting down' do
      latch = Concurrent::CountDownLatch.new(1)
      subject.post{ sleep(1) }
      subject.shutdown
      begin
        subject.post{ latch.count_down }
      rescue Concurrent::RejectedExecutionError
      end
      expect(latch.wait(0.1)).to be_falsey
    end

    it 'rejects the block once shutdown' do
      subject.shutdown
      latch = Concurrent::CountDownLatch.new(1)
      begin
        subject.post{ latch.count_down }
      rescue Concurrent::RejectedExecutionError
      end
      expect(latch.wait(0.1)).to be_falsey
    end
  end

  context 'auto terminate' do

    # https://github.com/ruby-concurrency/concurrent-ruby/issues/817
    # https://github.com/ruby-concurrency/concurrent-ruby/issues/839
    it 'does not stop shutdown ' do
      Timeout.timeout(10) do
        begin
          test_file = File.join File.dirname(__FILE__), 'executor_quits.rb'
          pid       = spawn RbConfig.ruby, test_file
          Process.waitpid pid
          expect($?.success?).to eq true
        rescue Errno::ECHILD
          # child already gone
        rescue Timeout::Error => e
          Process.kill :KILL, pid
          raise e
        end
      end
    end

  end

  context '#running?' do

    it 'returns true when the thread pool is running' do
      expect(subject).to be_running
    end

    it 'returns false when the thread pool is shutting down' do
      subject.post{ sleep(0.5) }
      subject.shutdown
      expect(subject).not_to be_running
      expect(subject.wait_for_termination(pool_termination_timeout)).to eq true
    end

    it 'returns false when the thread pool is shutdown' do
      subject.shutdown
      expect(subject.wait_for_termination(pool_termination_timeout)).to eq true
      expect(subject).not_to be_running
    end

    it 'returns false when the thread pool is killed' do
      subject.kill
      expect(subject.wait_for_termination(pool_termination_timeout)).to eq true
      expect(subject).not_to be_running
    end
  end

  context '#shutdown' do

    it 'stops accepting new tasks' do
      latch1 = Concurrent::CountDownLatch.new(1)
      latch2 = Concurrent::CountDownLatch.new(1)
      subject.post{ sleep(0.1); latch1.count_down }
      latch1.wait(1)
      subject.shutdown
      expect(subject.wait_for_termination(pool_termination_timeout)).to eq true
      begin
        subject.post{ latch2.count_down }
      rescue Concurrent::RejectedExecutionError
      end
      expect(latch2.wait(0.2)).to be_falsey
    end

    it 'allows in-progress tasks to complete' do
      latch = Concurrent::CountDownLatch.new(1)
      subject.post{ sleep(0.1); latch.count_down }
      subject.shutdown
      expect(latch.wait(1)).to be_truthy
    end

    it 'allows pending tasks to complete' do
      latch = Concurrent::CountDownLatch.new(2)
      subject.post{ sleep(0.2); latch.count_down }
      subject.post{ sleep(0.2); latch.count_down }
      subject.shutdown
      expect(latch.wait(1)).to be_truthy
    end
  end

  context '#shutdown followed by #wait_for_termination' do

    it 'allows in-progress tasks to complete' do
      latch = Concurrent::CountDownLatch.new(1)
      subject.post{ sleep(0.1); latch.count_down }
      subject.shutdown
      expect(subject.wait_for_termination(pool_termination_timeout)).to eq true
      expect(latch.wait(1)).to be_truthy
    end

    it 'allows pending tasks to complete' do
      q = Queue.new
      5.times do |i|
        subject.post { sleep 0.1; q << i }
      end
      subject.shutdown
      expect(subject.wait_for_termination(pool_termination_timeout)).to eq true
      expect(q.length).to eq 5
    end

    it 'stops accepting/running new tasks' do
      expected = Concurrent::AtomicFixnum.new(0)
      subject.post{ sleep(0.1); expected.increment }
      subject.post{ sleep(0.1); expected.increment }
      subject.shutdown
      begin
        subject.post{ expected.increment }
      rescue Concurrent::RejectedExecutionError
      end
      expect(subject.wait_for_termination(pool_termination_timeout)).to eq true
      expect(expected.value).to eq(2)
    end
  end

  context '#kill' do

    it 'stops accepting new tasks' do
      expected = Concurrent::AtomicBoolean.new(false)
      latch = Concurrent::CountDownLatch.new(1)
      subject.post{ sleep(0.1); latch.count_down }
      latch.wait(1)
      subject.kill
      begin
        subject.post{ expected.make_true }
      rescue Concurrent::RejectedExecutionError
      end
      sleep(0.1)
      expect(expected.value).to be_falsey
    end

    it 'rejects all pending tasks' do
      subject.post{ sleep(1) }
      sleep(0.1)
      subject.kill
      sleep(0.1)
      begin
        expect(subject.post{ nil }).to be_falsey
      rescue Concurrent::RejectedExecutionError
      end
    end
  end

  context '#wait_for_termination' do

    it 'immediately returns true when no operations are pending' do
      subject.shutdown
      expect(subject.wait_for_termination(0)).to be_truthy
    end

    it 'returns true after shutdown has complete' do
      10.times { subject << proc{ nil } }
      sleep(0.1)
      subject.shutdown
      expect(subject.wait_for_termination(pool_termination_timeout)).to be_truthy
    end

    it 'returns true when shutdown successfully completes before timeout' do
      subject.post{ sleep(0.5) }
      sleep(0.1)
      subject.shutdown
      expect(subject.wait_for_termination(pool_termination_timeout)).to be_truthy
    end

    it 'returns false when shutdown fails to complete before timeout' do
      unless subject.serialized?
        latch = Concurrent::CountDownLatch.new 1
        100.times{ subject.post{ latch.wait } }
        sleep(0.1)
        subject.shutdown
        expect(subject.wait_for_termination(0.01)).to be_falsey
        latch.count_down
      end
    end

    it 'waits forever when no timeout value is given' do
      subject.post{ sleep(0.5) }
      sleep(0.1)
      subject.shutdown
      expect(subject.wait_for_termination).to be_truthy
    end
  end
end
