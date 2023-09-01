require_relative 'thread_pool_executor_shared'
require 'concurrent/executor/thread_pool_executor'

module Concurrent

  RSpec.describe RubyThreadPoolExecutor, :type=>:mri do

    after(:each) do
      subject.shutdown
      expect(subject.wait_for_termination(pool_termination_timeout)).to eq true
    end

    subject do
      RubyThreadPoolExecutor.new(
        min_threads: 2,
        max_threads: 5,
        idletime: 60,
        max_queue: 10,
        fallback_policy: :discard
      )
    end

    it_should_behave_like :thread_pool

    it_should_behave_like :thread_pool_executor

    context :prune, if: !Concurrent.on_jruby? do # pruning is flaky on JRuby
      subject do
        RubyThreadPoolExecutor.new(idletime: 5, min_threads: 2, max_threads: 10)
      end

      Group = Struct.new :waiting_threads, :threads, :mutex, :cond

      def prepare_thread_group(size)
        cond = ConditionVariable.new
        mutex = Mutex.new
        threads = []
        size.times do
          subject.post do
            mutex.synchronize do
              threads << Thread.current
              cond.wait(mutex)
              threads.delete(Thread.current)
            end
          end
        end
        eventually(mutex: mutex) { expect(threads).to have_attributes(size: size) }
        Group.new(threads, threads.dup, mutex, cond)
      end

      def wakeup_thread_group(group)
        group.cond.broadcast
        eventually(mutex: group.mutex) do
          expect(group.waiting_threads).to have_attributes(size: 0)
        end
      end

      before(:each) do
        @now = Concurrent.monotonic_time
        allow(Concurrent).to receive(:monotonic_time) { @now }

        @group1 = prepare_thread_group(5)
        @group2 = prepare_thread_group(5)
      end

      def eventually(mutex: nil, timeout: 5, &block)
          start = Time.now
          while Time.now - start < timeout
            begin
              if mutex
                mutex.synchronize do
                  return yield
                end
              else
                return yield
              end
            rescue Exception => last_failure
            end
            Thread.pass
          end
          raise last_failure
      end

      it "triggers pruning when posting work if the last prune happened more than gc_interval ago" do
        wakeup_thread_group(@group1)
        @now += 6
        wakeup_thread_group(@group2)
        subject.post { }

        eventually { expect(@group1.threads).to all(have_attributes(status: false)) }
        eventually { expect(@group2.threads).to all(have_attributes(status: 'sleep')) }
      end

      it "does not trigger pruning when posting work if the last prune happened less than gc_interval ago" do
        wakeup_thread_group(@group1)
        @now += 3
        subject.prune_pool
        @now += 3
        wakeup_thread_group(@group2)
        subject.post { }

        eventually { expect(@group1.threads).to all(have_attributes(status: false)) }
        eventually { expect(@group2.threads).to all(have_attributes(status: 'sleep')) }
      end

      it "reclaims threads that have been idle for more than idletime seconds" do
        wakeup_thread_group(@group1)
        @now += 6
        wakeup_thread_group(@group2)
        subject.prune_pool

        eventually { expect(@group1.threads).to all(have_attributes(status: false)) }
        eventually { expect(@group2.threads).to all(have_attributes(status: 'sleep')) }
      end

      it "keeps at least min_length workers" do
        wakeup_thread_group(@group1)
        wakeup_thread_group(@group2)
        @now += 12
        subject.prune_pool
        all_threads = @group1.threads + @group2.threads
        eventually do
          finished_threads = all_threads.find_all { |t| !t.status }
          expect(finished_threads).to have_attributes(size: 8)
        end
      end
    end

    context '#remaining_capacity' do

      let!(:expected_max){ 100 }
      let(:latch) { Concurrent::CountDownLatch.new }

      subject do
        RubyThreadPoolExecutor.new(
          min_threads: 10,
          max_threads: 20,
          idletime: 60,
          max_queue: expected_max,
          fallback_policy: :discard
        )
      end

      it 'returns :max_length when no tasks are enqueued' do
        5.times{ subject.post{ nil } }
        subject.post { latch.count_down }
        latch.wait(0.1)
        expect(subject.remaining_capacity).to eq expected_max
      end

      it 'returns the remaining capacity when tasks are enqueued' do
        block = Concurrent::CountDownLatch.new
        100.times{ subject.post{ block.wait } }
        subject.post { latch.count_down }
        latch.wait(0.1)
        expect(subject.remaining_capacity).to be < expected_max
        block.count_down
      end
    end

    context 'threads naming' do
      subject do
        opts = { min_threads: 2 }
        opts[:name] = pool_name if pool_name
        described_class.new(opts)
      end

      let(:names) do
        require 'concurrent/set'
        Concurrent::Set.new
      end

      before do
        subject.post(names) { |names| names << Thread.current.name }
        subject.post(names) { |names| names << Thread.current.name }
        subject.shutdown
        subject.wait_for_termination(pool_termination_timeout)
        expect(names.size).to eq 2
      end

      context 'without pool name' do
        let(:pool_name) { }
        it 'sets counted name' do
          expect(names.all? { |name| name =~ /^worker-\d+$/ }).to be true
        end
      end

      context 'with pool name' do
        let(:pool_name) { 'MyExecutor' }
        it 'sets counted name' do
          expect(names.all? { |name| name =~ /^MyExecutor-worker-\d+$/ }).to be true
        end
      end
    end
  end
end
