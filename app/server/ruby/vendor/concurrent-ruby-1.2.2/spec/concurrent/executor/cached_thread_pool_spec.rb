require 'concurrent/executor/cached_thread_pool'
require_relative 'thread_pool_shared'

module Concurrent

  RSpec.describe CachedThreadPool do

    subject do
      described_class.new(fallback_policy: :discard)
    end

    after(:each) do
      subject.shutdown
      expect(subject.wait_for_termination(pool_termination_timeout)).to eq true
    end

    it_should_behave_like :thread_pool

    let(:latch) { Concurrent::CountDownLatch.new }

    context '#initialize' do

      subject { described_class.new }

      it 'sets :max_length to DEFAULT_MAX_POOL_SIZE' do
        expect(subject.max_length).to eq described_class::DEFAULT_MAX_POOL_SIZE
      end

      it 'sets :min_length to DEFAULT_MIN_POOL_SIZE' do
        expect(subject.min_length).to eq described_class::DEFAULT_MIN_POOL_SIZE
      end

      it 'sets :idletime to DEFAULT_THREAD_IDLETIMEOUT' do
        expect(subject.idletime).to eq described_class::DEFAULT_THREAD_IDLETIMEOUT
      end

      it 'sets :max_queue to DEFAULT_MAX_QUEUE_SIZE' do
        expect(subject.max_queue).to eq described_class::DEFAULT_MAX_QUEUE_SIZE
      end
    end

    context '#min_length' do

      it 'returns zero on creation' do
        expect(subject.min_length).to eq 0
      end

      it 'returns zero while running' do
        10.times { subject.post { nil } }
        subject.post { latch.count_down }
        latch.wait(0.1)
        expect(subject.min_length).to eq 0
      end

      it 'returns zero once shutdown' do
        10.times { subject.post { nil } }
        subject.post { latch.count_down }
        latch.wait(0.1)
        subject.shutdown
        expect(subject.wait_for_termination(pool_termination_timeout)).to eq true
        expect(subject.min_length).to eq 0
      end
    end

    context '#max_length' do

      it 'returns :max_length on creation' do
        expect(subject.max_length).to eq described_class::DEFAULT_MAX_POOL_SIZE
      end

      it 'returns :max_length while running' do
        10.times { subject.post { nil } }
        subject.post { latch.count_down }
        latch.wait(0.1)
        expect(subject.max_length).to eq described_class::DEFAULT_MAX_POOL_SIZE
      end

      it 'returns :max_length once shutdown' do
        10.times { subject.post { nil } }
        subject.post { latch.count_down }
        latch.wait(0.1)
        subject.shutdown
        expect(subject.wait_for_termination(pool_termination_timeout)).to eq true
        expect(subject.max_length).to eq described_class::DEFAULT_MAX_POOL_SIZE
      end
    end

    context '#largest_length' do

      it 'returns zero on creation' do
        expect(subject.largest_length).to eq 0
      end

      it 'returns a non-zero number once tasks have been received' do
        10.times { subject.post { sleep(0.1) } }
        subject.post { latch.count_down }
        latch.wait(0.1)
        expect(subject.largest_length).to be > 0
      end

      it 'returns a non-zero number after shutdown if tasks have been received' do
        10.times { subject.post { sleep(0.1) } }
        subject.post { latch.count_down }
        latch.wait(0.1)
        subject.shutdown
        expect(subject.wait_for_termination(pool_termination_timeout)).to eq true
        expect(subject.largest_length).to be > 0
      end
    end

    context '#idletime' do

      subject { described_class.new(idletime: 42) }

      it 'returns the thread idletime' do
        expect(subject.idletime).to eq 42
      end
    end

    context 'runtime-specific implementation' do

      if Concurrent.on_jruby?

        context '#initialize' do

          it 'sets :fallback_policy correctly' do
            clazz  = java.util.concurrent.ThreadPoolExecutor::DiscardPolicy
            policy = clazz.new
            expect(clazz).to receive(:new).at_least(:once).with(any_args).and_return(policy)

            subject = CachedThreadPool.new(fallback_policy: :discard)
            expect(subject.fallback_policy).to eq :discard
            subject.shutdown
            expect(subject.wait_for_termination(pool_termination_timeout)).to eq true
          end

          it 'defaults :fallback_policy to :abort' do
            subject = CachedThreadPool.new
            expect(subject.fallback_policy).to eq :abort
            subject.shutdown
            expect(subject.wait_for_termination(pool_termination_timeout)).to eq true
          end

          it 'raises an exception if given an invalid :fallback_policy' do
            expect {
              CachedThreadPool.new(fallback_policy: :bogus)
            }.to raise_error(ArgumentError)
          end
        end

      else

        context 'garbage collection' do

          subject { described_class.new(idletime: 0.1, max_threads: 2, gc_interval: 0) }

          it 'removes from pool any thread that has been idle too long' do
            latch = Concurrent::CountDownLatch.new(4)
            4.times { subject.post { sleep 0.1; latch.count_down } }
            expect(latch.wait(1)).to be true
            sleep 0.2
            subject.post {}
            sleep 0.2
            expect(subject.length).to be < 4
          end

          it 'deals with dead threads' do
            expect(subject).to receive(:ns_worker_died).exactly(5).times.and_call_original

            dead_threads_queue = Queue.new
            5.times { subject.post { sleep 0.1; dead_threads_queue.push Thread.current; raise Exception } }
            sleep(0.2)
            latch = Concurrent::CountDownLatch.new(5)
            5.times { subject.post { sleep 0.1; latch.count_down } }
            expect(latch.wait(1)).to be true

            dead_threads = []
            dead_threads << dead_threads_queue.pop until dead_threads_queue.empty?
            expect(dead_threads.all? { |t| !t.alive? }).to be true
          end
        end

        context 'worker creation and caching' do

          subject { described_class.new(idletime: 1, max_threads: 5) }

          it 'creates new workers when there are none available' do
            expect(subject.length).to eq 0
            5.times { sleep(0.1); subject << proc { sleep(1) } }
            sleep(1)
            expect(subject.length).to eq 5
          end

          it 'uses existing idle threads' do
            5.times { subject << proc { sleep(0.1) } }
            sleep(1)
            expect(subject.length).to be >= 5
            3.times { subject << proc { sleep(1) } }
            sleep(0.1)
            expect(subject.length).to be >= 5
          end
        end
      end

      context 'stress' do
        configurations = [
            { min_threads:    2,
              max_threads:    ThreadPoolExecutor::DEFAULT_MAX_POOL_SIZE,
              idletime:       0.1, # 1 minute
              max_queue: 0, # unlimited
              fallback_policy: :caller_runs, # shouldn't matter -- 0 max queue
              gc_interval: 0.1 },
            { min_threads:    2,
              max_threads:    4,
              idletime:       0.1, # 1 minute
              max_queue: 0, # unlimited
              fallback_policy: :caller_runs, # shouldn't matter -- 0 max queue
              gc_interval: 0.1 }
        ]

        configurations.each do |config|
          specify do
            pool = RubyThreadPoolExecutor.new(config)

            10.times do
              count = Concurrent::CountDownLatch.new(100)
              100.times do
                pool.post { count.count_down }
              end
              count.wait
              sleep 0.01 # let the tasks end after count_down
              expect(pool.length).to be <= [200, config[:max_threads]].min
              if pool.length > [110, config[:max_threads]].min
                puts "ERRORSIZE #{pool.length} max #{config[:max_threads]}"
              end
            end

            pool.shutdown
          end
        end
      end
    end
  end
end
