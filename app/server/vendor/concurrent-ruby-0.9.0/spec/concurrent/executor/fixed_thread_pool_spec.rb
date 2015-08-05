require_relative 'thread_pool_shared'

module Concurrent

  describe FixedThreadPool do

    let!(:num_threads){ 5 }
    subject { described_class.new(num_threads) }

    after(:each) do
      subject.kill
      sleep(0.1)
    end

    it_should_behave_like :thread_pool

    context '#initialize default values' do

      subject { described_class.new(5) }

      it 'defaults :min_length correctly' do
        expect(subject.min_length).to eq 5
      end

      it 'defaults :max_length correctly' do
        expect(subject.max_length).to eq 5
      end

      it 'defaults :fallback_policy to :abort' do
        expect(subject.fallback_policy).to eq :abort
      end


      it 'defaults :idletime correctly' do
        expect(subject.idletime).to eq subject.class.const_get(:DEFAULT_THREAD_IDLETIMEOUT)
      end

      it 'defaults default :max_queue to zero' do
        expect(subject.max_queue).to eq 0
      end

    end

    context '#initialize explicit values' do

      it 'raises an exception when the pool length is less than one' do
        expect {
          described_class.new(0)
        }.to raise_error(ArgumentError)
      end


      it 'sets explicit :max_queue correctly' do
        subject = described_class.new(5, :max_queue => 10)
        expect(subject.max_queue).to eq 10
      end

      it 'correctly sets valid :fallback_policy' do
        subject = described_class.new(5, :fallback_policy => :caller_runs)
        expect(subject.fallback_policy).to eq :caller_runs
      end

      it "correctly sets valid :idletime" do
        subject = described_class.new(5, :idletime => 10)
        expect(subject.idletime).to eq 10
      end

      it 'raises an exception if given an invalid :fallback_policy' do
        expect {
          described_class.new(5, fallback_policy: :bogus)
        }.to raise_error(ArgumentError)
      end


    end

    context '#min_length' do

      it 'returns :num_threads on creation' do
        expect(subject.min_length).to eq num_threads
      end

      it 'returns :num_threads while running' do
        10.times{ subject.post{ nil } }
        sleep(0.1)
        expect(subject.min_length).to eq num_threads
      end

      it 'returns :num_threads once shutdown' do
        10.times{ subject.post{ nil } }
        sleep(0.1)
        subject.shutdown
        subject.wait_for_termination(1)
        expect(subject.min_length).to eq num_threads
      end
    end

    context '#max_length' do

      it 'returns :num_threads on creation' do
        expect(subject.max_length).to eq num_threads
      end

      it 'returns :num_threads while running' do
        10.times{ subject.post{ nil } }
        sleep(0.1)
        expect(subject.max_length).to eq num_threads
      end

      it 'returns :num_threads once shutdown' do
        10.times{ subject.post{ nil } }
        sleep(0.1)
        subject.shutdown
        subject.wait_for_termination(1)
        expect(subject.max_length).to eq num_threads
      end
    end

    context '#length' do

      it 'returns :num_threads while running' do
        10.times{ subject.post{ nil } }
        sleep(0.1)
        expect(subject.length).to eq num_threads
      end
    end

    context '#largest_length' do

      it 'returns zero on creation' do
        expect(subject.largest_length).to eq 0
      end

      it 'returns :num_threads while running' do
        10.times{ subject.post{ nil } }
        sleep(0.1)
        expect(subject.largest_length).to eq num_threads
      end

      it 'returns :num_threads once shutdown' do
        10.times{ subject.post{ nil } }
        sleep(0.1)
        subject.shutdown
        subject.wait_for_termination(1)
        expect(subject.largest_length).to eq num_threads
      end
    end

    context '#kill' do

      it 'attempts to kill all in-progress tasks' do
        thread_count = [subject.length, 5].max
        @expected = false
        thread_count.times do
          # kill tries to shutdown first with 1sec timeout, so wait 2sec here
          subject.post { sleep(2) }
        end
        subject.post{ @expected = true }
        sleep(0.1)
        subject.kill
        sleep(0.1)
        expect(@expected).to be_falsey
      end
    end

    context 'worker creation and caching' do

      it 'never creates more than :num_threads threads' do
        pool = described_class.new(5)
        100.times{ pool << proc{ sleep(1) } }
        sleep(0.1)
        expect(pool.length).to eq 5
        pool.kill
      end
    end

    context 'fallback policy' do

      before(:each) do
        @queue = Queue.new
      end

      after(:each) do
        subject.kill
      end

      # On abort, it should raise an error
      it "raises an error when overflow on abort" do
        latch = Concurrent::CountDownLatch.new(5)
        mutex = Mutex.new

        subject = described_class.new(2, :max_queue => 2, :fallback_policy => :abort)
        expect {
          5.times do |i|
            subject.post do
              sleep 0.1
              mutex.synchronize{ @queue << i }
              latch.count_down
            end
          end
          latch.wait(1)
        }.to raise_error
      end

      # On discard, we'd expect no error, but also not all five results
      it 'discards when fallback_policy is :discard' do
        latch = Concurrent::CountDownLatch.new(5)
        mutex = Mutex.new

        subject = described_class.new(2, :max_queue => 2, :fallback_policy => :discard)
        5.times do |i|
          subject.post do
            sleep 0.1
            mutex.synchronize{ @queue << i }
            latch.count_down
          end
        end
        latch.wait(1)

        expect(@queue.length).to be < 5
      end

      # To check for caller_runs, we'll check how many unique threads
      # actually ran the block

      it 'uses the calling thread for overflow under caller_runs' do
        latch = Concurrent::CountDownLatch.new(5)
        mutex = Mutex.new

        subject = described_class.new(2, :max_queue => 2, :fallback_policy => :caller_runs)

        5.times do |i|
          subject.post do
            sleep 0.1
            mutex.synchronize{ @queue << Thread.current }
            latch.count_down
          end
        end
        latch.wait(1)

        # Turn the queue into an array
        a = []
        a << @queue.shift until @queue.empty?

        #NOTE: This test is very, very difficult to setup properly. Hence the 'be_within' matcher
        expect(a.size).to be_within(1).of(5) # one for each run of the block
        expect(a.uniq.size).to be_within(1).of(3) # one for each of the two threads, plus the caller
      end
    end

    context 'runtime-specific implementation' do

      if Concurrent.on_jruby?

        it 'sets :fallback_policy correctly' do
          clazz  = java.util.concurrent.ThreadPoolExecutor::DiscardPolicy
          policy = clazz.new
          expect(clazz).to receive(:new).at_least(:once).with(any_args).and_return(policy)

          subject = FixedThreadPool.new(5, fallback_policy: :discard)
          expect(subject.fallback_policy).to eq :discard
        end

      else

        context 'exception handling' do

          it 'restarts threads that experience exception' do
            count = subject.length
            count.times{ subject << proc{ raise StandardError } }
            sleep(1)
            expect(subject.length).to eq count
          end
        end

        context 'worker creation and caching' do

          it 'creates new workers when there are none available' do
            pool = described_class.new(5)
            expect(pool.length).to eq 0
            5.times{ pool << proc{ sleep(1) } }
            sleep(0.1)
            expect(pool.length).to eq 5
            pool.kill
          end
        end
      end
    end
  end
end
