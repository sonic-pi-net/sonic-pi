require_relative 'thread_pool_shared'

shared_examples :thread_pool_executor do

  after(:each) do
    subject.kill
    subject.wait_for_termination(0.1)
  end

  context '#initialize defaults' do

    subject { described_class.new }

    it 'defaults :min_length to DEFAULT_MIN_POOL_SIZE' do
      expect(subject.min_length).to eq described_class::DEFAULT_MIN_POOL_SIZE
    end


    it 'defaults :max_length to DEFAULT_MAX_POOL_SIZE' do
      expect(subject.max_length).to eq described_class::DEFAULT_MAX_POOL_SIZE
    end

    it 'defaults :idletime to DEFAULT_THREAD_IDLETIMEOUT' do
      expect(subject.idletime).to eq described_class::DEFAULT_THREAD_IDLETIMEOUT
    end

    it 'defaults :max_queue to DEFAULT_MAX_QUEUE_SIZE' do
      expect(subject.max_queue).to eq described_class::DEFAULT_MAX_QUEUE_SIZE
    end

    it 'defaults :fallback_policy to :abort' do
      expect(subject.fallback_policy).to eq :abort
    end
  end

  context "#initialize explicit values" do

    it "sets :min_threads" do
      expect(described_class.new(min_threads: 2).min_length).to eq 2
    end

    it "sets :max_threads" do
      expect(described_class.new(max_threads: 2).max_length).to eq 2
    end

    it "sets :idletime" do
      expect(described_class.new(idletime: 2).idletime).to eq 2
    end

    it "doesn't allow max_threads < min_threads" do
      expect {
        described_class.new(min_threads: 2, max_threads: 1)
      }.to raise_error(ArgumentError)
    end

    it 'accepts all valid fallback policies' do
      Concurrent::RubyThreadPoolExecutor::FALLBACK_POLICIES.each do |policy|
        subject = described_class.new(fallback_policy: policy)
        expect(subject.fallback_policy).to eq policy
      end
    end

    it 'raises an exception if :max_threads is less than zero' do
      expect {
        described_class.new(max_threads: -1)
      }.to raise_error(ArgumentError)
    end

    it 'raises an exception if :min_threads is less than zero' do
      expect {
        described_class.new(min_threads: -1)
      }.to raise_error(ArgumentError)
    end

    it 'raises an exception if :max_threads greater than the max allowable' do
      expect {
        described_class.new(max_threads: described_class::DEFAULT_MAX_POOL_SIZE+1)
      }.to raise_error(ArgumentError)
    end

    it 'raises an exception if :max_threads is less than :min_threads' do
      expect {
        described_class.new(max_threads: 1, min_threads: 100)
      }.to raise_error(ArgumentError)
    end

    it 'raises an exception if given an invalid :fallback_policy' do
      expect {
        described_class.new(fallback_policy: :bogus)
      }.to raise_error(ArgumentError)
    end
  end

  context '#max_queue' do

    let!(:expected_max){ 100 }
    subject{ described_class.new(max_queue: expected_max) }

    it 'returns the set value on creation' do
      expect(subject.max_queue).to eq expected_max
    end

    it 'returns the set value when running' do
      trigger = Concurrent::Event.new
      5.times{ subject.post{ trigger.wait } }
      expect(subject.max_queue).to eq expected_max
      trigger.set
    end

    it 'returns the set value after stopping' do
      5.times{ subject.post{ nil } }
      subject.shutdown
      subject.wait_for_termination(1)
      expect(subject.max_queue).to eq expected_max
    end
  end

  context '#queue_length' do

    let!(:expected_max){ 10 }
    subject do
      described_class.new(
        min_threads: 2,
        max_threads: 5,
        max_queue: expected_max,
        fallback_policy: :discard
      )
    end

    it 'returns zero on creation' do
      expect(subject.queue_length).to eq 0
    end

    it 'returns zero when there are no enqueued tasks' do
      latch = Concurrent::CountDownLatch.new(5)
      5.times{ subject.post{ latch.count_down } }
      latch.wait(0.1)
      expect(subject.queue_length).to eq 0
    end

    it 'returns the size of the queue when tasks are enqueued' do
      trigger = Concurrent::Event.new
      20.times{ subject.post{ trigger.wait } }
      expect(subject.queue_length).to be > 0
      trigger.set
    end

    it 'returns zero when stopped' do
      trigger = Concurrent::Event.new
      20.times{ subject.post{ trigger.wait } }
      subject.shutdown
      trigger.set
      subject.wait_for_termination(1)
      expect(subject.queue_length).to eq 0
    end

    it 'can never be greater than :max_queue' do
      trigger = Concurrent::Event.new
      20.times{ subject.post{ trigger.wait } }
      expect(subject.queue_length).to be <= expected_max
      trigger.set
    end
  end

  context '#remaining_capacity' do

    let!(:expected_max){ 100 }
    subject{ described_class.new(max_queue: expected_max) }

    it 'returns -1 when :max_queue is set to zero' do
      executor = described_class.new(max_queue: 0)
      expect(executor.remaining_capacity).to eq -1
    end

    it 'returns :max_length on creation' do
      expect(subject.remaining_capacity).to eq expected_max
    end

    it 'returns :max_length when stopped' do
      100.times{ subject.post{ nil } }
      subject.shutdown
      subject.wait_for_termination(1)
      expect(subject.remaining_capacity).to eq expected_max
    end
  end

  context '#fallback_policy' do

    let!(:min_threads){ 1 }
    let!(:max_threads){ 1 }
    let!(:idletime){ 60 }
    let!(:max_queue){ 1 }

    context ':abort' do

      subject do
        described_class.new(
          min_threads: min_threads,
          max_threads: max_threads,
          idletime: idletime,
          max_queue: max_queue,
          fallback_policy: :abort
        )
      end

      specify '#post raises an error when the queue is at capacity' do
        trigger = Concurrent::Event.new
        expect {
          20.times{ subject.post{ trigger.wait } }
        }.to raise_error(Concurrent::RejectedExecutionError)
        trigger.set
      end

      specify '#<< raises an error when the queue is at capacity' do
        trigger = Concurrent::Event.new
        expect {
          20.times{ subject << proc { trigger.wait } }
        }.to raise_error(Concurrent::RejectedExecutionError)
        trigger.set
      end

      specify '#post raises an error when the executor is shutting down' do
        trigger = Concurrent::Event.new
        expect {
          subject.shutdown; subject.post{ trigger.wait }
        }.to raise_error(Concurrent::RejectedExecutionError)
        trigger.set
      end

      specify '#<< raises an error when the executor is shutting down' do
        trigger = Concurrent::Event.new
        expect {
          subject.shutdown; subject << proc { trigger.wait }
        }.to raise_error(Concurrent::RejectedExecutionError)
        trigger.set
      end

      specify 'a #post task is never executed when the queue is at capacity' do
        all_tasks_posted = Concurrent::Event.new

        latch = Concurrent::CountDownLatch.new(max_threads)

        initial_executed = Concurrent::AtomicFixnum.new(0)
        subsequent_executed = Concurrent::AtomicFixnum.new(0)

        # Fill up all the threads (with a task that won't complete until
        # all tasks are posted)
        max_threads.times do
          subject.post{ latch.count_down; all_tasks_posted.wait ; initial_executed.increment;}
        end

        # Wait for all those tasks to be taken off the queue onto a
        # worker thread and start executing
        latch.wait

        # Fill up the queue (with a task that won't complete until
        # all tasks are posted)
        max_queue.times do
          subject.post{ all_tasks_posted.wait; initial_executed.increment; }
        end

        # Inject 20 more tasks, which should throw an exception
        20.times do
          expect {
            subject.post { subsequent_executed.increment; }
          }.to raise_error(Concurrent::RejectedExecutionError)
        end

        # Trigger the event, so that the tasks in the threads and on
        # the queue can run to completion
        all_tasks_posted.set

        # Wait for all tasks to finish
        subject.shutdown
        subject.wait_for_termination

        # The tasks should have run until all the threads and the
        # queue filled up...
        expect(initial_executed.value).to be (max_threads + max_queue)

        # ..but been dropped after that
        expect(subsequent_executed.value).to be 0
      end

      specify 'a #<< task is never executed when the queue is at capacity' do
        all_tasks_posted = Concurrent::Event.new

        latch = Concurrent::CountDownLatch.new(max_threads)

        initial_executed = Concurrent::AtomicFixnum.new(0)
        subsequent_executed = Concurrent::AtomicFixnum.new(0)

        # Fill up all the threads (with a task that won't complete until
        # all tasks are posted)
        max_threads.times do
          subject << proc { latch.count_down; all_tasks_posted.wait ; initial_executed.increment;}
        end

        # Wait for all those tasks to be taken off the queue onto a
        # worker thread and start executing
        latch.wait

        # Fill up the queue (with a task that won't complete until
        # all tasks are posted)
        max_queue.times do
          subject << proc { all_tasks_posted.wait; initial_executed.increment; }
        end

        # Inject 20 more tasks, which should throw an exeption
        20.times do
          expect {
            subject << proc { subsequent_executed.increment; }
          }.to raise_error(Concurrent::RejectedExecutionError)
        end

        # Trigger the event, so that the tasks in the threads and on
        # the queue can run to completion
        all_tasks_posted.set

        # Wait for all tasks to finish
        subject.shutdown
        subject.wait_for_termination

        # The tasks should have run until all the threads and the
        # queue filled up...
        expect(initial_executed.value).to be (max_threads + max_queue)

        # ..but been rejected after that
        expect(subsequent_executed.value).to be 0
      end
    end

    context ':discard' do

      subject do
        described_class.new(
          min_threads: min_threads,
          max_threads: max_threads,
          idletime: idletime,
          max_queue: max_queue,
          fallback_policy: :discard
        )
      end

      specify 'a #post task is never executed when the queue is at capacity' do
        all_tasks_posted = Concurrent::Event.new

        latch = Concurrent::CountDownLatch.new(max_threads)

        initial_executed = Concurrent::AtomicFixnum.new(0)
        subsequent_executed = Concurrent::AtomicFixnum.new(0)

        # Fill up all the threads (with a task that won't complete until
        # all tasks are posted)
        max_threads.times do
          subject.post{ latch.count_down; all_tasks_posted.wait ; initial_executed.increment;}
        end

        # Wait for all those tasks to be taken off the queue onto a
        # worker thread and start executing
        latch.wait

        # Fill up the queue (with a task that won't complete until
        # all tasks are posted)
        max_queue.times do
          subject.post{ all_tasks_posted.wait; initial_executed.increment; }
        end

        # Inject 20 more tasks, which should be dropped without an exception
        20.times do
          subject.post{ subsequent_executed.increment; }
        end

        # Trigger the event, so that the tasks in the threads and on
        # the queue can run to completion
        all_tasks_posted.set

        # Wait for all tasks to finish
        subject.shutdown
        subject.wait_for_termination

        # The tasks should have run until all the threads and the
        # queue filled up...
        expect(initial_executed.value).to be (max_threads + max_queue)

        # ..but been dropped after that
        expect(subsequent_executed.value).to be 0
      end

      specify 'a #<< task is never executed when the queue is at capacity' do
        all_tasks_posted = Concurrent::Event.new

        latch = Concurrent::CountDownLatch.new(max_threads)

        initial_executed = Concurrent::AtomicFixnum.new(0)
        subsequent_executed = Concurrent::AtomicFixnum.new(0)

        # Fill up all the threads (with a task that won't complete until
        # all tasks are posted)
        max_threads.times do
          subject << proc { latch.count_down; all_tasks_posted.wait ; initial_executed.increment;}
        end

        # Wait for all those tasks to be taken off the queue onto a
        # worker thread and start executing
        latch.wait

        # Fill up the queue (with a task that won't complete until
        # all tasks are posted)
        max_queue.times do
          subject << proc { all_tasks_posted.wait; initial_executed.increment; }
        end

        # Inject 20 more tasks, which should be dropped without an exception
        20.times do
          subject << proc { subsequent_executed.increment; }
        end

        # Trigger the event, so that the tasks in the threads and on
        # the queue can run to completion
        all_tasks_posted.set

        # Wait for all tasks to finish
        subject.shutdown
        subject.wait_for_termination

        # The tasks should have run until all the threads and the
        # queue filled up...
        expect(initial_executed.value).to be (max_threads + max_queue)

        # ..but been dropped after that
        expect(subsequent_executed.value).to be 0
      end

      specify 'a #post task is never executed when the executor is shutting down' do
        executed = Concurrent::AtomicFixnum.new(0)

        subject.shutdown
        subject.post{ executed.increment }

        # Wait for all tasks to finish
        subject.wait_for_termination

        expect(executed.value).to be 0
      end

      specify 'a #<< task is never executed when the executor is shutting down' do
        executed = Concurrent::AtomicFixnum.new(0)

        subject.shutdown
        subject << proc { executed.increment }

        # Wait for all tasks to finish
        subject.wait_for_termination

        expect(executed.value).to be 0
      end

      specify '#post returns false when the executor is shutting down' do
        subject.shutdown
        ret = subject.post{ nil }
        expect(ret).to be false
      end
    end

    context ':caller_runs' do

      subject do
        described_class.new(
          min_threads: 1,
          max_threads: 1,
          idletime: idletime,
          max_queue: 1,
          fallback_policy: :caller_runs
        )
      end

      specify '#post does not create any new threads when the queue is at capacity' do
        trigger = Concurrent::Event.new
        initial = Thread.list.length

        # Post several tasks to the executor. Has to be a new thread,
        # because it will start blocking once the queue fills up.
        Thread.new do
          5.times{ subject.post{ trigger.wait } }
        end

        expect(Thread.list.length).to be < initial + 1 + 5

        # Let the executor tasks complete.
        trigger.set
      end

      specify '#<< executes the task on the current thread when the queue is at capacity' do
        trigger = Concurrent::Event.new
        latch = Concurrent::CountDownLatch.new(5)
        subject.post{ trigger.wait }
        5.times{|i| subject << proc { latch.count_down } }
        latch.wait(0.1)
        trigger.set
      end

      specify '#post executes the task on the current thread when the queue is at capacity' do
        trigger = Concurrent::Event.new
        latch = Concurrent::CountDownLatch.new(5)
        subject.post{ trigger.wait }
        5.times{|i| subject.post{ latch.count_down } }
        latch.wait(0.1)
        trigger.set
      end

      specify '#post executes the task on the current thread when the executor is shutting down' do
        latch = Concurrent::CountDownLatch.new(1)
        subject.shutdown
        subject.post{ latch.count_down }
        latch.wait(0.1)
      end

      specify '#<< executes the task on the current thread when the executor is shutting down' do
        latch = Concurrent::CountDownLatch.new(1)
        subject.shutdown
        subject << proc { latch.count_down }
        latch.wait(0.1)
      end
    end
  end

  context '#overflow_policy' do
    context ':caller_runs is honoured even if the old overflow_policy arg is used' do

      subject do
        described_class.new(
          min_threads: 1,
          max_threads: 1,
          idletime: 60,
          max_queue: 1,
          overflow_policy: :caller_runs
        )
      end

      specify '#<< executes the task on the current thread when the executor is shutting down' do
        latch = Concurrent::CountDownLatch.new(1)
        subject.shutdown
        subject << proc { latch.count_down }
        latch.wait(0.1)
      end
    end
  end
end
