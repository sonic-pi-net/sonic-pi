if Concurrent.on_jruby?

  require_relative 'thread_pool_executor_shared'

  module Concurrent

    describe JavaThreadPoolExecutor, :type => :jruby do

      after(:each) do
        subject.kill
        sleep(0.1)
      end

      subject do
        JavaThreadPoolExecutor.new(
          min_threads: 2,
          max_threads: 5,
          idletime: 60,
          max_queue: 10,
          fallback_policy: :discard
        )
      end

      it_should_behave_like :thread_pool

      it_should_behave_like :thread_pool_executor

      context '#overload_policy' do

        specify ':abort maps to AbortPolicy' do
          clazz = java.util.concurrent.ThreadPoolExecutor::AbortPolicy
          policy = clazz.new
          expect(clazz).to receive(:new).at_least(:once).with(any_args).and_return(policy)
          JavaThreadPoolExecutor.new(
            min_threads: 2,
            max_threads: 5,
            idletime: 60,
            max_queue: 10,
            fallback_policy: :abort
          )
        end

        specify ':discard maps to DiscardPolicy' do
          clazz = java.util.concurrent.ThreadPoolExecutor::DiscardPolicy
          policy = clazz.new
          expect(clazz).to receive(:new).at_least(:once).with(any_args).and_return(policy)
          JavaThreadPoolExecutor.new(
            min_threads: 2,
            max_threads: 5,
            idletime: 60,
            max_queue: 10,
            fallback_policy: :discard
          )
        end

        specify ':caller_runs maps to CallerRunsPolicy' do
          clazz = java.util.concurrent.ThreadPoolExecutor::CallerRunsPolicy
          policy = clazz.new
          expect(clazz).to receive(:new).at_least(:once).with(any_args).and_return(policy)
          JavaThreadPoolExecutor.new(
            min_threads: 2,
            max_threads: 5,
            idletime: 60,
            max_queue: 10,
            fallback_policy: :caller_runs
          )
        end
      end
    end
  end
end
