require 'concurrent/executor/simple_executor_service'
require 'concurrent/configuration'
require_relative 'executor_service_shared'

module Concurrent

  RSpec.describe SimpleExecutorService do

    subject { SimpleExecutorService.new }

    it_should_behave_like :executor_service

    context '#post' do

      subject { SimpleExecutorService.new }

      it 'creates a new thread for a call without arguments' do
        thread = in_thread{ nil }
        expect(Thread).to receive(:new).with(no_args()).and_return(thread)
        expect(Concurrent.global_fast_executor).not_to receive(:post).with(any_args())
        subject.post{ nil }
      end

      it 'executes a call without arguments' do
        latch = CountDownLatch.new(1)
        subject.post{ latch.count_down }
        expect(latch.wait(1)).to be_truthy
      end

      it 'creates a new thread for a call with arguments' do
        thread = in_thread{ nil }
        expect(Thread).to receive(:new).with(1,2,3).and_return(thread)
        expect(Concurrent.global_fast_executor).not_to receive(:post).with(any_args())
        subject.post(1,2,3){ nil }
      end

      it 'executes a call with one argument' do
        latch = CountDownLatch.new(3)
        subject.post(3){|count| count.times{ latch.count_down } }
        expect(latch.wait(1)).to be_truthy
      end

      it 'executes a call with multiple arguments' do
        latch = CountDownLatch.new(10)
        subject.post(1,2,3,4){|*count| count.reduce(:+).times{ latch.count_down } }
        expect(latch.wait(1)).to be_truthy
      end

      it 'aliases #<<' do
        thread = in_thread{ nil }
        expect(Thread).to receive(:new).with(no_args()).and_return(thread)
        expect(Concurrent.global_fast_executor).not_to receive(:post).with(any_args())
        subject << proc{ nil }
      end
    end

    context 'SimpleExecutorService.post' do

      subject { SimpleExecutorService }

      it 'creates a new thread for a call without arguments' do
        thread = in_thread{ nil }
        expect(Thread).to receive(:new).with(no_args()).and_return(thread)
        expect(Concurrent.global_fast_executor).not_to receive(:post).with(any_args())
        subject.post{ nil }
      end

      it 'executes a call without arguments' do
        latch = CountDownLatch.new(1)
        subject.post{ latch.count_down }
        expect(latch.wait(1)).to be_truthy
      end

      it 'creates a new thread for a call with arguments' do
        thread = in_thread{ nil }
        expect(Thread).to receive(:new).with(1,2,3).and_return(thread)
        expect(Concurrent.global_fast_executor).not_to receive(:post).with(any_args())
        subject.post(1,2,3){ nil }
      end

      it 'executes a call with one argument' do
        latch = CountDownLatch.new(3)
        subject.post(3){|count| count.times{ latch.count_down } }
        expect(latch.wait(1)).to be_truthy
      end

      it 'executes a call with multiple arguments' do
        latch = CountDownLatch.new(10)
        subject.post(1,2,3,4){|*count| count.reduce(:+).times{ latch.count_down } }
        expect(latch.wait(1)).to be_truthy
      end

      it 'aliases #<<' do
        thread = in_thread{ nil }
        expect(Thread).to receive(:new).with(no_args()).and_return(thread)
        expect(Concurrent.global_fast_executor).not_to receive(:post).with(any_args())
        subject << proc{ nil }
      end
    end
  end
end
