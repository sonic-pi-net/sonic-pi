require 'concurrent/async'

module Concurrent
  RSpec.describe Async do

    let(:async_class) do
      Class.new do
        include Concurrent::Async
        attr_accessor :accessor
        def initialize(*args)
        end
        def echo(msg)
          msg
        end
        def gather(first, second = nil)
          return first, second
        end
        def boom(ex = StandardError.new)
          raise ex
        end
        def wait(seconds)
          sleep(seconds)
        end
        def with_block
          yield
        end
      end
    end

    subject do
      async_class.new
    end

    context 'object creation' do

      it 'delegates to the original constructor' do
        args = [:foo, 'bar', 42]
        expect(async_class).to receive(:original_new).once.with(*args).and_call_original
        async_class.new(*args)
      end

      specify 'passes all args to the original constructor' do
        clazz = Class.new do
          include Concurrent::Async
          attr_reader :args
          def initialize(*args)
            @args = args
          end
        end

        object = clazz.new(:foo, :bar)
        expect(object.args).to eq [:foo, :bar]
      end

      specify 'passes a given block to the original constructor' do
        clazz = Class.new do
          include Concurrent::Async
          attr_reader :block
          def initialize(&block)
            @block = yield
          end
        end

        object = clazz.new{ 42 }
        expect(object.block).to eq 42
      end

      specify 'initializes synchronization' do
        mock = async_class.new
        allow(async_class).to receive(:original_new).and_return(mock)
        expect(mock).to receive(:init_synchronization).once.with(no_args)
        async_class.new
      end
    end

    context '#validate_argc' do

      subject do
        Class.new {
          def zero() nil; end
          def three(a, b, c, &block) nil; end
          def two_plus_two(a, b, c=nil, d=nil, &block) nil; end
          def many(*args, &block) nil; end
        }.new
      end

      it 'raises an exception when the method is not defined' do
        expect {
          Async::validate_argc(subject, :bogus)
        }.to raise_error(StandardError)
      end

      it 'raises an exception for too many args on a zero arity method' do
        expect {
          Async::validate_argc(subject, :zero, 1, 2, 3)
        }.to raise_error(ArgumentError)
      end

      it 'does not raise an exception for correct zero arity' do
        expect {
          Async::validate_argc(subject, :zero)
        }.not_to raise_error
      end

      it 'raises an exception for too many args on a method with positive arity' do
        expect {
          Async::validate_argc(subject, :three, 1, 2, 3, 4)
        }.to raise_error(ArgumentError)
      end

      it 'raises an exception for too few args on a method with positive arity' do
        expect {
          Async::validate_argc(subject, :three, 1, 2)
        }.to raise_error(ArgumentError)
      end

      it 'does not raise an exception for correct positive arity' do
        expect {
          Async::validate_argc(subject, :three, 1, 2, 3)
        }.not_to raise_error
      end

      it 'raises an exception for too few args on a method with negative arity' do
        expect {
          Async::validate_argc(subject, :two_plus_two, 1)
        }.to raise_error(ArgumentError)
      end

      it 'does not raise an exception for correct negative arity' do
        expect {
          Async::validate_argc(subject, :two_plus_two, 1, 2)
          Async::validate_argc(subject, :two_plus_two, 1, 2, 3, 4)
          Async::validate_argc(subject, :two_plus_two, 1, 2, 3, 4, 5, 6)

          Async::validate_argc(subject, :many)
          Async::validate_argc(subject, :many, 1, 2)
          Async::validate_argc(subject, :many, 1, 2, 3, 4)
        }.not_to raise_error
      end
    end

    context '#async' do

      it 'raises an error when calling a method that does not exist' do
        expect {
          subject.async.bogus
        }.to raise_error(StandardError)
      end

      it 'raises an error when passing too few arguments' do
        expect {
          subject.async.gather
        }.to raise_error(ArgumentError)
      end

      it 'raises an error when pasing too many arguments (arity >= 0)' do
        expect {
          subject.async.echo(1, 2, 3, 4, 5)
        }.to raise_error(StandardError)
      end

      it 'returns the existence of the method' do
        expect(subject.async.respond_to?(:echo)).to be_truthy
        expect(subject.async.respond_to?(:not_exist_method)).to be_falsy
      end

      it 'returns a :pending IVar' do
        val = subject.async.wait(1)
        expect(val).to be_a Concurrent::IVar
        expect(val).to be_pending
      end

      it 'runs the future on the global executor' do
        expect(Concurrent.global_io_executor).to receive(:post).with(any_args).
          and_call_original
        subject.async.echo(:foo)
      end

      it 'sets the value on success' do
        val = subject.async.echo(:foo)
        expect(val.value).to eq :foo
        expect(val).to be_fulfilled
      end

      it 'sets the reason on failure' do
        ex = ArgumentError.new
        val = subject.async.boom(ex)
        val.wait
        expect(val.reason).to eq ex
        expect(val).to be_rejected
      end

      it 'sets the reason when giving too many optional arguments' do
        val = subject.async.gather(1, 2, 3, 4, 5)
        val.wait
        expect(val.reason).to be_a StandardError
        expect(val).to be_rejected
      end

      it 'supports attribute accessors' do
        subject.async.accessor = :foo
        val = subject.async.accessor
        expect(val.value).to eq :foo
        expect(subject.accessor).to eq :foo
      end

      it 'supports methods with blocks' do
        val = subject.async.with_block{ :foo }
        expect(val.value).to eq :foo
      end
    end

    context '#await' do

      it 'raises an error when calling a method that does not exist' do
        expect {
          subject.await.bogus
        }.to raise_error(StandardError)
      end

      it 'raises an error when passing too few arguments' do
        expect {
          subject.await.gather
        }.to raise_error(ArgumentError)
      end

      it 'raises an error when pasing too many arguments (arity >= 0)' do
        expect {
          subject.await.echo(1, 2, 3, 4, 5)
        }.to raise_error(StandardError)
      end

      it 'returns the existence of the method' do
        expect(subject.await.respond_to?(:echo)).to be_truthy
        expect(subject.await.respond_to?(:not_exist_method)).to be_falsy
      end

      it 'returns a :fulfilled IVar' do
        val = subject.await.echo(5)
        expect(val).to be_a Concurrent::IVar
        expect(val).to be_fulfilled
      end

      it 'runs the future on the global executor' do
        expect(Concurrent.global_io_executor).to receive(:post).with(any_args).
          and_call_original
        subject.await.echo(:foo)
      end

      it 'sets the value on success' do
        val = subject.await.echo(:foo)
        expect(val.value).to eq :foo
        expect(val).to be_fulfilled
      end

      it 'sets the reason on failure' do
        ex = ArgumentError.new
        val = subject.await.boom(ex)
        expect(val.reason).to eq ex
        expect(val).to be_rejected
      end

      it 'sets the reason when giving too many optional arguments' do
        val = subject.await.gather(1, 2, 3, 4, 5)
        expect(val.reason).to be_a StandardError
        expect(val).to be_rejected
      end

      it 'supports attribute accessors' do
        subject.await.accessor = :foo
        val = subject.await.accessor
        expect(val.value).to eq :foo
        expect(subject.accessor).to eq :foo
      end

      it 'supports methods with blocks' do
        val = subject.await.with_block{ :foo }
        expect(val.value).to eq :foo
      end
    end

    context 'locking' do

      it 'uses the same lock for both #async and #await' do
        object = Class.new {
          include Concurrent::Async
          attr_reader :bucket
          def gather(seconds, first, *rest)
            sleep(seconds)
            (@bucket ||= []).concat([first])
            @bucket.concat(rest)
          end
        }.new

        object.async.gather(0.5, :a, :b)
        object.await.gather(0, :c, :d)
        expect(object.bucket).to eq [:a, :b, :c, :d]
      end
    end

    context 'fork safety' do
      it 'does not hang when forked' do
        skip "Platform does not support fork" unless Process.respond_to?(:fork)
        object = Class.new {
          include Concurrent::Async
          def foo; end
        }.new
        object.async.foo
        _, status = Process.waitpid2(fork {object.await.foo})
        expect(status.exitstatus).to eq 0
      end
    end
  end
end
