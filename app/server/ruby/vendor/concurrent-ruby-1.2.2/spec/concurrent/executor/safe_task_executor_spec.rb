require 'concurrent/executor/safe_task_executor'

module Concurrent

  RSpec.describe SafeTaskExecutor do

    describe '#execute' do

      context 'happy execution' do

        let(:task) { Proc.new { 42 } }
        subject { SafeTaskExecutor.new(task) }

        it 'should return success' do
          success, _value, _reason = subject.execute
          expect(success).to be_truthy
        end

        it 'should return task value' do
          _success, value, _reason = subject.execute
          expect(value).to eq 42
        end

        it 'should return a nil reason' do
          _success, _value, reason = subject.execute
          expect(reason).to be_nil
        end

        it 'passes all arguments to #execute to the task' do
          expected = nil
          task = proc {|*args| expected = args }
          SafeTaskExecutor.new(task).execute(1, 2, 3)
          expect(expected).to eq [1, 2, 3]
        end

        it 'protectes #execute with a mutex' do
          expect(subject).to receive(:synchronize).with(no_args)
          subject.execute
        end
      end

      context 'failing execution' do

        let(:task) { Proc.new { raise StandardError.new('an error') } }
        subject { SafeTaskExecutor.new(task) }

        it 'should return false success' do
          success, _value, _reason = subject.execute
          expect(success).to be_falsey
        end

        it 'should return a nil value' do
          _success, value, _reason = subject.execute
          expect(value).to be_nil
        end

        it 'should return the reason' do
          _success, _value, reason = subject.execute
          expect(reason).to be_a(StandardError)
          expect(reason.message).to eq 'an error'
        end

        it 'rescues Exception when :rescue_exception is true' do
          task = proc { raise Exception }
          subject = SafeTaskExecutor.new(task, rescue_exception: true)
          expect {
            subject.execute
          }.to_not raise_error
        end

        it 'rescues StandardError when :rescue_exception is false' do
          task = proc { raise StandardError }
          subject = SafeTaskExecutor.new(task, rescue_exception: false)
          expect {
            subject.execute
          }.to_not raise_error

          task = proc { raise Exception }
          subject = SafeTaskExecutor.new(task, rescue_exception: false)
          expect {
            subject.execute
          }.to raise_error(Exception)
        end

        it 'rescues StandardError by default' do
          task = proc { raise StandardError }
          subject = SafeTaskExecutor.new(task)
          expect {
            subject.execute
          }.to_not raise_error

          task = proc { raise Exception }
          subject = SafeTaskExecutor.new(task)
          expect {
            subject.execute
          }.to raise_error(Exception)
        end
      end

      # These tests only make sense on CRuby as they test a workaround for CRuby bugs: https://github.com/ruby-concurrency/concurrent-ruby/issues/931
      if Concurrent.on_cruby? and Concurrent.ruby_version(:<, 3, 2, 0)
        context 'local jump error' do
          def execute
            Thread.new do
              executor = SafeTaskExecutor.new(-> { yield 42 })
              @result = executor.execute
            end.join
          end

          subject do
            to_enum(:execute).first
            @result
          end

          it 'should return success' do
            success, _value, _reason = subject
            expect(success).to be_truthy
          end

          it 'should return a nil value' do
            _success, value, _reason = subject
            expect(value).to be_nil
          end

          it 'should return a nil reason' do
            _success, _value, reason = subject
            expect(reason).to be_nil
          end
        end
      end
    end
  end
end
