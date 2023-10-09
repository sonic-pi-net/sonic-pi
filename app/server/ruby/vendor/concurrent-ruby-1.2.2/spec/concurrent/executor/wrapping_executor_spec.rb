require 'concurrent/executor/wrapping_executor'
require 'concurrent/configuration'

module Concurrent
  RSpec.describe WrappingExecutor do

    let(:wrapping_executor) { WrappingExecutor.new(executor, &wrapper) }
    let(:executor) { Concurrent.global_fast_executor }
    let(:wrapper) { nil }
    let(:args) { { foo: 'bar', baz: 42 } }
    let(:task) { -> (*args) { return nil } }

    subject { wrapping_executor }

    it { is_expected.to be_kind_of(WrappingExecutor) }
    it { is_expected.to respond_to(:post) }
    it { is_expected.to respond_to(:can_overflow?) }
    it { is_expected.to respond_to(:serialized?) }

    describe '#post' do
      context 'with passthrough wrapper' do
        let(:wrapper) { -> (*args, &task) { return *args, task } }

        it {
          expect(executor).to receive(:post).with(args) { |&block| expect(block).to be(task) }
          wrapping_executor.post(args, &task)
        }
      end

      context 'with wrapper modifying args' do
        let(:wrapper) { -> (*args, &task) { return *args, { xyz: 'abc' }, task } }

        it {
          expect(executor).to receive(:post).with(args, { xyz: 'abc' }) { |&block| expect(block).to be(task) }
          wrapping_executor.post(args, &task)
        }
      end

      context 'with wrapper modifying task' do
        let(:wrapper) { -> (*args, &task) { return *args, another_task } }
        let(:another_task) { -> (*args) { return true } }

        it {
          expect(executor).to receive(:post).with(args) { |&block| expect(block).to be(another_task) }
          wrapping_executor.post(args, &task)
        }
      end

    end
  end
end
