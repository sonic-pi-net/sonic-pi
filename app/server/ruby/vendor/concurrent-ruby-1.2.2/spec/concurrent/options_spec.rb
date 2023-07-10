require 'concurrent/options'

module Concurrent

  RSpec.describe Options do

    context '.executor_from_options' do

      let(:executor) { ImmediateExecutor.new }
      let(:io_executor) { ImmediateExecutor.new }
      let(:fast_executor) { ImmediateExecutor.new }

      it 'returns the given :executor' do
        expect(Options.executor_from_options(executor: executor)).to eq executor
      end

      it 'returns the global io executor when :executor is :io' do
        executor = Options.executor_from_options(executor: :io)
        expect(executor).to eq Concurrent.global_io_executor
      end

      it 'returns the global fast executor when :executor is :fast' do
        executor = Options.executor_from_options(executor: :fast)
        expect(executor).to eq Concurrent.global_fast_executor
      end

      it 'returns an immediate executor when :executor is :immediate' do
        executor = Options.executor_from_options(executor: :immediate)
        expect(executor).to be_a Concurrent::ImmediateExecutor
      end

      it 'raises an exception when :executor is an unrecognized symbol' do
        expect {
          Options.executor_from_options(executor: :bogus)
        }.to raise_error(ArgumentError)
      end
    end
  end
end
