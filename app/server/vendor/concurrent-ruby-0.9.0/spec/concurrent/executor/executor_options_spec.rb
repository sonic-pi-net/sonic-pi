module Concurrent

  describe 'Executor.executor_from_options' do

    let(:executor) { ImmediateExecutor.new }
    let(:io_executor) { ImmediateExecutor.new }
    let(:fast_executor) { ImmediateExecutor.new }

    it 'returns the given :executor' do
      expect(Executor.executor_from_options(executor: executor)).to eq executor
    end

    it 'returns the global io executor when :executor is :io' do
      expect(Concurrent).to receive(:global_io_executor).and_return(:io_executor)
      Executor.executor_from_options(executor: :io)
    end

    it 'returns the global fast executor when :executor is :fast' do
      expect(Concurrent).to receive(:global_fast_executor).and_return(:fast_executor)
      Executor.executor_from_options(executor: :fast)
    end

    it 'returns an immediate executor when :executor is :immediate' do
      executor = Executor.executor_from_options(executor: :immediate)
    end

    it 'raises an exception when :executor is an unrecognized symbol' do
      expect {
        Executor.executor_from_options(executor: :bogus)
      }.to raise_error(ArgumentError)
    end

    it 'returns the global fast executor when :operation is true' do
      expect(Concurrent).to receive(:global_fast_executor).
                                and_return(:fast_executor)
      Executor.executor_from_options(operation: true)
    end

    it 'returns the global io executor when :operation is false' do
      expect(Concurrent).to receive(:global_io_executor).
                                and_return(:io_executor)
      Executor.executor_from_options(operation: false)
    end

    it 'returns the global fast executor when :task is false' do
      expect(Concurrent).to receive(:global_fast_executor).
                                and_return(:fast_executor)
      Executor.executor_from_options(task: false)
    end

    it 'returns the global io executor when :task is true' do
      expect(Concurrent).to receive(:global_io_executor).
                                and_return(:io_executor)
      Executor.executor_from_options(task: true)
    end

    it 'returns nil when :executor is nil' do
      expect(Executor.executor_from_options(executor: nil)).to be_nil
    end

    it 'returns nil when no option is given' do
      expect(Executor.executor_from_options).to be_nil
    end

    specify ':executor overrides :operation' do
      expect(Executor.executor_from_options(executor: executor, operation: true)).
          to eq executor
    end

    specify ':executor overrides :task' do
      expect(Executor.executor_from_options(executor: executor, task: true)).
          to eq executor
    end

    specify ':operation overrides :task' do
      expect(Concurrent).to receive(:global_fast_executor).
                                and_return(:fast_executor)
      Executor.executor_from_options(operation: true, task: true)
    end
  end
end
