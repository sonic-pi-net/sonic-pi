require 'concurrent/dataflow'
require 'concurrent/executor/simple_executor_service'

module Concurrent

  RSpec.describe 'dataflow' do

    let(:executor) { ImmediateExecutor.new }
    let(:root_executor) { SimpleExecutorService.new }

    it 'raises an exception when no block given' do
      expect { Concurrent::dataflow }.to raise_error(ArgumentError)
      expect { Concurrent::dataflow_with(root_executor) }.to raise_error(ArgumentError)
    end

    specify '#dataflow uses the global fast executor' do
      input = Future.execute{0}
      expect(Concurrent).to receive(:dataflow_with).once.
        with(Concurrent.global_io_executor, input)
      Concurrent::dataflow(input){0}
    end

    specify '#dataflow_with uses the given executor' do
      skip('flaky on truffleruby') if Concurrent.on_truffleruby?

      input = Future.execute{0}
      result = Future.new{0}

      expect(Future).to receive(:new).with(executor: root_executor).and_return(result)
      Concurrent::dataflow_with(root_executor, input){0}
    end

    specify '#dataflow_with raises an exception when no executor given' do
      expect {
        Concurrent::dataflow_with(nil){ nil }
      }.to raise_error(ArgumentError)
    end

    it 'accepts zero or more dependencies' do
      Concurrent::dataflow(){0}
      Concurrent::dataflow(Future.execute{0}){0}
      Concurrent::dataflow(Future.execute{0}, Future.execute{0}){0}

      Concurrent::dataflow_with(root_executor, ){0}
      Concurrent::dataflow_with(root_executor, Future.execute{0}){0}
      Concurrent::dataflow_with(root_executor, Future.execute{0}, Future.execute{0}){0}
    end

    it 'accepts uncompleted dependencies' do
      d = Future.new(executor: executor){0}
      Concurrent::dataflow(d){0}
      d.execute

      d = Future.new(executor: executor){0}
      Concurrent::dataflow_with(root_executor, d){0}
      d.execute
    end

    it 'accepts completed dependencies' do
      d = Future.new(executor: executor){0}
      d.execute
      Concurrent::dataflow(d){0}

      d = Future.new(executor: executor){0}
      d.execute
      Concurrent::dataflow_with(root_executor, d){0}
    end

    it 'raises an exception if any dependencies are not IVars' do
      expect { Concurrent::dataflow(nil) }.to raise_error(ArgumentError)
      expect { Concurrent::dataflow(Future.execute{0}, nil) }.to raise_error(ArgumentError)
      expect { Concurrent::dataflow(nil, Future.execute{0}) }.to raise_error(ArgumentError)

      expect { Concurrent::dataflow_with(root_executor, nil) }.to raise_error(ArgumentError)
      expect { Concurrent::dataflow_with(root_executor, Future.execute{0}, nil) }.to raise_error(ArgumentError)
      expect { Concurrent::dataflow_with(root_executor, nil, Future.execute{0}) }.to raise_error(ArgumentError)
    end

    it 'doesn\'t raise exceptions from dependencies, unless called with !' do
      d1 = Concurrent::dataflow { raise 'd1 error' }
      d2 = Concurrent::dataflow { raise 'd2 error' }
      f = Concurrent::dataflow!(d1, d2) { |d1v, d2v| [d1v, d2v] }
      expect { f.value! }.to raise_error(RuntimeError).with_message('d1 error')

      d1 = Concurrent::dataflow { raise 'd1 error' }
      d2 = Concurrent::dataflow { raise 'd2 error' }
      f = Concurrent::dataflow(d1, d2) { |d1v, d2v| [d1v, d2v] }
      expect { f.value! }.to_not raise_error
    end

    it 'returns a Future' do
      expect(Concurrent::dataflow{0}).to be_a(Future)
      expect(Concurrent::dataflow{0}).to be_a(Future)
    end

    context 'does not schedule the Future' do

      specify 'if no dependencies are completed' do
        d = Future.new(executor: executor){0}
        f = Concurrent::dataflow(d){0}
        expect(f).to be_unscheduled
        d.execute

        d = Future.new(executor: executor){0}
        f = Concurrent::dataflow_with(root_executor, d){0}
        expect(f).to be_unscheduled
        d.execute
      end

      specify 'if one dependency of two is completed' do
        d1 = Future.new(executor: executor){0}
        d2 = Future.new(executor: executor){0}
        f = Concurrent::dataflow(d1, d2){0}
        d1.execute
        expect(f).to be_unscheduled
        d2.execute

        d1 = Future.new(executor: executor){0}
        d2 = Future.new(executor: executor){0}
        f = Concurrent::dataflow_with(root_executor, d1, d2){0}
        d1.execute
        expect(f).to be_unscheduled
        d2.execute
      end
    end

    context 'schedules the Future when all dependencies are available' do

      specify 'if there is just one' do
        d = Future.new(executor: executor){0}
        f = Concurrent::dataflow(d){0}
        d.execute
        expect(f.value).to eq 0

        d = Future.new(executor: executor){0}
        f = Concurrent::dataflow_with(root_executor, d){0}
        d.execute
        expect(f.value).to eq 0
      end

      specify 'if there is more than one' do
        d1 = Future.new(executor: executor){0}
        d2 = Future.new(executor: executor){0}
        f = Concurrent::dataflow(d1, d2){0}
        d1.execute
        d2.execute
        expect(f.value).to eq 0

        d1 = Future.new(executor: executor){0}
        d2 = Future.new(executor: executor){0}
        f = Concurrent::dataflow_with(root_executor, d1, d2){0}
        d1.execute
        d2.execute
        expect(f.value).to eq 0
      end
    end

    context 'counts already executed dependencies' do

      specify 'if there is just one' do
        d = Future.new(executor: executor){0}
        d.execute
        f = Concurrent::dataflow(d){0}
        expect(f.value).to eq 0

        d = Future.new(executor: executor){0}
        d.execute
        f = Concurrent::dataflow_with(root_executor, d){0}
        expect(f.value).to eq 0
      end

      specify 'if there is more than one' do
        d1 = Future.new(executor: executor){0}
        d2 = Future.new(executor: executor){0}
        d1.execute
        d2.execute
        f = Concurrent::dataflow(d1, d2){0}
        expect(f.value).to eq 0

        d1 = Future.new(executor: executor){0}
        d2 = Future.new(executor: executor){0}
        d1.execute
        d2.execute
        f = Concurrent::dataflow_with(root_executor, d1, d2){0}
        expect(f.value).to eq 0
      end
    end

    context 'passes the values of dependencies into the block' do

      specify 'if there is just one' do
        d = Future.new(executor: executor){14}
        f = Concurrent::dataflow(d){|v| v }
        d.execute
        expect(f.value).to eq 14

        d = Future.new(executor: executor){14}
        f = Concurrent::dataflow_with(root_executor, d){|v| v }
        d.execute
        expect(f.value).to eq 14
      end

      specify 'if there is more than one' do
        d1 = Future.new(executor: executor){14}
        d2 = Future.new(executor: executor){2}
        f = Concurrent::dataflow(d1, d2) {|v1, v2| v1 + v2}
        d1.execute
        d2.execute
        expect(f.value).to eq 16

        d1 = Future.new(executor: executor){14}
        d2 = Future.new(executor: executor){2}
        f = Concurrent::dataflow_with(root_executor, d1, d2) {|v1, v2| v1 + v2}
        d1.execute
        d2.execute
        expect(f.value).to eq 16
      end
    end

    context 'module function' do

      it 'can be called as Concurrent.dataflow and Concurrent.dataflow_with' do

        def fib_with_dot(n)
          if n < 2
            Concurrent.dataflow { n }
          else
            n1 = fib_with_dot(n - 1)
            n2 = fib_with_dot(n - 2)
            Concurrent.dataflow_with(ImmediateExecutor.new, n1, n2) { n1.value + n2.value }
          end
        end

        expected = fib_with_dot(7)
        expect(expected.value).to eq 13
      end

    end
  end
end
