require 'concurrent/concern/obligation'

module Concurrent
  module Concern

    RSpec.describe Obligation do

      let (:obligation_class) do
        Class.new(Synchronization::LockableObject) do
          include Obligation
          public :state=, :compare_and_set_state, :if_state
          attr_writer :value, :reason
          def initialize
            super
            set_deref_options
            init_obligation
          end
        end
      end

      let (:obligation) { obligation_class.new }
      let (:event) { double 'event' }

      RSpec.shared_examples :incomplete do

        it 'should be not completed' do
          expect(obligation).not_to be_complete
        end

        it 'should be incomplete' do
          expect(obligation).to be_incomplete
        end

        methods = [:value, :value!, :no_error!]
        methods.each do |method|
          describe "##{method}" do

            it 'should return immediately if timeout is zero' do
              result = obligation.send(method, 0)
              if method == :no_error!
                expect(result).to eq obligation
              else
                expect(result).to be_nil
              end
            end

            it 'should block on the event if timeout is not set' do
              allow(obligation).to receive(:event).and_return(event)
              expect(event).to receive(:wait).with(nil)

              obligation.send method
            end

            it 'should block on the event if timeout is not zero' do
              allow(obligation).to receive(:event).and_return(event)
              expect(event).to receive(:wait).with(5)

              obligation.send(method, 5)
            end

          end
        end
      end

      context 'unscheduled' do
        before(:each) { obligation.state = :unscheduled }
        it_should_behave_like :incomplete
      end

      context 'pending' do
        before(:each) { obligation.state = :pending }
        it_should_behave_like :incomplete
      end

      context 'fulfilled' do

        before(:each) do
          obligation.send :set_state, true, 42, nil
          allow(obligation).to receive(:event).and_return(event)
        end

        it 'should be completed' do
          expect(obligation).to be_complete
        end

        it 'should be not incomplete' do
          expect(obligation).not_to be_incomplete
        end

        describe '#value' do

          it 'should return immediately if timeout is zero' do
            expect(obligation.value(0)).to eq 42
          end

          it 'should return immediately if timeout is not set' do
            expect(event).not_to receive(:wait)

            expect(obligation.value).to eq 42
          end

          it 'should return immediately if timeout is not zero' do
            expect(event).not_to receive(:wait)

            expect(obligation.value(5)).to eq 42
          end

        end

        describe '#value!' do

          it 'should return immediately if timeout is zero' do
            expect(obligation.value!(0)).to eq 42
          end

          it 'should return immediately if timeout is not set' do
            expect(event).not_to receive(:wait)

            expect(obligation.value!).to eq 42
          end

          it 'should return immediately if timeout is not zero' do
            expect(event).not_to receive(:wait)

            expect(obligation.value!(5)).to eq 42
          end

        end

        describe '#no_error!' do

          it 'should return immediately if timeout is zero' do
            expect(obligation.no_error!(0)).to eq obligation
          end

          it 'should return immediately if timeout is not set' do
            expect(event).not_to receive(:wait)

            expect(obligation.no_error!).to eq obligation
          end

          it 'should return immediately if timeout is not zero' do
            expect(event).not_to receive(:wait)

            expect(obligation.no_error!(5)).to eq obligation
          end

        end

      end

      context 'rejected' do

        before(:each) do
          obligation.send :set_state, false, nil, (raise rescue $!)
          allow(obligation).to receive(:event).and_return(event)
        end

        it 'should be completed' do
          expect(obligation).to be_complete
        end

        it 'should be not incomplete' do
          expect(obligation).not_to be_incomplete
        end


        describe '#value' do

          it 'should return immediately if timeout is zero' do
            expect(event).not_to receive(:wait)

            expect(obligation.value(0)).to be_nil
          end

          it 'should return immediately if timeout is not set' do
            expect(event).not_to receive(:wait)

            expect(obligation.value).to be_nil
          end

          it 'should return immediately if timeout is not zero' do
            expect(event).not_to receive(:wait)

            expect(obligation.value(5)).to be_nil
          end

        end

        describe '#value!' do

          it 'should return immediately if timeout is zero' do
            expect(event).not_to receive(:wait)

            expect { obligation.value!(0) }.to raise_error StandardError
          end

          it 'should return immediately if timeout is not set' do
            expect(event).not_to receive(:wait)

            expect { obligation.value! }.to raise_error StandardError
          end

          it 'should return immediately if timeout is not zero' do
            expect(event).not_to receive(:wait)

            expect { obligation.value!(5) }.to raise_error StandardError
          end

        end

        describe '#no_error!' do

          it 'should return immediately if timeout is zero' do
            expect(event).not_to receive(:wait)

            expect { obligation.no_error!(0) }.to raise_error StandardError
          end

          it 'should return immediately if timeout is not set' do
            expect(event).not_to receive(:wait)

            expect { obligation.no_error! }.to raise_error StandardError
          end

          it 'should return immediately if timeout is not zero' do
            expect(event).not_to receive(:wait)

            expect { obligation.no_error!(5) }.to raise_error StandardError
          end

        end

      end

      describe '#compare_and_set_state' do

        before(:each) { obligation.state = :unscheduled }

        context 'unexpected state' do
          it 'should return false if state is not the expected one' do
            expect(obligation.compare_and_set_state(:pending, :rejected)).to be_falsey
          end

          it 'should not change the state if current is not the expected one' do
            obligation.compare_and_set_state(:pending, :rejected)
            expect(obligation.state).to eq :unscheduled
          end
        end

        context 'expected state' do
          it 'should return true if state is the expected one' do
            expect(obligation.compare_and_set_state(:pending, :unscheduled)).to be_truthy
          end

          it 'should not change the state if current is not the expected one' do
            obligation.compare_and_set_state(:pending, :unscheduled)
            expect(obligation.state).to eq :pending
          end
        end

      end

      describe '#if_state' do

        before(:each) { obligation.state = :unscheduled }

        it 'should raise without block' do
          expect { obligation.if_state(:pending) }.to raise_error(ArgumentError)
        end

        it 'should return false if state is not expected' do
          expect(obligation.if_state(:pending, :rejected) { 42 }).to be_falsey
        end

        it 'should the block value if state is expected' do
          expect(obligation.if_state(:rejected, :unscheduled) { 42 }).to eq 42
        end

        it 'should execute the block within the mutex' do
          expect(obligation).to receive(:synchronize)
          obligation.if_state(:unscheduled) { nil }
        end
      end

      context '#get_arguments_from' do

        it 'returns an empty array when opts is not given' do
          args = obligation.send(:get_arguments_from)
          expect(args).to be_a ::Array
          expect(args).to be_empty
        end

        it 'returns an empty array when opts is an empty hash' do
          args = obligation.send(:get_arguments_from, {})
          expect(args).to be_a ::Array
          expect(args).to be_empty
        end

        it 'returns an empty array when there is no :args key' do
          args = obligation.send(:get_arguments_from, foo: 'bar')
          expect(args).to be_a ::Array
          expect(args).to be_empty
        end

        it 'returns an empty array when the :args key has a nil value' do
          args = obligation.send(:get_arguments_from, args: nil)
          expect(args).to be_a ::Array
          expect(args).to be_empty
        end

        it 'returns a one-element array when the :args key has a non-array value' do
          args = obligation.send(:get_arguments_from, args: 'foo')
          expect(args).to eq ['foo']
        end

        it 'returns an array when when the :args key has an array value' do
          expected = [1, 2, 3, 4]
          args = obligation.send(:get_arguments_from, args: expected)
          expect(args).to eq expected
        end

        it 'returns the given array when the :args key has a complex array value' do
          expected = [(1..10).to_a, (20..30).to_a, (100..110).to_a]
          args = obligation.send(:get_arguments_from, args: expected)
          expect(args).to eq expected
        end
      end
    end
  end
end
