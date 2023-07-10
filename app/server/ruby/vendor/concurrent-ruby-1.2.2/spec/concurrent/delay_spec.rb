require 'concurrent/delay'
require_relative 'concern/dereferenceable_shared'
require_relative 'concern/obligation_shared'

module Concurrent

  RSpec.describe Delay do

    context 'behavior' do

      # dereferenceable

      def dereferenceable_subject(value, opts = {})
        delay = Delay.new(opts){ value }
        delay.tap{ delay.value }
      end

      it_should_behave_like :dereferenceable

      # obligation

      let!(:fulfilled_value) { 10 }
      let!(:rejected_reason) { StandardError.new('mojo jojo') }

      let(:pending_subject) do
        Delay.new(executor: :fast){ sleep 0.1; fulfilled_value }
      end

      let(:fulfilled_subject) do
        delay = Delay.new{ fulfilled_value }
        delay.tap{ delay.value }
      end

      let(:rejected_subject) do
        delay = Delay.new{ raise rejected_reason }
        delay.tap{ delay.value }
      end

      it_should_behave_like :obligation
    end

    context '#initialize' do

      it 'sets the state to :pending' do
        expect(Delay.new{ nil }.state).to eq :pending
        expect(Delay.new{ nil }).to be_pending
      end

      it 'raises an exception when no block given' do
        expect {
          Delay.new
        }.to raise_error(ArgumentError)
      end
    end


    context '#reconfigure' do
      it 'returns value of block used in reconfiguration' do
        expect(Delay.new { nil }.tap { |d| d.reconfigure { true } }.value).to be_truthy
      end

      it 'returns false when process completed?' do
        d = Delay.new { 1 }
        expect(d.reconfigure { 2 }).to be_truthy
        expect(d.value).to be 2
        expect(d.reconfigure { 3 }).to be_falsey
      end
    end

    context '#value' do

      let(:task){ proc{ nil } }

      it 'does not call the block before #value is called' do
        expect(task).not_to receive(:call).with(any_args)
        Delay.new(&task)
      end

      it 'calls the block when #value is called' do
        expect(task).to receive(:call).once.with(any_args).and_return(nil)
        Delay.new(&task).value
      end

      it 'only calls the block once no matter how often #value is called' do
        expect(task).to receive(:call).once.with(any_args).and_return(nil)
        delay = Delay.new(&task)
        5.times{ delay.value }
      end

      it 'raises when called recursively' do
        delay = Delay.new { delay.value }
        expect { delay.value! }.to raise_error(IllegalOperationError)
        expect(delay.reason).to be_a_kind_of(IllegalOperationError)
      end

      it 'can be called twice' do
        delay = Delay.new { 10 }
        expect(delay.value).to eq 10
        expect(delay.value).to eq 10
      end
    end
  end
end
