require_relative 'ivar_shared'
require 'concurrent/ivar'

module Concurrent

  RSpec.describe IVar do

    let!(:value) { 10 }

    let!(:fulfilled_value) { 10 }
    let(:rejected_reason) { StandardError.new('Boom!') }

    subject { IVar.new(value) }

    let(:pending_subject) do
      ivar = IVar.new
      in_thread do
        sleep(0.1)
        ivar.set(fulfilled_value)
      end
      ivar
    end

    let(:fulfilled_subject) do
      IVar.new.set(fulfilled_value)
    end

    let(:rejected_subject) do
      IVar.new.fail(rejected_reason)
    end

    it_should_behave_like :ivar do
      subject{ IVar.new }

      def dereferenceable_subject(value, opts = {})
        IVar.new(value, opts)
      end

      def dereferenceable_observable(opts = {})
        IVar.new(NULL, opts)
      end

      def execute_dereferenceable(subject)
        subject.set('value')
      end

      def trigger_observable(observable)
        observable.set('value')
      end
    end

    context '#initialize' do

      it 'does not have to set an initial value' do
        i = IVar.new
        expect(i).to be_incomplete
      end

      it 'does not set an initial value if you pass NULL' do
        i = IVar.new(NULL)
        expect(i).to be_incomplete
      end

      it 'can set an initial value' do
        i = IVar.new(14)
        expect(i).to be_complete
        expect(i.value).to eq 14
      end

      it 'can set an initial value with a block' do
        i = IVar.new{ 42 }
        expect(i).to be_complete
        expect(i.value).to eq 42
      end

      it 'raises an exception if given both a value and a block' do
        expect {
          IVar.new(42){ 42 }
        }.to raise_error(ArgumentError)
      end
    end

    context 'observation' do

      let(:clazz) do
        Class.new do
          attr_reader :value
          attr_reader :reason
          attr_reader :count
          define_method(:update) do |time, value, reason|
            @count ||= 0
            @count = @count.to_i + 1
            @value = value
            @reason = reason
          end
        end
      end

      let(:observer) { clazz.new }

      it 'notifies all observers on #set' do
        i = IVar.new
        i.add_observer(observer)

        i.set(42)

        expect(observer.value).to eq(42)
        expect(observer.reason).to be_nil
      end

      context 'deadlock avoidance' do

        def reentrant_observer(i)
          obs = ::Object.new
          obs.define_singleton_method(:update) do |time, value, reason|
            @value = i.value
          end
          obs.define_singleton_method(:value) { @value }
          obs
        end

        it 'should notify observers outside mutex lock' do
          i = IVar.new
          obs = reentrant_observer(i)

          i.add_observer(obs)
          i.set(42)

          expect(obs.value).to eq 42
        end

        it 'should notify a new observer added after fulfillment outside lock' do
          i = IVar.new
          i.set(42)
          obs = reentrant_observer(i)

          i.add_observer(obs)

          expect(obs.value).to eq 42
        end
      end
    end
  end
end
