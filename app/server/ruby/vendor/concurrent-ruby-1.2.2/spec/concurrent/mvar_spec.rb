require_relative 'concern/dereferenceable_shared'
require 'concurrent/mvar'

module Concurrent

  RSpec.describe MVar do

    context 'behavior' do

      # dereferenceable

      def dereferenceable_subject(value, opts = {})
        MVar.new(value, opts)
      end

      it_should_behave_like :dereferenceable
    end

    describe '#initialize' do

      it 'accepts no initial value' do
        m = MVar.new
        expect(m).to be_empty
      end

      it 'accepts an empty initial value' do
        m = MVar.new(MVar::EMPTY)
        expect(m).to be_empty
      end

      it 'accepts an initial value' do
        m = MVar.new(14)
        expect(m).not_to be_empty
      end

      it 'accepts a nil initial value' do
        m = MVar.new(nil)
        expect(m).not_to be_empty
      end

    end

    describe '#take' do

      it 'sets the MVar to empty' do
        m = MVar.new(14)
        m.take
        expect(m).to be_empty
      end

      it 'returns the value on a full MVar' do
        m = MVar.new(14)
        expect(m.take).to eq 14
      end

      it 'waits for another thread to #put' do
        m = MVar.new

        in_thread {
          sleep(0.1)
          m.put 14
        }

        expect(m.take).to eq 14
      end

      it 'returns TIMEOUT on timeout on an empty MVar' do
        m = MVar.new
        expect(m.take(0.1)).to eq MVar::TIMEOUT
      end

    end

    describe '#borrow' do

      it 'yields current value to the block and puts back value' do
        m = MVar.new(14)
        expect { |b| m.borrow(&b) }.to yield_with_args(14)
        expect(m.take).to eq(14)
      end

      it 'puts back value even if an exception is raised' do
        m = MVar.new(14)
        expect { m.borrow { fail 'boom!' } }.to raise_error('boom!')
        expect(m.take).to eq(14)
      end

      it 'returns the returned value of the block' do
        m = MVar.new(14)
        expect(m.borrow { 2 }).to eq(2)
        expect(m.take).to eq(14)
      end

      it 'returns TIMEOUT on timeout on an empty MVar' do
        m = MVar.new
        expect(m.borrow(0.1) { }).to eq MVar::TIMEOUT
      end

    end


    describe '#put' do

      it 'sets the MVar to be empty' do
        m = MVar.new(14)
        m.take
        expect(m).to be_empty
      end

      it 'sets a new value on an empty MVar' do
        m = MVar.new
        m.put 14
        expect(m.take).to eq 14
      end

      it 'waits for another thread to #take' do
        m = MVar.new(14)

        in_thread {
          sleep(0.1)
          m.take
        }

        expect(m.put(14)).to eq 14
      end

      it 'returns TIMEOUT on timeout on a full MVar' do
        m = MVar.new(14)
        expect(m.put(14, 0.1)).to eq MVar::TIMEOUT
      end

      it 'returns the value' do
        m = MVar.new
        expect(m.put(14)).to eq 14
      end

    end

    describe '#empty?' do

      it 'returns true on an empty MVar' do
        m = MVar.new
        expect(m).to be_empty
      end

      it 'returns false on a full MVar' do
        m = MVar.new(14)
        expect(m).not_to be_empty
      end

    end

    describe '#full?' do

      it 'returns false on an empty MVar' do
        m = MVar.new
        expect(m).not_to be_full
      end

      it 'returns true on a full MVar' do
        m = MVar.new(14)
        expect(m).to be_full
      end

    end

    describe '#modify' do

      it 'raises an exception when no block given' do
        m = MVar.new(14)
        expect { m.modify }.to raise_error(ArgumentError)
      end

      it 'modifies a full MVar' do
        m = MVar.new(14)
        m.modify { |v| v + 2 }
        expect(m.take).to eq 16
      end

      it 'returns the unmodified value' do
        m = MVar.new(14)
        expect(m.modify { |v| v + 2 }).to eq 14
      end

      it 'waits for another thread to #put' do
        m = MVar.new

        in_thread {
          sleep(0.1)
          m.put 14
        }

        expect(m.modify { |v| v + 2 }).to eq 14
      end

      it 'is atomic' do
        m = MVar.new(0)

        # #modify conceptually does #take and #put - but it should be atomic.
        # Check that another #put can't sneak it during the #modify.

        in_thread {
          m.modify do |v|
            sleep(0.5)
            1
          end
        }

        sleep(0.1)
        expect(m.put(2, 0.5)).to eq MVar::TIMEOUT
        expect(m.take).to eq 1
      end

      it 'returns TIMEOUT on timeout on an empty MVar' do
        m = MVar.new
        expect(m.modify(0.1) { |v| v + 2 }).to eq MVar::TIMEOUT
      end

    end

    describe '#try_put!' do

      it 'returns true an empty MVar' do
        m = MVar.new
        expect(m.try_put!(14)).to eq true
      end

      it 'returns false on a full MVar' do
        m = MVar.new(14)
        expect(m.try_put!(14)).to eq false
      end

      it 'sets an empty MVar to be full' do
        m = MVar.new
        m.try_put! 14
        expect(m).to be_full
      end

    end

    describe '#try_take!' do

      it 'returns EMPTY an empty MVar' do
        m = MVar.new
        expect(m.try_take!).to eq MVar::EMPTY
      end

      it 'returns the value on a full MVar' do
        m = MVar.new(14)
        expect(m.try_take!).to eq 14
      end

      it 'sets a full MVar to be empty' do
        m = MVar.new(14)
        m.try_take!
        expect(m).to be_empty
      end

    end

    describe '#set!' do

      it 'sets an empty MVar to be full' do
        m = MVar.new
        m.set! 14
        expect(m).to be_full
      end

      it 'sets a full MVar to be full' do
        m = MVar.new(2)
        m.set! 14
        expect(m).to be_full
        expect(m.take).to eq 14
      end

      it 'returns EMPTY on an empty MVar' do
        m = MVar.new
        expect(m.set!(2)).to eq MVar::EMPTY
      end

      it 'returns the original value on a full MVar' do
        m = MVar.new(14)
        expect(m.set!(2)).to eq 14
      end

    end

    describe '#modify!' do

      it 'raises an exception when no block given' do
        m = MVar.new(14)
        expect { m.modify! }.to raise_error(ArgumentError)
      end

      it 'modifies a full MVar' do
        m = MVar.new(14)
        m.modify! { |v| v + 2 }
        expect(m.take).to eq 16
      end

      it 'modifies an empty MVar' do
        m = MVar.new
        m.modify! { |v| 14 }
        expect(m.take).to eq 14
      end

      it 'can be used to set a full MVar to empty' do
        m = MVar.new(14)
        m.modify! { |v| MVar::EMPTY }
        expect(m).to be_empty
      end

      it 'can be used to set an empty MVar to empty' do
        m = MVar.new
        m.modify! { |v| MVar::EMPTY }
        expect(m).to be_empty
      end

      it 'returns the unmodified value' do
        m = MVar.new(14)
        expect(m.modify! { |v| v + 2 }).to eq 14
      end

    end

    context 'spurious wake ups' do

      let(:m) { MVar.new }

      before(:each) do
        def m.simulate_spurious_wake_up
          @mutex.synchronize do
            @full_condition.broadcast
            @empty_condition.broadcast
          end
        end
      end

      describe '#take' do
        it 'waits for another thread to #put' do
          in_thread { sleep(0.5); m.put 14 }
          in_thread { sleep(0.1); m.simulate_spurious_wake_up }

          expect(m.take).to eq 14
        end

        it 'returns TIMEOUT on timeout on an empty MVar' do
          result = nil
          in_thread { result = m.take(0.3) }
          sleep(0.1)
          in_thread { m.simulate_spurious_wake_up }
          sleep(0.1)
          expect(result).to be_nil
          sleep(0.2)
          expect(result).to eq MVar::TIMEOUT
        end
      end

      describe '#modify' do

        it 'waits for another thread to #put' do
          in_thread { sleep(0.5); m.put 14 }
          in_thread { sleep(0.1); m.simulate_spurious_wake_up }

          expect(m.modify { |v| v + 2 }).to eq 14
        end

        it 'returns TIMEOUT on timeout on an empty MVar' do
          result = nil
          in_thread { result = m.modify(0.3) { |v| v + 2 } }
          sleep(0.1)
          in_thread { m.simulate_spurious_wake_up }
          sleep(0.1)
          expect(result).to be_nil
          sleep(0.2)
          expect(result).to eq MVar::TIMEOUT
        end
      end

      describe '#put' do

        before(:each) { m.put(42) }

        it 'waits for another thread to #take' do
          in_thread { sleep(0.5); m.take }
          in_thread { sleep(0.1); m.simulate_spurious_wake_up }

          expect(m.put(14)).to eq 14
        end

        it 'returns TIMEOUT on timeout on a full MVar' do
          # TODO (pitr-ch 15-Oct-2016): fails on jruby
          result = nil
          in_thread { result = m.put(14, 0.3) }
          sleep(0.1)
          in_thread { m.simulate_spurious_wake_up }
          sleep(0.1)
          expect(result).to be_nil
          sleep(0.2)
          expect(result).to eq MVar::TIMEOUT
        end
      end


    end

  end

end
