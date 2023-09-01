require 'concurrent/edge/cancellation'

RSpec.describe 'Concurrent' do
  describe 'Cancellation', edge: true do
    specify 'basic' do
      cancellation, origin = Concurrent::Cancellation.new
      expect(cancellation.origin).to eq origin
      expect(cancellation.to_s).to match(/Cancellation.*pending/)

      futures1 = ::Array.new(2) do
        Concurrent::Promises.future(cancellation) do |c|
          loop { Thread.pass; break if c.canceled? }
          :done
        end
      end

      futures2 = ::Array.new(2) do
        Concurrent::Promises.future(cancellation) do |c|
          loop { c.check!; Thread.pass } rescue $!
        end
      end

      sleep 0.01
      origin.resolve
      expect(cancellation.to_s).to match(/Cancellation.*canceled/)

      futures1.each do |future|
        expect(future.value!).to eq :done
      end
      futures2.each do |future|
        expect(future.value!).to be_a_kind_of Concurrent::CancelledOperationError
      end
    end

    specify do
      cancellation, origin = Concurrent::Cancellation.new
      origin.resolve
      expect(cancellation.canceled?).to be_truthy

      cancellable_branch = Concurrent::Promises.delay { 1 }
      expect((cancellable_branch | origin).value).to be_nil
      expect(cancellable_branch.resolved?).to be_falsey
    end

    specify do
      _, source = Concurrent::Cancellation.new

      cancellable_branch = Concurrent::Promises.delay { 1 }
      expect(Concurrent::Promises.any_resolved_future(cancellable_branch, source).value).to eq 1
      expect(cancellable_branch.resolved?).to be_truthy
    end

    specify do
      cancellation, origin = Concurrent::Cancellation.new(Concurrent::Promises.resolvable_future)
      origin.resolve false, nil, err = StandardError.new('Cancelled')
      expect(cancellation.canceled?).to be_truthy

      cancellable_branch = Concurrent::Promises.delay { 1 }
      expect((cancellable_branch | origin.to_future).reason).to eq err
      expect(cancellable_branch.resolved?).to be_falsey
    end

    specify do
      head                 = Concurrent::Promises.resolvable_future
      cancellation, origin = Concurrent::Cancellation.new head.then(&:succ)

      futures = ::Array.new(2) do
        Concurrent::Promises.future(cancellation) do |c|
          loop { Thread.pass; break if c.canceled? }
          :done
        end
      end

      head.fulfill 1
      futures.each do |future|
        expect(future.value!).to eq :done
      end
      expect(origin.to_future.value!).to eq 2
    end

    specify '#join' do
      cancellation_a, origin_a = Concurrent::Cancellation.new
      cancellation_b, _        = Concurrent::Cancellation.new
      combined_cancellation    = cancellation_a.join(cancellation_b)

      origin_a.resolve

      expect(cancellation_a.canceled?).to be_truthy
      expect(cancellation_b.canceled?).to be_falsey
      expect(combined_cancellation.canceled?).to be_truthy
    end
  end
end
