module Concurrent

  describe Synchronization do
    describe Synchronization::Object do

      class AClass < Synchronization::Object
        attr_volatile :volatile
        attr_accessor :not_volatile

        def initialize(value = nil)
          super()
          @Final = value
          ns_initialize
          ensure_ivar_visibility!
        end

        def final
          @Final
        end

        def count
          synchronize { @count += 1 }
        end

        def wait(timeout = nil)
          synchronize { ns_wait(timeout) }
        end

        private

        def ns_initialize
          @count = 0
        end
      end

      subject { AClass.new }

      describe '#wait' do

        it 'waiting thread is sleeping' do
          t = Thread.new do
            Thread.abort_on_exception = true
            subject.wait
          end
          sleep 0.1
          expect(t.status).to eq 'sleep'
        end

        it 'sleeping thread can be killed' do
          t = Thread.new do
            Thread.abort_on_exception = true
            subject.wait rescue nil
          end
          sleep 0.1
          t.kill
          sleep 0.1
          expect(t.status).to eq false
          expect(t.alive?).to eq false
        end
      end

      describe '#synchronize' do
        it 'allows only one thread to execute count' do
          threads = 10.times.map { Thread.new(subject) { 100.times { subject.count } } }
          threads.each(&:join)
          expect(subject.count).to eq 1001
        end
      end

      describe 'signaling' do
        pending 'for now pending, tested pretty well by Event'
      end

      specify 'final field always visible' do
        store = AClass.new 'asd'
        t1    = Thread.new { 1000000000.times { |i| store = AClass.new i.to_s } }
        t2    = Thread.new { 10.times { expect(store.final).not_to be_nil; Thread.pass } }
        t2.join
        t1.kill
      end

      describe 'attr volatile' do
        specify 'older writes are always visible' do
          store              = AClass.new
          store.not_volatile = 0
          store.volatile     = 0

          t1 = Thread.new do
            Thread.abort_on_exception = true
            1000000000.times do |i|
              store.not_volatile = i
              store.volatile     = i
            end
          end

          t2 = Thread.new do
            10.times do
              volatile     = store.volatile
              not_volatile = store.not_volatile
              expect(not_volatile).to be >= volatile
              Thread.pass
            end
          end

          t2.join
          t1.kill
        end
      end
    end
  end
end
