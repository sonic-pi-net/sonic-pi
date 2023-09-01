require 'timeout'
require 'concurrent/synchronization'

module Concurrent

  RSpec.describe Synchronization do

    RSpec.shared_examples :attr_volatile do

      specify 'older writes are always visible' do
        store              = store()
        store.not_volatile = 0
        store.volatile     = 0
        @stop              = false

        in_thread do
          Thread.abort_on_exception = true
          1000000000.times do |i|
            store.not_volatile = i
            store.volatile     = i
            break if @stop # on JRuby this is not kill-able loop
          end
        end

        t2 = in_thread do
          Thread.abort_on_exception = true
          10.times.map do
            Thread.pass
            volatile     = store.volatile
            not_volatile = store.not_volatile
            not_volatile >= volatile
          end
        end

        expect(t2.value.all?).to eq true
        @stop = true
      end
    end

    describe Synchronization::Object do
      class AAClass < Synchronization::Object
      end

      class ABClass < AAClass
        safe_initialization!
      end

      class ACClass < ABClass
      end

      class ADClass < ACClass
        safe_initialization!
      end

      it 'does not ensure visibility when not needed' do
        expect(Concurrent::Synchronization).not_to receive(:full_memory_barrier)
        AAClass.new
      end

      it "does ensure visibility when specified" do
        expect(Concurrent::Synchronization).to receive(:full_memory_barrier).exactly(:once)
        ABClass.new
      end

      it "does ensure visibility when specified in a parent" do
        expect(Concurrent::Synchronization).to receive(:full_memory_barrier).exactly(:once)
        ACClass.new
      end

      it "does ensure visibility once when specified in child again" do
        expect(Concurrent::Synchronization).to receive(:full_memory_barrier).exactly(:once)
        ADClass.new
      end

      # TODO (pitr 12-Sep-2015): give a whole gem a pass to find classes with final fields without using the convention and migrate
      Synchronization::Object.ensure_safe_initialization_when_final_fields_are_present

      class VolatileFieldClass < Synchronization::Object
        attr_volatile :volatile
        attr_accessor :not_volatile
      end

      let(:store) { VolatileFieldClass.new }
      it_should_behave_like :attr_volatile
    end

    describe Synchronization::LockableObject do

      class BClass < Synchronization::LockableObject
        safe_initialization!

        attr_volatile :volatile
        attr_accessor :not_volatile

        def initialize(value = nil)
          super()
          @Final = value
          ns_initialize
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

        public :synchronize

        private

        def ns_initialize
          @count = 0
        end
      end

      subject { BClass.new }

      describe '#wait' do

        it 'puts the current thread to sleep' do
          t1 = in_thread do
            Thread.abort_on_exception = true
            subject.wait
          end
          t2 = in_thread { Thread.pass until t1.status == 'sleep' }
          join_with t2
        end

        it 'allows the sleeping thread to be killed' do
          t = in_thread do
            Thread.abort_on_exception = true
            subject.wait rescue nil
          end
          sleep 0.1
          t.kill
          sleep 0.1
          expect(t.join).not_to eq nil
          expect(t.alive?).to eq false
        end

        it 'releases the lock on the current object' do
          t1 = in_thread do
            # #wait should release lock, even if it was already held on entry
            t2 = in_thread { subject.wait }
            Thread.pass until t2.status == 'sleep'
            subject.synchronize {} # it will deadlock here if #wait doesn't release lock
            t2
          end
          join_with t1
          repeat_until_success { expect(t1.value.status).to eq 'sleep' }
        end

        it 'can be called from within a #synchronize block' do
          t1 = in_thread do
            t2 = in_thread { subject.synchronize { subject.wait } }
            Thread.pass until t2.status == 'sleep'
            subject.synchronize {} # it will deadlock here if #wait doesn't release lock
            t2
          end
          join_with t1
          repeat_until_success { expect(t1.value.status).to eq 'sleep' }
        end
      end

      describe '#synchronize' do
        it 'allows only one thread to execute count' do
          threads = 10.times.map { in_thread(subject) { 100.times { subject.count } } }
          threads.each(&:join)
          expect(subject.count).to eq 1001
        end
      end

      describe 'signaling' do
        pending 'for now pending, tested pretty well by Event'
      end

      specify 'final field always visible' do
        require 'concurrent/atomic/count_down_latch'
        store = BClass.new 'asd'
        done  = CountDownLatch.new
        in_thread do
          1000000000.times do |i|
            store = BClass.new i.to_s
            break if done.count == 0
          end
        end
        in_thread do
          10.times do
            expect(store.final).not_to be_nil
            Thread.pass
          end
          done.count_down
        end
      end

      let(:store) { BClass.new }
      it_should_behave_like :attr_volatile
    end

    describe 'Concurrent::Synchronization::Volatile module' do
      class BareClass
        include Synchronization::Volatile

        attr_volatile :volatile
        attr_accessor :not_volatile
      end

      let(:store) { BareClass.new }
      it_should_behave_like :attr_volatile
    end

    describe 'attr_atomic' do
      specify do
        a = Class.new(Synchronization::Object) do
          attr_atomic :a

          def initialize(*rest)
            super
            self.a = :a
          end
        end

        b = Class.new(a) do
          attr_atomic :b

          def initialize
            super
            self.b = :b
          end
        end

        instance = b.new
        expect(instance.a).to be == :a
        expect(instance.b).to be == :b
      end
    end

  end
end
