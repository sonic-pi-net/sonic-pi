require 'concurrent/atomic/semaphore'

RSpec.shared_examples :semaphore do
  let(:semaphore) { described_class.new(3) }

  describe '#initialize' do
    it 'raises an exception if the initial count is not an integer' do
      expect {
        described_class.new('foo')
      }.to raise_error(ArgumentError)
    end

    context 'when initializing with 0' do
      let(:semaphore) { described_class.new(0) }

      it do
        expect(semaphore).to_not be nil
      end
    end

    context 'when initializing with -1' do
      let(:semaphore) { described_class.new(-1) }

      it do
        semaphore.release
        expect(semaphore.available_permits).to eq 0
      end
    end
  end

  describe '#acquire' do
    context 'without block' do
      context 'permits available' do
        it 'should return nil immediately' do
          result = semaphore.acquire
          expect(result).to be_nil
        end
      end

      context 'not enough permits available' do
        it 'should block thread until permits are available' do
          semaphore.drain_permits
          in_thread { sleep(0.2); semaphore.release }

          result = semaphore.acquire
          expect(result).to be_nil
          expect(semaphore.available_permits).to eq 0
        end
      end

      context 'when acquiring negative permits' do
        it 'raises ArgumentError' do
          expect {
            semaphore.acquire(-1)
          }.to raise_error(ArgumentError)
        end
      end
    end

    context 'with block' do
      context 'permits available' do
        it 'should acquire permits, run the block, release permits, and return block return value' do
          available_permits = semaphore.available_permits
          yielded = false
          expected_result = Object.new

          actual_result = semaphore.acquire do
            expect(semaphore.available_permits).to eq(available_permits - 1)
            yielded = true
            expected_result
          end

          expect(semaphore.available_permits).to eq(available_permits)
          expect(yielded).to be true
          expect(actual_result).to be(expected_result)
        end

        it 'if the block raises, the permit is still released' do
          expect {
            expect {
              semaphore.acquire do
                raise 'boom'
              end
            }.to raise_error('boom')
          }.to_not change { semaphore.available_permits }
        end
      end

      context 'not enough permits available' do
        it 'should block thread until permits are available' do
          yielded = false
          semaphore.drain_permits
          in_thread { sleep(0.2); semaphore.release }
          expected_result = Object.new

          actual_result = semaphore.acquire do
            yielded = true
            expected_result
          end

          expect(actual_result).to be(expected_result)
          expect(yielded).to be true
          expect(semaphore.available_permits).to eq 1
        end
      end

      context 'when acquiring negative permits' do
        it 'raises ArgumentError' do
          expect {
            expect {
              semaphore.acquire(-1) do
                raise 'block should never run'
              end
            }.to raise_error(ArgumentError)
          }.not_to change { semaphore.available_permits }
        end
      end
    end
  end

  describe '#drain_permits' do
    it 'drains all available permits' do
      drained = semaphore.drain_permits
      expect(drained).to eq 3
      expect(semaphore.available_permits).to eq 0
    end

    it 'drains nothing in no permits are available' do
      semaphore.reduce_permits 3
      drained = semaphore.drain_permits
      expect(drained).to eq 0
    end
  end

  describe '#try_acquire' do
    context 'without block' do
      context 'without timeout' do
        it 'acquires immediately if permits are available' do
          result = semaphore.try_acquire(1)
          expect(result).to be_truthy
        end

        it 'returns false immediately in no permits are available' do
          result = semaphore.try_acquire(20)
          expect(result).to be_falsey
        end

        context 'when trying to acquire negative permits' do
          it do
            expect {
              semaphore.try_acquire(-1)
            }.to raise_error(ArgumentError)
          end
        end
      end

      context 'with timeout' do
        it 'acquires immediately if permits are available' do
          result = semaphore.try_acquire(1, 5)
          expect(result).to be_truthy
        end

        it 'acquires when permits are available within timeout' do
          semaphore.drain_permits
          in_thread { sleep 0.1; semaphore.release }
          result = semaphore.try_acquire(1, 1)
          expect(result).to be_truthy
        end

        it 'returns false on timeout' do
          semaphore.drain_permits
          result = semaphore.try_acquire(1, 0.1)
          expect(result).to be_falsey
        end
      end
    end

    context 'with block' do
      context 'without timeout' do
        it 'acquires immediately if permits are available and returns block return value' do
          yielded = false
          available_permits = semaphore.available_permits
          expected_result = Object.new

          actual_result = semaphore.try_acquire(1) do
            yielded = true
            expect(semaphore.available_permits).to eq(available_permits - 1)
            expected_result
          end

          expect(actual_result).to be(expected_result)
          expect(yielded).to be true
          expect(semaphore.available_permits).to eq available_permits
        end

        it 'releases permit if block raises' do
          expect {
            expect {
              semaphore.try_acquire(1) do
                raise 'boom'
              end
            }.to raise_error('boom')
          }.not_to change { semaphore.available_permits }
        end

        it 'returns false immediately in no permits are available' do
          expect {
            result = semaphore.try_acquire(20) do
              raise 'block should never run'
            end

            expect(result).to be_falsey
          }.not_to change { semaphore.available_permits }
        end

        context 'when trying to acquire negative permits' do
          it do
            expect {
              expect {
                semaphore.try_acquire(-1) do
                  raise 'block should never run'
                end
              }.to raise_error(ArgumentError)
            }.not_to change { semaphore.available_permits }
          end
        end
      end

      context 'with timeout' do
        it 'acquires immediately if permits are available, and returns block return value' do
          expect {
            yielded = false
            expected_result = Object.new

            actual_result = semaphore.try_acquire(1, 5) do
              yielded = true
              expected_result
            end

            expect(actual_result).to be(expected_result)
            expect(yielded).to be true
          }.not_to change { semaphore.available_permits }
        end

        it 'releases permits if block raises' do
          expect {
            expect {
              semaphore.try_acquire(1, 5) do
                raise 'boom'
              end
            }.to raise_error('boom')
          }.not_to change { semaphore.available_permits }
        end

        it 'acquires when permits are available within timeout, and returns block return value' do
          yielded = false
          semaphore.drain_permits
          in_thread { sleep 0.1; semaphore.release }
          expected_result = Object.new

          actual_result = semaphore.try_acquire(1, 1) do
            yielded = true
            expected_result
          end

          expect(actual_result).to be(expected_result)
          expect(yielded).to be true
          expect(semaphore.available_permits).to be 1
        end

        it 'returns false on timeout' do
          semaphore.drain_permits

          result = semaphore.try_acquire(1, 0.1) do
            raise 'block should never run'
          end

          expect(result).to be_falsey
          expect(semaphore.available_permits).to be 0
        end
      end
    end
  end

  describe '#reduce_permits' do
    it 'raises ArgumentError if reducing by negative number' do
      expect {
        semaphore.reduce_permits(-1)
      }.to raise_error(ArgumentError)
    end

    it 'reduces permits below zero' do
      semaphore.reduce_permits 1003
      expect(semaphore.available_permits).to eq(-1000)
    end

    it 'reduces permits' do
      semaphore.reduce_permits 1
      expect(semaphore.available_permits).to eq 2
      semaphore.reduce_permits 2
      expect(semaphore.available_permits).to eq 0
    end

    it 'reduces zero permits' do
      semaphore.reduce_permits 0
      expect(semaphore.available_permits).to eq 3
    end
  end

  describe '#release' do
    it 'increases the number of available permits by one' do
      semaphore.release
      expect(semaphore.available_permits).to eq 4
    end

    context 'when a number of permits is specified' do
      it 'increases the number of available permits by the specified value' do
        semaphore.release(2)
        expect(semaphore.available_permits).to eq 5
      end

      context 'when permits is set to negative number' do
        it do
          expect {
            semaphore.release(-1)
          }.to raise_error(ArgumentError)
        end
      end
    end
  end
end

module Concurrent
  RSpec.describe MutexSemaphore do
    it_should_behave_like :semaphore
  end

  if Concurrent.on_jruby?
    RSpec.describe JavaSemaphore do
      it_should_behave_like :semaphore
    end
  end

  RSpec.describe Semaphore do
    if Concurrent.on_jruby?
      it 'inherits from JavaSemaphore' do
        expect(Semaphore.ancestors).to include(JavaSemaphore)
      end
    else
      it 'inherits from MutexSemaphore' do
        expect(Semaphore.ancestors).to include(MutexSemaphore)
      end
    end
  end
end
