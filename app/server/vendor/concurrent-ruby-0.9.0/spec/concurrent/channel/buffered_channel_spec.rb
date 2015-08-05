module Concurrent
  module Channel

    describe BufferedChannel do

      let(:size) { 2 }
      let!(:channel) { BufferedChannel.new(size) }
      let(:probe) { Channel::Probe.new }

      context 'without timeout' do

        describe '#push' do
          it 'adds elements to buffer' do
            expect(channel.buffer_queue_size).to be 0

            channel.push('a')
            channel.push('a')

            expect(channel.buffer_queue_size).to be 2
          end

          it 'should block when buffer is full' do
            channel.push 1
            channel.push 2

            t = Thread.new { channel.push 3 }
            sleep(0.05)
            expect(t.status).to eq 'sleep'
          end

          it 'restarts thread when buffer is no more full' do
            channel.push 'hi'
            channel.push 'foo'

            result = nil

            Thread.new { channel.push 'bar'; result = 42 }

            sleep(0.1)

            channel.pop

            sleep(0.1)

            expect(result).to eq 42
          end

          it 'should assign value to a probe if probe set is not empty' do
            channel.select(probe)
            Thread.new { sleep(0.1); channel.push 3 }
            expect(probe.value.first).to eq 3
          end
        end

        describe '#pop' do
          it 'should block if buffer is empty' do
            t = Thread.new { channel.pop }
            sleep(0.05)
            expect(t.status).to eq 'sleep'
          end

          it 'returns value if buffer is not empty' do
            channel.push 1
            result = channel.pop

            expect(result.first).to eq 1
          end

          it 'removes the first value from the buffer' do
            channel.push 'a'
            channel.push 'b'

            expect(channel.pop.first).to eq 'a'
            expect(channel.buffer_queue_size).to eq 1
          end
        end

      end

      describe 'select' do

        it 'does not block' do
          t = Thread.new { channel.select(probe) }

          sleep(0.05)

          expect(t.status).to eq false
        end

        it 'gets notified by writer thread' do
          channel.select(probe)

          Thread.new { channel.push 82 }

          expect(probe.value.first).to eq 82
        end

      end

      context 'already set probes' do
        context 'empty buffer' do
          it 'discards already set probes' do
            probe.set('set value')

            channel.select(probe)

            channel.push 27

            expect(channel.buffer_queue_size).to eq 1
            expect(channel.probe_set_size).to eq 0
          end
        end

        context 'empty probe set' do
          it 'discards set probe' do
            probe.set('set value')

            channel.push 82

            channel.select(probe)

            expect(channel.buffer_queue_size).to eq 1

            expect(channel.pop.first).to eq 82

          end
        end
      end

      describe 'probe set' do

        it 'has size zero after creation' do
          expect(channel.probe_set_size).to eq 0
        end

        it 'increases size after a select' do
          channel.select(probe)
          expect(channel.probe_set_size).to eq 1
        end

        it 'decreases size after a removal' do
          channel.select(probe)
          channel.remove_probe(probe)
          expect(channel.probe_set_size).to eq 0
        end

      end

    end
  end
end
