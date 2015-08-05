module Concurrent
  module Channel

    describe Channel do

      describe '.select' do

        context 'without timeout' do
          it 'returns the first value available on a channel' do
            channels = [ UnbufferedChannel.new, UnbufferedChannel.new]

            Thread.new { channels[1].push 77 }

            value, channel = Channel.select(*channels)

            expect(value).to eq 77
            expect(channel).to be channels[1]
          end

          it 'cleans up' do
            channels = [ UnbufferedChannel.new, UnbufferedChannel.new]
            channels.each { |ch| allow(ch).to receive(:remove_probe).with( an_instance_of(Channel::Probe) )}

            Thread.new { channels[1].push 77 }

            value, channel = Channel.select(*channels)

            expect(value).to eq 77
            expect(channel).to be channels[1]

            channels.each { |ch| expect(ch).to have_received(:remove_probe).with( an_instance_of(Channel::Probe) ) }
          end
        end

      end

    end
  end
end
