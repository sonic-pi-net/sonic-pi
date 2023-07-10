require_relative 'timing_buffer_shared'
require 'concurrent/channel/buffer/ticker'

module Concurrent::Channel::Buffer

  RSpec.describe Ticker, edge: true do

    let(:delay) { 0.1 }
    subject { described_class.new(delay) }

    it_behaves_like :channel_timing_buffer

    context '#take' do
      it 'triggers until closed' do
        expected = 3
        actual = 0
        expected.times { actual += 1 if subject.take.is_a? Concurrent::Channel::Tick }
        expect(actual).to eq expected
      end

      it 'returns Concurrent::NULL when closed after trigger' do
        subject.take
        subject.close
        expect(subject).to be_closed
        expect(subject.take).to eq Concurrent::NULL
      end
    end

    context '#poll' do
      it 'triggers until closed' do
        expected = 3
        actual = 0
        expected.times do
          until subject.poll.is_a?(Concurrent::Channel::Tick)
            actual += 1
          end
        end
      end
    end

    context '#next' do
      it 'triggers until closed' do
        expected = 3
        actual = 0
        expected.times { actual += 1 if subject.next.first.is_a? Concurrent::Channel::Tick }
        expect(actual).to eq expected
      end

      it 'returns true for more while open' do
        _, more = subject.next
        expect(more).to be true
      end

      it 'returns false for more once closed' do
        subject.close
        _, more = subject.next
        expect(more).to be false
      end
    end
  end
end
