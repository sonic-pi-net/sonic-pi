require_relative 'buffered_shared'
require 'concurrent/channel/buffer/sliding'

module Concurrent::Channel::Buffer

  RSpec.describe Sliding, edge: true do

    subject { described_class.new(10) }

    it_behaves_like :channel_buffered_buffer

    specify do
      expect(subject).to_not be_blocking
    end

    context '#put' do

      it 'does not block when full' do
        subject = described_class.new(1)
        3.times {|i| expect(subject.put(i)).to be true }
      end

      it 'drops the first value when full' do
        subject = described_class.new(1)
        3.times{|i| subject.put(i)}
        internal_buffer = subject.instance_variable_get(:@buffer)
        expect(internal_buffer.size).to eq 1
        expect(internal_buffer.first).to eq 2
      end
    end

    context '#offer' do

      it 'returns true immediately when full' do
        subject = described_class.new(1)
        subject.put(:foo)
        expect(subject.offer(:bar)).to be true
      end

      it 'drops the first value when full' do
        subject = described_class.new(1)
        3.times{|i| subject.offer(i)}
        internal_buffer = subject.instance_variable_get(:@buffer)
        expect(internal_buffer.size).to eq 1
        expect(internal_buffer.first).to eq 2
      end
    end
  end
end
