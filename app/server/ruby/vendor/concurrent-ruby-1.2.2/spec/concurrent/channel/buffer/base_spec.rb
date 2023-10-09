require_relative 'buffered_shared'
require 'concurrent/channel/buffer/base'

module Concurrent::Channel::Buffer

  RSpec.describe Base, edge: true do

    subject { described_class.new }

    specify do
      expect(subject.capacity).to eq 0
    end

    specify do
      expect(subject).to be_blocking
    end

    specify do
      expect {
        subject.size
      }.to raise_error(NotImplementedError)
    end

    specify do
      expect {
        subject.empty?
      }.to raise_error(NotImplementedError)
    end

    specify do
      expect {
        subject.full?
      }.to raise_error(NotImplementedError)
    end

    specify do
      expect {
        subject.put(42)
      }.to raise_error(NotImplementedError)
    end

    specify do
      expect {
        subject.offer(42)
      }.to raise_error(NotImplementedError)
    end

    specify do
      expect {
        subject.take
      }.to raise_error(NotImplementedError)
    end

    specify do
      expect {
        subject.poll
      }.to raise_error(NotImplementedError)
    end

    specify do
      expect {
        subject.next
      }.to raise_error(NotImplementedError)
    end

    specify do
      expect(subject).to_not be_closed
    end

    specify do
      subject.close
      expect(subject).to be_closed
    end
  end
end
