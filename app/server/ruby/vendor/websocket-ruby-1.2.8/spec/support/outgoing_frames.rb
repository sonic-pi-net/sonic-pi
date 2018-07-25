# frozen_string_literal: true

RSpec.shared_examples_for 'valid_outgoing_frame' do
  it 'is outgoing frame' do
    expect(subject.class).to be WebSocket::Frame::Outgoing
  end

  it 'is in proper verions' do
    expect(subject.version).to eql version
  end

  it 'has proper type set' do
    expect(subject.type).to eql frame_type
  end

  it 'contains decoded data' do
    expect(subject.data).to eql(decoded_text)
  end

  it 'returns encoded data as to_s' do
    expect(subject.to_s).to eql(encoded_text)
  end

  context 'after parsing' do
    before { subject.to_s }

    it 'has valid errors set' do
      expect(subject.error).to eql error
    end

    it 'requires sending' do
      expect(subject.require_sending?).to eql require_sending
    end
  end
end
