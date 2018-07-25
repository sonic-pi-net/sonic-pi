# frozen_string_literal: true

RSpec.shared_examples_for 'valid_incoming_frame' do
  let(:decoded_text_array) { Array(decoded_text) }
  let(:frame_type_array) { Array(frame_type) }

  it 'is incoming frame' do
    expect(subject.class).to be WebSocket::Frame::Incoming
  end

  it 'is in proper verions' do
    expect(subject.version).to eql version
  end

  it 'does not have type set' do
    expect(subject.type).to be nil
  end

  it 'is not decoded' do
    expect(subject.decoded?).to be false
  end

  it 'contains encoded data' do
    expect(subject.data).to eql(encoded_text || '')
  end

  it 'returns data as to_s' do
    expect(subject.to_s).to eql(encoded_text || '')
  end

  it 'has specified number of returned frames' do
    decoded_text_array.each_with_index do |da, index|
      n = subject.next
      expect(n).not_to be_nil, "Should return frame for #{da}, #{frame_type_array[index]}"
      expect(n.class).to eql(WebSocket::Frame::Incoming), "Should be WebSocket::Frame::Incoming, #{n} received instead"
    end
    expect(subject.next).to be_nil
    if error.is_a?(Class)
      expect(subject.error).to eql(error.new.message)
    else
      expect(subject.error).to eql(error)
    end
  end

  it 'returns valid decoded frame for each specified decoded texts' do
    decoded_text_array.each_with_index do |da, index|
      f = subject.next
      expect(f.decoded?).to be true
      expect(f.type).to eql(frame_type_array[index])
      expect(f.code).to eql(close_code) if defined?(close_code)
      expect(f.to_s).to eql(da)
    end
  end

  context 'with raising' do
    before { WebSocket.should_raise = true }
    after { WebSocket.should_raise = false }

    it 'has specified number of returned frames' do
      if error
        expect do
          decoded_text_array.each_with_index do |da, index|
            n = subject.next
            expect(n).not_to be_nil, "Should return frame for #{da}, #{frame_type_array[index]}"
            expect(n.class).to eql(WebSocket::Frame::Incoming), "Should be WebSocket::Frame::Incoming, #{n} received instead"
          end
          expect(subject.next).to be_nil
          if error.is_a?(Class)
            expect(subject.error).to eql(error.new.message)
          else
            expect(subject.error).to eql(error)
          end
        end.to raise_error(error)
      end
    end
  end
end
