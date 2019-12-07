shared_examples_for 'valid_incoming_frame' do
  let(:decoded_text_array) { decoded_text == "" ? [""] : Array(decoded_text) } # Bug in Ruby 1.8 -> Array("") => [] instead of [""]
  let(:frame_type_array) { Array(frame_type) }

  its(:class) { should eql(WebSocket::Frame::Incoming) }
  its(:data) { should eql(encoded_text || "") }
  its(:version) { should eql(version) }
  its(:type) { should be_nil }
  its(:decoded?) { should be_false }
  its(:to_s) { should eql(encoded_text || "") }

  it "should have specified number of returned frames" do
    decoded_text_array.each_with_index do |da, index|
      n = subject.next
      n.should_not be_nil, "Should return frame for #{da}, #{frame_type_array[index]}"
      n.class.should eql(WebSocket::Frame::Incoming), "Should be WebSocket::Frame::Incoming, #{n} received instead"
    end
    subject.next.should be_nil
    if error.is_a?(Class)
      subject.error.should eql(error.new.message)
    else
      subject.error.should eql(error)
    end
  end

  it "should return valid decoded frame for each specified decoded texts" do
    decoded_text_array.each_with_index do |da, index|
      f = subject.next
      f.decoded?.should be_true
      f.type.should eql(frame_type_array[index])
      f.to_s.should eql(da)
    end
  end

  context "with raising" do
    before { WebSocket.should_raise = true }
    after { WebSocket.should_raise = false }

    it "should have specified number of returned frames" do
      expect do
        decoded_text_array.each_with_index do |da, index|
          n = subject.next
          n.should_not be_nil, "Should return frame for #{da}, #{frame_type_array[index]}"
          n.class.should eql(WebSocket::Frame::Incoming), "Should be WebSocket::Frame::Incoming, #{n} received instead"
        end
        subject.next.should be_nil
        if error.is_a?(Class)
          subject.error.should eql(error.new.message)
        else
          subject.error.should eql(error)
        end
      end.to raise_error(error) if error
    end
  end
end
