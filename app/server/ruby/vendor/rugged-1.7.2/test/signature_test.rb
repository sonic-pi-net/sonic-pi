require "test_helper"

class SignatureTest < Rugged::TestCase
  def test_parse_signature_from_buffer
    buf = "Random J Hacker <hacks@example.com> 1499423310 +0200"
    sig = Rugged::signature_from_buffer(buf)

    expected = {
      :name => "Random J Hacker",
      :email => "hacks@example.com",
      :time => Time.at(1499423310),
    }

    assert_equal expected, sig
    assert_equal 2*3600, sig[:time].gmt_offset
  end

  def test_parse_signature_from_buffer_invalid
    buf = "Random J Hacker <hacks@example.com> foobar 1499423310 +0200"
    assert_raises(Rugged::InvalidError) { Rugged::signature_from_buffer(buf) }
  end
end
