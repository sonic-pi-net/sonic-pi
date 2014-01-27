# Start looking for MIDI classes in the directory above this one.
# This forces us to use the local copy of MIDI, even if there is
# a previously installed version out there somewhere.
$LOAD_PATH[0, 0] = File.join(File.dirname(__FILE__), '..', 'lib')

require 'test/unit'
require 'midilib'

class VarLenTester < Test::Unit::TestCase

  VAR_LEN_TEST_DATA = {
    0x00000000 => 0x00,
    0x00000040 => 0x40,
    0x0000007F => 0x7F,
    0x00000080 => 0x8100,
    0x00002000 => 0xC000,
    0x00003FFF => 0xFF7F,
    0x00004000 => 0x818000,
    0x00100000 => 0xC08000,
    0x001FFFFF => 0xFFFF7F,
    0x00200000 => 0x81808000,
    0x08000000 => 0xC0808000,
    0x0FFFFFFF => 0xFFFFFF7F,
  }

  def num_to_var_len(num, answer)
    varlen = MIDI::Utils.as_var_len(num)
    varlen.reverse.each do |b|
      assert_equal(answer & 0xff, b)
      answer = answer >> 8
    end
  end

  def test_num_to_var_len
    VAR_LEN_TEST_DATA.each { |varlen, answer| num_to_var_len(varlen, answer) }
  end

end
