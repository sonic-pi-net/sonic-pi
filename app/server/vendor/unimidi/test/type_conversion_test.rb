require 'helper'

class UniMIDI::TypeConversionTest < Test::Unit::TestCase

  context "TypeConversion" do

    context "#numeric_byte_array_to_hex_string" do

      should "convert byte array to hex string" do
        result = UniMIDI::TypeConversion.numeric_byte_array_to_hex_string([0x90, 0x40, 0x40])
        assert "904040", result
      end

    end

  end

end
