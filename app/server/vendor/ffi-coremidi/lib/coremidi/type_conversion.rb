module CoreMIDI

  # Helper for convertig MIDI data
  module TypeConversion

    extend self

    # Convert an array of numeric byes to a hex string (e.g. [0x90, 0x40, 0x40] becomes "904040")
    # @param [Array<Fixnum>] bytes
    # @return [String]
    def numeric_bytes_to_hex_string(bytes)
      string_bytes = bytes.map do |byte| 
        str = byte.to_s(16).upcase
        str = "0" + str if byte < 16
        str
      end
      string_bytes.join
    end 

  end
end
