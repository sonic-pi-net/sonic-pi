module UniMIDI
  
  # Utility for converting between different data formats
  module TypeConversion

    extend self
      
    # Convert an array of numeric bytes to string of hex bytes 
    # @param [Array<Fixnum>] byte An array of numeric bytes eg [0x90, 0x40, 0x40]
    # @return [String] A string of hex bytes eg "904040"
    def numeric_byte_array_to_hex_string(bytes)
      bytes.map { |b| b.to_s(16) }.join
    end
      
  end
  
end
