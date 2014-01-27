module MIDI

  # Utility methods.
  class Utils

    # MIDI note names. NOTE_NAMES[0] is 'C', NOTE_NAMES[1] is 'C#', etc.
    NOTE_NAMES = [
      'C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'
    ]

    # Given a MIDI note number, return the name and octave as a string.
    def Utils.note_to_s(num)
      note = num % 12
      octave = num / 12
      return "#{NOTE_NAMES[note]}#{octave - 1}"
    end

    # Given an integer, returns it as a variable length array of bytes (the
    # format used by MIDI files).
    #
    # The converse operation--converting a var len into a number--requires
    # input from a stream of bytes. Therefore we don't supply it here. That is
    # a part of the MIDIFile class.
    def Utils.as_var_len(val)
      buffer = []
      buffer << (val & 0x7f)
      val = (val >> 7)
      while val > 0
        buffer << (0x80 + (val & 0x7f))
        val = (val >> 7)
      end
      return buffer.reverse!
    end

  end
end
