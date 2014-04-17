require 'midilib/consts'

if RUBY_VERSION < '1.9'
  class IO
    def readbyte
      c = getc()
      raise 'unexpected EOF' unless c
      c
    end
  end
end

module MIDI

  module IO

    # A MIDIFile parses a MIDI file and calls methods when it sees MIDI events.
    # Most of the methods are stubs. To do anything interesting with the events,
    # override these methods (those between the "The rest of these are NOPs by
    # default" and "End of NOPs" comments).
    #
    # See SeqReader for a subclass that uses these methods to create Event
    # objects.
    class MIDIFile

      MThd_BYTE_ARRAY = [77, 84, 104, 100] # "MThd"
      MTrk_BYTE_ARRAY = [77, 84, 114, 107] # "MTrk"

      # This array is indexed by the high half of a status byte. Its
      # value is either the number of bytes needed (1 or 2) for a channel
      # message, or 0 if it's not a channel message.
      NUM_DATA_BYTES = [
	0, 0, 0, 0, 0, 0, 0, 0, # 0x00 - 0x70
	2, 2, 2, 2, 1, 1, 2, 0  # 0x80 - 0xf0
      ]

      attr_accessor  :curr_ticks	# Current time, from delta-time in MIDI file
      attr_accessor  :ticks_so_far # Number of delta-time ticks so far
      attr_accessor  :bytes_to_be_read # Counts number of bytes expected

      attr_accessor  :no_merge	# true means continued sysex are not collapsed
      attr_accessor  :skip_init	# true if initial garbage should be skipped

      # Raw data info
      attr_accessor  :raw_time_stamp_data
      attr_accessor  :raw_var_num_data
      attr_accessor  :raw_data

      def initialize
	@no_merge = false
	@skip_init = true
	@io = nil
	@bytes_to_be_read = 0
	@msg_buf = nil
      end

      # The only public method. Each MIDI event in the file causes a
      # method to be called.
      def read_from(io)
	error('must specify non-nil input stream') if io.nil?
	@io = io

	ntrks = read_header()
	error('No tracks!') if ntrks <= 0

	ntrks.times { read_track() }
      end

      # This default getc implementation tries to read a single byte
      # from io and returns it as an integer.
      def getc
        @bytes_to_be_read -= 1
        @io.readbyte()
      end

      # Return the next +n+ bytes from @io as an array.
      def get_bytes(n)
	buf = []
	n.times { buf << getc() }
	buf
      end

      # The default error handler.
      def error(str)
	loc = @io.tell() - 1
	raise "#{self.class.name} error at byte #{loc} (0x#{'%02x' % loc}): #{str}"
      end

      # The rest of these are NOPs by default.

      # MIDI header.
      def header(format, ntrks, division)
      end

      def start_track(bytes_to_be_read)
      end

      def end_track()
      end

      def note_on(chan, note, vel)
      end

      def note_off(chan, note, vel)
      end

      def pressure(chan, note, press)
      end

      def controller(chan, control, value)
      end

      def pitch_bend(chan, msb, lsb)
      end

      def program(chan, program)
      end

      def chan_pressure(chan, press)
      end

      def sysex(msg)
      end

      def meta_misc(type, msg)
      end

      def sequencer_specific(type, msg)
      end

      def sequence_number(num)
      end

      def text(type, msg)
      end

      def eot()
      end

      def time_signature(numer, denom, clocks, qnotes)
      end

      def smpte(hour, min, sec, frame, fract)
      end

      def tempo(microsecs)
      end

      def key_signature(sharpflat, is_minor)
      end

      def arbitrary(msg)
      end

      # End of NOPs.


      # Read through 'MThd' or 'MTrk' header string. If skip is true, attempt
      # to skip initial trash. If there is an error, #error is called.
      def read_mt_header_string(bytes, skip)
	b = []
	bytes_to_read = 4
	while true
          data = get_bytes(bytes_to_read)
          b += data
          if b.length < 4
            error("unexpected EOF while trying to read header string #{s}")
          end

          # See if we found the bytes we're looking for
          return if b == bytes

          if skip		# Try again with the next char
            i = b[1..-1].index(bytes[0])
            if i.nil?
              b = []
              bytes_to_read = 4
            else
              b = b[i..-1]
              bytes_to_read = 4 - i
            end
          else
            error("header string #{bytes.collect{|b| b.chr}.join} not found")
          end
	end
      end

      # Read a header chunk.
      def read_header
	@bytes_to_be_read = 0
	read_mt_header_string(MThd_BYTE_ARRAY, @skip_init) # "MThd"

	@bytes_to_be_read = read32()
	format = read16()
	ntrks = read16()
	division = read16()

	header(format, ntrks, division)

	# Flush any extra stuff, in case the length of the header is not 6
	if @bytes_to_be_read > 0
          get_bytes(@bytes_to_be_read)
          @bytes_to_be_read = 0
	end

	return ntrks
      end

      # Read a track chunk.
      def read_track
	c = c1 = type = needed = 0
	sysex_continue = false	# True if last msg was unfinished
	running = false		# True when running status used
	status = 0		# (Possibly running) status byte

	@bytes_to_be_read = 0
	read_mt_header_string(MTrk_BYTE_ARRAY, false)

	@bytes_to_be_read = read32()
	@curr_ticks = @ticks_so_far = 0

	start_track()

	while @bytes_to_be_read > 0
          @curr_ticks = read_var_len() # Delta time
          @ticks_so_far += @curr_ticks

          # Copy raw var num data into raw time stamp data
          @raw_time_stamp_data = @raw_var_num_data.dup()

          c = getc()		# Read first byte

          if sysex_continue && c != EOX
            error("didn't find expected continuation of a sysex")
          end

          if (c & 0x80).zero? # Running status?
            error('unexpected running status') if status.zero?
            running = true
          else
            status = c
            running = false
          end

          needed = NUM_DATA_BYTES[(status >> 4) & 0x0f]

          if needed.nonzero?	# i.e., is it a channel message?
            c1 = running ? c : (getc() & 0x7f)

            # The "& 0x7f" here may seem unnecessary, but I've seen
            # "bad" MIDI files that had, for example, volume bytes
            # with the upper bit set. This code should not harm
            # proper data.
            chan_message(running, status, c1,
                         (needed > 1) ? (getc() & 0x7f) : 0)
            next
          end

          case c
          when META_EVENT	# Meta event
            type = getc()
            msg_init()
            msg_read(read_var_len())
            meta_event(type)
          when SYSEX		# Start of system exclusive
            msg_init()
            msg_add(SYSEX)
            c = msg_read(read_var_len())

            if c == EOX || !@no_merge
              handle_sysex(msg())
            else
              sysex_continue = true
            end
          when EOX		# Sysex continuation or arbitrary stuff
            msg_init() if !sysex_continue
            c = msg_read(read_var_len())

            if !sysex_continue
              handle_arbitrary(msg())
            elsif c == EOX
              handle_sysex(msg())
              sysex_continue = false
            end
          else
            bad_byte(c)
          end
	end
	end_track()
      end

      # Handle an unexpected byte.
      def bad_byte(c)
	error(sprintf("unexpected byte: 0x%02x", c))
      end

      # Handle a meta event.
      def meta_event(type)
	m = msg()		# Copy of internal message buffer

	# Create raw data array
	@raw_data = []
	@raw_data << META_EVENT
	@raw_data << type
	@raw_data << @raw_var_num_data
	@raw_data << m
	@raw_data.flatten!

	case type
	when META_SEQ_NUM
          sequence_number((m[0] << 8) + m[1])
	when META_TEXT, META_COPYRIGHT, META_SEQ_NAME, META_INSTRUMENT,
          META_LYRIC, META_MARKER, META_CUE, 0x08, 0x09, 0x0a,
          0x0b, 0x0c, 0x0d, 0x0e, 0x0f
          text(type, m)
	when META_TRACK_END
          eot()
	when META_SET_TEMPO
          tempo((m[0] << 16) + (m[1] << 8) + m[2])
	when META_SMPTE
          smpte(m[0], m[1], m[2], m[3], m[4])
	when META_TIME_SIG
          time_signature(m[0], m[1], m[2], m[3])
	when META_KEY_SIG
          key_signature(m[0], m[1] == 0 ? false : true)
	when META_SEQ_SPECIF
          sequencer_specific(type, m)
	else
          meta_misc(type, m)
	end
      end

      # Handle a channel message (note on, note off, etc.)
      def chan_message(running, status, c1, c2)
	@raw_data = []
	@raw_data << status unless running
	@raw_data << c1
	@raw_data << c2

	chan = status & 0x0f

	case (status & 0xf0)
	when NOTE_OFF
          note_off(chan, c1, c2)
	when NOTE_ON
          note_on(chan, c1, c2)
	when POLY_PRESSURE
          pressure(chan, c1, c2)
	when CONTROLLER
          controller(chan, c1, c2)
	when PITCH_BEND
          pitch_bend(chan, c1, c2)
	when PROGRAM_CHANGE
          program(chan, c1)
	when CHANNEL_PRESSURE
          chan_pressure(chan, c1)
	else
          error("illegal chan message 0x#{'%02x' % (status & 0xf0)}\n")
	end
      end

      # Copy message into raw data array, then call sysex().
      def handle_sysex(msg)
	@raw_data = msg.dup()
	sysex(msg)
      end

      # Copy message into raw data array, then call arbitrary().
      def handle_arbitrary(msg)
	@raw_data = msg.dup()
	arbitrary(msg)
      end

      # Read and return a sixteen bit value.
      def read16
	val = (getc() << 8) + getc()
	val = -(val & 0x7fff) if (val & 0x8000).nonzero?
	return val
      end

      # Read and return a 32-bit value.
      def read32
	val = (getc() << 24) + (getc() << 16) + (getc() << 8) +
          getc()
	val = -(val & 0x7fffffff) if (val & 0x80000000).nonzero?
	return val
      end

      # Read a varlen value.
      def read_var_len
	@raw_var_num_data = []
	c = getc()
	@raw_var_num_data << c
	val = c
	if (val & 0x80).nonzero?
          val &= 0x7f
          while true
            c = getc()
            @raw_var_num_data << c
            val = (val << 7) + (c & 0x7f)
            break if (c & 0x80).zero?
          end
	end
	return val
      end

      # Write a sixteen-bit value.
      def write16(val)
	val = (-val) | 0x8000 if val < 0
	putc((val >> 8) & 0xff)
	putc(val & 0xff)
      end

      # Write a 32-bit value.
      def write32(val)
	val = (-val) | 0x80000000 if val < 0
	putc((val >> 24) & 0xff)
	putc((val >> 16) & 0xff)
	putc((val >> 8) & 0xff)
	putc(val & 0xff)
      end

      # Write a variable length value.
      def write_var_len(val)
	if val.zero?
          putc(0)
          return
	end

	buf = []

	buf << (val & 0x7f)
	while (value >>= 7) > 0
          buf << (val & 0x7f) | 0x80
	end

	buf.reverse.each { |b| putc(b) }
      end

      # Add a byte to the current message buffer.
      def msg_add(c)
	@msg_buf << c
      end

      # Read and add a number of bytes to the message buffer. Return
      # the last byte (so we can see if it's an EOX or not).
      def msg_read(n_bytes)
	@msg_buf += get_bytes(n_bytes)
	@msg_buf.flatten!
	return @msg_buf[-1]
      end

      # Initialize the internal message buffer.
      def msg_init
	@msg_buf = []
      end

      # Return a copy of the internal message buffer.
      def msg
	return @msg_buf.dup()
      end

    end

  end
end
