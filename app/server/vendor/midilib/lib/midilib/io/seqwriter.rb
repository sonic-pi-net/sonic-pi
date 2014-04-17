# Writes MIDI files.

require 'midilib/event'
require 'midilib/utils'

module MIDI

  module IO

    class SeqWriter

      def initialize(seq, proc = nil) # :yields: num_tracks, index
	@seq = seq
	@update_block = block_given?() ? Proc.new() : proc
      end

      # Writes a MIDI format 1 file.
      def write_to(io)
	@io = io
	@bytes_written = 0
	write_header()
	@update_block.call(nil, @seq.tracks.length, 0) if @update_block
	@seq.tracks.each_with_index do |track, i|
          write_track(track)
          @update_block.call(track, @seq.tracks.length, i) if @update_block
	end
      end

      def write_header
	@io.print 'MThd'
	write32(6)
	write16(1)		# Ignore sequence format; write as format 1
	write16(@seq.tracks.length)
	write16(@seq.ppqn)
      end

      def write_track(track)
	@io.print 'MTrk'
	track_size_file_pos = @io.tell()
	write32(0)		# Dummy byte count; overwritten later
	@bytes_written = 0	# Reset after previous write

	write_instrument(track.instrument)

	prev_event = nil
	prev_status = 0
	track.events.each do |event|
          if !event.kind_of?(Realtime)
            write_var_len(event.delta_time)
          end

          data = event.data_as_bytes()
          status = data[0] # status byte plus channel number, if any

          # running status byte
          status = possibly_munge_due_to_running_status_byte(data, prev_status)

          @bytes_written += write_bytes(data)

          prev_event = event
          prev_status = status
	end

	# Write track end event.
	event = MetaEvent.new(META_TRACK_END)
	write_var_len(0)
	@bytes_written += write_bytes(event.data_as_bytes())

	# Go back to beginning of track data and write number of bytes,
	# then come back here to end of file.
	@io.seek(track_size_file_pos)
	write32(@bytes_written)
	@io.seek(0, ::IO::SEEK_END)
      end

      # If we can use a running status byte, delete the status byte from
      # the given data. Return the status to remember for next time as the
      # running status byte for this event.
      def possibly_munge_due_to_running_status_byte(data, prev_status)
	status = data[0]
	return status if status >= 0xf0 || prev_status >= 0xf0

	chan = (status & 0x0f)
	return status if chan != (prev_status & 0x0f)

	status = (status & 0xf0)
	prev_status = (prev_status & 0xf0)

	# Both events are on the same channel. If the two status bytes are
	# exactly the same, the rest is trivial. If it's note on/note off,
	# we can combine those further.
	if status == prev_status
          data[0,1] = []	# delete status byte from data
          return status + chan
	elsif status == NOTE_OFF && data[2] == 64
          # If we see a note off and the velocity is 64, we can store
          # a note on with a velocity of 0. If the velocity isn't 64
          # then storing a note on would be bad because the would be
          # changed to 64 when reading the file back in.
          data[2] = 0		# set vel to 0; do before possible shrinking
          status = NOTE_ON + chan
          if prev_status == NOTE_ON
            data[0,1] = []	# delete status byte
          else
            data[0] = status
          end
          return status
	else
          # Can't compress data
          return status + chan
	end
      end

      def write_instrument(instrument)
	event = MetaEvent.new(META_INSTRUMENT, instrument)
	write_var_len(0)
	data = event.data_as_bytes()
	@bytes_written += write_bytes(data)
      end

      def write_var_len(val)
	buffer = Utils.as_var_len(val)
	@bytes_written += write_bytes(buffer)
      end

      def write16(val)
	val = (-val | 0x8000) if val < 0

	buffer = []
	@io.putc((val >> 8) & 0xff)
	@io.putc(val & 0xff)
	@bytes_written += 2
      end

      def write32(val)
	val = (-val | 0x80000000) if val < 0

	@io.putc((val >> 24) & 0xff)
	@io.putc((val >> 16) & 0xff)
	@io.putc((val >> 8) & 0xff)
	@io.putc(val & 0xff)
	@bytes_written += 4
      end

      def write_bytes(bytes)
	bytes.each { |b| @io.putc(b) }
	bytes.length
      end
    end

  end
end
