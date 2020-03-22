module Aubio
	class Onsets

		def initialize(aubio_source, params)
      # TODO: cleanup param dups
			@sample_rate = params[:sample_rate] || 44100
			@window_size = params[:window_size] || 1024
			@hop_size    = params[:hop_size]    || 512

			@source = aubio_source
			@onset = Api.new_aubio_onset('default', @window_size, @hop_size, @sample_rate)
			Api.aubio_onset_set_minioi_ms(@onset, 12.0)
			Api.aubio_onset_set_threshold(@onset, 0.3)

			# create output for source
			@sample_buffer = Api.new_fvec(@hop_size)
			# create output for pitch and beat
			@out_fvec = Api.new_fvec(1)
		end

		def each
			return enum_for(:each) unless block_given?

			total_frames_counter = 0
			read_buffer = FFI::MemoryPointer.new(:int)

			loop do
				# Perform onset calculation
        Api.aubio_source_do(@source, @sample_buffer, read_buffer)
				Api.aubio_onset_do(@onset, @sample_buffer, @out_fvec)

        # Retrieve result
				onset_new_peak = Api.fvec_get_sample(@out_fvec, 0)
        no_of_bytes_read = read_buffer.read_int
        total_frames_counter += no_of_bytes_read

        if onset_new_peak > 0.0
					onset_seconds = Api.aubio_onset_get_last_s(@onset)
					onset_milliseconds = Api.aubio_onset_get_last_ms(@onset)
					output = {
            :s => onset_seconds,
            :ms => onset_milliseconds,
            :start => (onset_seconds == 0.0 ? 1 : 0),
            :end => 0
					}
          yield output
				end

        if no_of_bytes_read != @hop_size
          # there's no more audio to look at

          # Let's output one last onset to mark the end of the file
          total_time = total_frames_counter.to_f / @sample_rate.to_f
          output = {
            :s => total_time,
            :ms => total_time/1000.0,
            :start => 0,
            :end  => 1
          }
          yield output

          # clean up
          Api.del_aubio_onset(@onset)
          Api.del_fvec(@sample_buffer)
          Api.del_fvec(@out_fvec)

          break
        end
			end
		end

	end
end
