module Aubio
	class Beats

		def initialize(aubio_source, params)
      # TODO: cleanup param dups
			@sample_rate = params[:sample_rate] || 44100
			@window_size = params[:window_size] || 1024
			@hop_size    = params[:hop_size]    || 512

			@source = aubio_source
			@tempo = Api.new_aubio_tempo('specdiff', @window_size, @hop_size, @sample_rate)

			# create output for source
			@sample_buffer = Api.new_fvec(@hop_size)
			# create output for beat
			@out_fvec = Api.new_fvec(1)
		end

		def each
			return enum_for(:each) unless block_given?

			total_frames_counter = 0
			read_buffer = FFI::MemoryPointer.new(:int)

			loop do
				# Perform tempo calculation
        Api.aubio_source_do(@source, @sample_buffer, read_buffer)
				Api.aubio_tempo_do(@tempo, @sample_buffer, @out_fvec)

        # Retrieve result
				is_beat = Api.fvec_get_sample(@out_fvec, 0)
        no_of_bytes_read = read_buffer.read_int
        total_frames_counter += no_of_bytes_read

        if is_beat > 0.0
					tempo_seconds = Api.aubio_tempo_get_last_s(@tempo)
					tempo_milliseconds = Api.aubio_tempo_get_last_ms(@tempo)
          tempo_confidence = Api.aubio_tempo_get_confidence(@tempo)

					output = {
            :confidence => tempo_confidence,
            :s => tempo_seconds,
            :ms => tempo_milliseconds,
            :start => (tempo_seconds == 0.0 ? 1 : 0),
            :end => 0
					}
          yield output
				end

        if no_of_bytes_read != @hop_size
          # there's no more audio to look at

          # Let's output one last tempo to mark the end of the file
          total_time = total_frames_counter.to_f / @sample_rate.to_f
          output = {
            :confidence => 1.0,
            :s => total_time,
            :ms => total_time/1000.0,
            :start => 0,
            :end  => 1
          }
          yield output

          # clean up
          Api.del_aubio_tempo(@tempo)
          Api.del_fvec(@sample_buffer)
          Api.del_fvec(@out_fvec)

          break
        end
			end
		end

	end
end
