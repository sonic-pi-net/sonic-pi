module Aubio
  class Pitches

    def initialize(aubio_source, params)
      # TODO: cleanup param dups
      @sample_rate = params[:sample_rate] || 44100
      @window_size = params[:window_size] || 1024
      @hop_size    = params[:hop_size]    || 512

      # Set the tolerance for the pitch detection algorithm.
      # Typical values range between 0.2 and 0.9.
      # Pitch candidates found with a confidence less than this threshold will not be selected.
      # The higher the threshold, the more confidence in the candidates.
      @confidence_thresh  = params[:confidence_thresh]  || 0.9

      @pitch_method = params[:pitch_method] || "yinfast"

      @source = aubio_source
      @pitch = Api.new_aubio_pitch(@pitch_method, @window_size, @hop_size, @sample_rate)
      Api.aubio_pitch_set_unit(@pitch, 'midi')
      Api.aubio_pitch_set_tolerance(@pitch, @confidence_thresh)

      # create output for source
      @sample_buffer = Api.new_fvec(@hop_size)
      # create output for pitch and beat
      @out_fvec = Api.new_fvec(1)
    end

    def each
      return enum_for(:each) unless block_given?

      total_frames_counter = 0
      read_buffer = FFI::MemoryPointer.new(:int)
      last_pitch = 0

      loop do
        # Perform pitch calculation
        Api.aubio_source_do(@source, @sample_buffer, read_buffer)
        Api.aubio_pitch_do(@pitch, @sample_buffer, @out_fvec)

        # Retrieve result
        pitch = Api.fvec_get_sample(@out_fvec, 0)
        confidence = Api.aubio_pitch_get_confidence(@pitch)
        no_of_bytes_read = read_buffer.read_int
        total_frames_counter += no_of_bytes_read

        if (last_pitch - pitch).abs >= 1 and confidence > @confidence_thresh
          output = {
            :pitch => pitch,
            :confidence => confidence,
            :start => (total_frames_counter == 0 ? 1 : 0),
            :end => 0
          }
          yield output
        end

        last_pitch = pitch

        if no_of_bytes_read != @hop_size
          # there's no more audio to look at

          # Let's output one last pitch to mark the end of the file
          total_time = total_frames_counter.to_f / @sample_rate.to_f
          output = {
            :pitch => pitch,
            :confidence => confidence,
            :start => 0,
            :end  => 1
          }
          yield output

          # clean up
          Api.del_aubio_pitch(@pitch)
          Api.del_fvec(@sample_buffer)
          Api.del_fvec(@out_fvec)

          break
        end
      end
    end

  end
end
