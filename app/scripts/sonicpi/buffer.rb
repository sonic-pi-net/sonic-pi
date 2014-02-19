module SonicPi
  class Buffer
    attr_reader :id, :num_frames, :num_chans, :sample_rate, :duration
    def initialize(id, num_frames, num_chans, sample_rate)
      @id = id
      @num_frames = num_frames
      @num_chans = num_chans
      @sample_rate = sample_rate
      @duration = num_frames.to_f / sample_rate.to_f
    end
  end
end
