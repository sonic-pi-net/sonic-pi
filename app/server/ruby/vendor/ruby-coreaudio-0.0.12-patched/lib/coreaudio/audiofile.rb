
module CoreAudio
  class AudioFile
    def read(frames=nil)
      if frames
        frames = Integer(frames)
        if frames and frames > 0
          return read_frames(frames)
        elsif frames == 0
          return NArray.sint(0)
        else
          raise ArgumentError,
            "coreaudio: read frame number must be zero or positive"
        end
      end

      # read all frames
      chunk = self.inner_rate.to_i * 10
      total = nil
      loop do
        tmp = read_frames(chunk)
        if tmp.nil?
          break
        end
        if total.nil?
          total = tmp
        else
          new_na = NArray.sint(total.shape[0], tmp.shape[1] + total.shape[1])
          new_na[false, 0...total.shape[1]] = total
          new_na[false, total.shape[1]..-1] = tmp
          total = new_na
        end
      end

      total
    end
  end
end
