# SPDX-License-Identifier: AGPL-3.0-or-later
# Frame count, channels and rate from a sound file's header: FLAC STREAMINFO
# or a RIFF WAVE fmt/data pair. For hosts with files (the CLI); the browser
# gets the same numbers from SuperSonic.
module SonicPi
  module AudioMeta
    def self.read(path)
      bytes = File.open(path, "rb") { |f| f.read }
      magic = bytes.byteslice(0, 4)
      return flac(bytes) if magic == "fLaC"
      return wav(bytes) if magic == "RIFF"
      raise "unknown sound file: #{path}"
    end

    def self.flac(bytes)
      header = bytes.byteslice(4, 4).unpack("N")[0]      # 1 bit last, 7 bits type, 24 bits length
      raise "no STREAMINFO" unless (header >> 24) & 0x7f == 0
      info = bytes.byteslice(8, header & 0xffffff)
      hi = info.byteslice(10, 4).unpack("N")[0]           # rate 20 | chans-1 3 | bps-1 5 | total samples 36, as two words
      lo = info.byteslice(14, 4).unpack("N")[0]
      { sample_rate: hi >> 12, num_chans: ((hi >> 9) & 0x7) + 1, num_frames: ((hi & 0xf) << 32) | lo }
    end

    def self.wav(bytes)
      raise "not WAVE" unless bytes.byteslice(8, 4) == "WAVE"
      pos = 12
      meta = {}
      while pos + 8 <= bytes.bytesize
        id = bytes.byteslice(pos, 4)
        size = bytes.byteslice(pos + 4, 4).unpack("V")[0]
        if id == "fmt "
          fmt = bytes.byteslice(pos + 8, size)
          meta[:num_chans] = fmt.byteslice(2, 2).unpack("v")[0]
          meta[:sample_rate] = fmt.byteslice(4, 4).unpack("V")[0]
          meta[:block_align] = fmt.byteslice(12, 2).unpack("v")[0]
        elsif id == "data"
          meta[:num_frames] = size / meta[:block_align]
          break
        end
        pos += 8 + size + (size & 1)
      end
      { sample_rate: meta[:sample_rate], num_chans: meta[:num_chans], num_frames: meta[:num_frames] }
    end
  end
end
