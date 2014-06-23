require 'midilib'
module SonicPi
  module Mods
    module SPMIDI
      def midi_notes(file, track_num=nil)
        seq = MIDI::Sequence.new()
        File.open(file) do |f|
          seq.read f
        end
        idx = track_num || track_idx_with_most_notes(seq)
        seq.tracks[idx].to_a.reject{|i| !i.is_a? MIDI::NoteOn}.map{|i| i.note}
      end

      private

      def track_idx_with_most_notes(seq)
        tracks = seq.tracks
        counts = []
        seq.tracks.each_with_index{|t, i| counts << [i, t.count]}
        counts.sort{|x,y| x[1] <=> y[1]}.reverse[0][0]
      end
    end
  end
end
