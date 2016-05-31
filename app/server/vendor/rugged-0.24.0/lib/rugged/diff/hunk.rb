module Rugged
  class Diff
    class Hunk
      def delta
        @owner
      end

      def inspect
        "#<#{self.class.name}:#{object_id} {header: #{header.inspect}, count: #{count.inspect}}>"
      end

      # Returns an Array containing all lines of the hunk.
      def lines
        each_line.to_a
      end
    end
  end
end
