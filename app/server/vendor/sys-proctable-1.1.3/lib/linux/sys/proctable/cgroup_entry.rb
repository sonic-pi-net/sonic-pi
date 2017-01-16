module Sys
  class ProcTable
    # This represents a cgroup entry
    #
    # Have a look at `man 5 proc` on a linux distribution, to get some more
    # information about the lines and their fields in `/proc/[pid]/cgroup`.
    #
    # Example:
    #
    #   entry = CgroupEntry.new '7:devices:/init.scope'
    #   entry.hierarchy_id  # => 7
    #   entry.subsystems    # => ['devices']
    #   entry.control_group # => '/init.scope'
    #
    class CgroupEntry
      # Create a new cgroup entry object
      #
      # This expects a string of '7:devices:/init.scope' - see `man 5 proc` for a
      # reference.
      def initialize(string)
        @string = string.chomp
        @fields = @string.split(/:/)
      rescue
        @fields = []
      end

      # This returns the hierarchy id of the cgroup entry
      def hierarchy_id
        @fields[0].to_i
      end

      # Return sets of subsystems bound to the hierarchy
      def subsystems
        @fields[1].split(/,/)
      rescue
        []
      end

      # control group in the hierarchy to which the process belongs
      def control_group
        @fields[2]
      end

      # Return the line itself
      def to_s
        @string
      end
    end
  end
end
