module Sys
  class ProcTable
    # Smaps represents a process' memory size for all mapped files
    #
    # A single mapped file memory entry looks like this:
    #
    #   00400000-004d4000 r-xp 00000000 fd:00 785                      /bin/bash
    #   Size:                848 kB
    #   Rss:                 572 kB
    #   Pss:                 572 kB
    #   Shared_Clean:          0 kB
    #   Shared_Dirty:          0 kB
    #   Private_Clean:       572 kB
    #   Private_Dirty:         0 kB
    #   Referenced:          572 kB
    #   Anonymous:             0 kB
    #   AnonHugePages:         0 kB
    #   Swap:                  0 kB
    #   KernelPageSize:        4 kB
    #   MMUPageSize:           4 kB
    #
    # Have a look at `man 5 proc` on a linux distribution, to get some more
    # information about the lines and fields in `/proc/[pid]/smaps`.
    #
    # Example:
    #
    #   smaps = Smaps.new(123, IO.read("/proc/1234/smaps")
    #     => #<Sys::ProcTable::Smaps:0x007f8ac5930768 @pid=123, @pss=107000, @rss=368000, @uss=96000, @swap=192000, @vss=136752000>
    #   smaps.pss  # => 109568
    #   smaps.rss  # => 376832
    #   smaps.uss  # => 98304
    #   smaps.swap # => 196608
    #   smaps.vss  # => 140034048
    #
    class Smaps

      # Process ID for this smaps
      attr_reader :pid

      # Proportional set size
      #
      # PSS is the size of private pages added to each shared mapping's size
      # divided by the number of processes that share it.  It is meant to
      # provide a better representation of the amount of memory actually used
      # by a process.
      #
      # If a process has 4k of private pages, 4k of shared pages shared with one
      # other process, and 3k of pages shared with two other processes, the PSS
      # is:
      #
      # 4k + (4k / 2) + (3k / 3) = 7k
      #
      attr_reader :pss
      alias_method :proportional_set_size, :pss

      # Resident set size
      #
      # RSS is the total size of all pages, shared or not, mapped to a process.
      attr_reader :rss
      alias_method :resident_set_size, :rss

      # Unique set size
      #
      # USS is the total size of all private pages mapped to a process.
      attr_reader :uss
      alias_method :unique_set_size, :uss

      # Swap
      #
      # Swap is the total size of all swapped pages mapped to a process.
      attr_reader :swap

      # Virtual set size
      #
      # VSS is the total accessible address space in a process.  Since files are
      # lazily loaded, this value represents the total size of all mapped files
      # if they were all loaded.
      attr_reader :vss
      alias_method :virtual_set_size, :vss

      # Create a new smaps object
      #
      #
      # This expects a process id and a string containing the contents of
      # /proc/PID/smaps - see `man 5 proc` for a reference.
      #
      # The smaps contents are parsed and memory sizes are calculated in bytes.
      def initialize(pid, smaps_contents)
        @pid            = pid
        @pss            = 0
        @rss            = 0
        @uss            = 0
        @swap           = 0
        @vss            = 0
        smaps_contents.each_line { |line| parse_smaps_line(line) }
      end

      alias_method :to_s, :inspect

      private

      def parse_smaps_line(line)
        case line
        when /^Pss:\s+?(\d+)/
          @pss += Regexp.last_match[1].to_i * 1000
        when /^Rss:\s+?(\d+)/
          @rss += Regexp.last_match[1].to_i * 1000
        when /^Size:\s+?(\d+)/
          @vss += Regexp.last_match[1].to_i * 1000
        when /^Swap:\s+?(\d+)/
          @swap += Regexp.last_match[1].to_i * 1000
        when /^Private_(Clean|Dirty):\s+?(\d+)/
          @uss += Regexp.last_match[2].to_i * 1000
        end
      end
    end
  end
end
