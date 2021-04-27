require 'sys/proctable'

# The Sys module serves as a namespace only
module Sys

  # The Top class serves as a toplevel name for the 'top' method.
  class Top

    # The version of the sys-top library
    VERSION = '1.0.5'.freeze

    # Returns an array of Struct::ProcTableStruct elements containing up
    # to +num+ elements, sorted by +field+. The default number of elements
    # is 10, while the default field is 'pctcpu'.
    #
    # Exception: the default sort field is 'pid' on AIX, Darwin and Windows.
    #
    def self.top(num=10, field='pctcpu')
      field = field.to_s if field.is_a?(Symbol)

      aix = RbConfig::CONFIG['host_os'] =~ /aix/i
      darwin = RbConfig::CONFIG['host_os'] =~ /darwin/i

      # Sort by pid on Windows and AIX by default
      if (File::ALT_SEPARATOR || aix || darwin) && field == 'pctcpu'
        field = 'pid'
      end

      Sys::ProcTable.ps.sort_by{ |obj| obj.send(field) || '' }[0..num-1]
    end
  end
end
