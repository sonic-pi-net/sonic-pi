require 'rbconfig'

case RbConfig::CONFIG['host_os']
  when /aix/i
    require_relative '../aix/sys/proctable'
  when /darwin/i
    require_relative '../darwin/sys/proctable'
  when /freebsd/i
    require_relative '../freebsd/sys/proctable'
  when /linux/i
    require_relative '../linux/sys/proctable'
  when /sunos|solaris/i
    require_relative '../sunos/sys/proctable'
  when /mswin|win32|dos|cygwin|mingw|windows/i
    require_relative '../windows/sys/proctable'
  else
    raise "Unsupported platform"
end
