# sys-proctable

## Description

  A Ruby interface for gathering process information.

## Prerequisites

* RSpec 3.x (development only)

## Supported Platforms

* Windows 2000 or later
* Linux 2.6+
* FreeBSD
* Solaris 8+
* OS X 10.7+
* AIX 5.3+

## Installation

```sh
gem install sys-proctable
```

For version 1.1.5 or earlier, you may need to specify a platform in some cases. For example:

```sh
gem install sys-proctable --platform mswin32 # Windows
gem install sys-proctable --platform sunos   # Solaris
gem install sys-proctable --platform linux   # Linux
gem install sys-proctable --platform freebsd # FreeBSD
gem install sys-proctable --platform darwin  # OS X
```

## Synopsis

```ruby
require 'sys/proctable'
include Sys

# Everything
ProcTable.ps{ |p|
  puts p.pid.to_s
  puts p.comm
  # ...
}

# Just one process
s = ProcTable.ps(pid: 2123)
puts s.pid.to_s
puts s.comm
# ...

# Return the results as an array of ProcTableStructs
a = ProcTable.ps
a.each do |p|
  puts p.pid
  # ...
end
```

## Notes

Various platforms support different options. Mostly this is to let you
skip the collection of certain bits of information in order to improve
speed and/or reduce memory. For example on Linux you can do this to
skip the collection of smaps information:

```ruby
Sys::ProcTable.ps(smaps: false)
```

Windows users may send a host name to get process information from a
different host. This relies on the WMI service running.

```ruby
Sys::ProcTable.ps(host: some_host)
```

## Known Issues

### FreeBSD

A kvm interface is used. That means the owner of the process using the
sys-proctable library needs to be a member of the kvm group (or root).

### Bundler

For version 1.1.5 or earlier, Bundler seems to have trouble installing the
proper gem because of the platform specific gem names. To deal with that,
run this command first:

```sh
bundle config specific_platform true
```

You should not have to do this for version 1.2.0 or later.

### Solaris

The cmdline member on Solaris is limited to 80 characters unless you (or
your program) own the process. This is a Solaris design flaw/feature.

### OS X

The libproc interface is used. That means you will only get list of
processes that you have access to. To get a full listing, run as root.

## Future Plans

Support for Solaris will probably be dropped in the next major release.

## Acknowledgements

This library was originally based on the Perl module Proc::ProcessTable
by Dan Urist. Many ideas, as well as large chunks of code, were taken
from his work. So, a big THANK YOU goes out to Dan Urist.

A big thanks also goes out to Mike Hall who was very helpful with ideas,
logic and testing.

Thanks also go to Sean Chittenden for providing an account on one of his
FreeBSD machines. This is how the FreeBSD support was (initially) added.

Thanks go to James Hranicky for providing a patch that grabs name, eid,
euid, gid and guid info in the Linux version, along with some general
debugging help.

Thanks go to David Felstead for the original OS X code. Thanks also go
to Matthias Zirnstein for adding the original cmdline support for OS X.

Finally I'd like to thank all the folks who have submitted bug reports
and/or patches.

## Help Wanted

I do not have access to all platforms. If your platform is not supported
then you will need to either submit a patch or give me a remote account
on a box with a compiler so that I can write the code.

## More documentation

See the documentation under the 'doc' directory for more information,
including platform specific notes and issues.

## License

Apache 2.0

## Copyright

(C) 2003-2019 Daniel J. Berger
All Rights Reserved.

## Author

Daniel J. Berger
