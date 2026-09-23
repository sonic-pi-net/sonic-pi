#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/sonic-pi-net/sonic-pi
# License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require 'socket'
require 'rbconfig'

# The spider runs with RubyGems disabled (daemon.rb: --disable=gems), and
# from Ruby 3.5 fiddle is a bundled gem rather than part of the standard
# library, so a plain require finds nothing there. It still ships with every
# Ruby; find it under the one that is running — its lib directory and the
# directory its compiled extension lives in — and try again. Older Rubies
# (the AppImage's 3.4, a distro's) have it on the load path already.
begin
  require 'fiddle'
rescue LoadError
  gems_root = File.join(RbConfig::CONFIG["rubylibprefix"], "gems", RbConfig::CONFIG["ruby_version"])
  lib = Dir.glob(File.join(gems_root, "gems", "fiddle-*", "lib")).max
  ext = Dir.glob(File.join(gems_root, "extensions", "*", "*", "fiddle-*")).max
  raise if lib.nil?
  $LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
  $LOAD_PATH.unshift(ext) if ext && !$LOAD_PATH.include?(ext)
  require 'fiddle'
end

module SonicPi
  # How a Ruby process gets the engine's anonymous shared-memory segment.
  #
  # The segment has no name, so it cannot be opened: the engine serves it on
  # an attach endpoint (clockwork's shm_attach.hpp). Connect, receive one
  # 24-byte hello with the descriptor riding alongside as SCM_RIGHTS, map it
  # read-only. On Windows the hello carries a handle the engine duplicated
  # into this process and the channel is a named pipe. No request, no state:
  # attaching is the connection.
  #
  # Nothing here is compiled. Ruby's socket library receives descriptors, and
  # Fiddle reaches mmap (or MapViewOfFile) in the C library, which is what
  # lets the reader ship inside a Debian package that strips native
  # extensions.
  module ClockworkShmAttach
    HELLO_MAGIC   = 0x43574154   # 'CWAT'
    HELLO_VERSION = 1
    HELLO_BYTES   = 24           # u32 magic, u32 version, u64 size, u64 handle

    class AttachError < StandardError; end

    def self.windows?
      !!(RUBY_PLATFORM =~ /mswin|mingw|cygwin/)
    end

    # The endpoint an engine on `port` serves unless told otherwise — the
    # same derivation as shm_attach::default_endpoint, so a reader that only
    # knows the port still meets it.
    def self.default_endpoint(port, windows: windows?)
      return "\\\\.\\pipe\\clockwork-shm-#{port}" if windows
      dir = dir_from("XDG_RUNTIME_DIR") || dir_from("TMPDIR")
      return "/tmp/clockwork-shm-#{Process.euid}-#{port}.sock" unless dir
      "#{dir}/clockwork-shm-#{port}.sock"
    end

    def self.dir_from(var)
      v = ENV[var]
      return nil if v.nil? || v.empty?
      v = v.chomp("/") while v.length > 1 && v.end_with?("/")
      v
    end
    private_class_method :dir_from

    # Connect and take the hand-off. Returns [io_or_handle, size].
    def self.receive(endpoint, timeout: 2.0)
      windows? ? Win32.receive(endpoint) : Posix.receive(endpoint, timeout)
    end

    # Map the received segment read-only. The descriptor may be closed after.
    def self.map(io_or_handle, size)
      windows? ? Win32.map(io_or_handle, size) : Posix.map(io_or_handle, size)
    end

    # Read-only view of the mapped segment. `bytes(offset, len)` is the whole
    # interface a reader needs (ClockworkArena::StringMemory matches it).
    class MappedMemory
      attr_reader :size

      def initialize(pointer, size, unmapper)
        @ptr, @size, @unmapper = pointer, size, unmapper
      end

      def bytes(offset, len)
        raise AttachError, "segment is unmapped" unless @ptr
        if offset < 0 || len < 0 || offset + len > @size
          raise AttachError, "read of #{len} at #{offset} runs past the #{@size}-byte segment"
        end
        @ptr[offset, len]
      end

      def unmap
        return unless @ptr
        @unmapper.call(@ptr, @size)
        @ptr = nil
      end

      def mapped?
        !@ptr.nil?
      end
    end

    module Posix
      PROT_READ  = 1
      MAP_SHARED = 1

      def self.receive(endpoint, timeout)
        sock = begin
          UNIXSocket.new(endpoint)
        rescue SystemCallError => e
          raise AttachError, "connect #{endpoint}: #{e.message}"
        end
        begin
          # Both ends check the peer's uid: memory goes only to its own user.
          uid, = sock.getpeereid
          raise AttachError, "the engine at #{endpoint} is not running as this user" unless uid == Process.euid
          # Bound the wait: a server that accepted but never speaks must not
          # hang the caller.
          unless IO.select([sock], nil, nil, timeout)
            raise AttachError, "timed out waiting for the hand-off from #{endpoint}"
          end
          data, _addr, _flags, *controls = begin
            sock.recvmsg(HELLO_BYTES, 0, 256, scm_rights: true)
          rescue SystemCallError, SocketError => e
            raise AttachError, "malformed hand-off from #{endpoint}: #{e.message}"
          end
          ios = controls.flat_map { |c| c.unix_rights || [] rescue [] }
          unless data && data.bytesize == HELLO_BYTES
            ios.each { |io| io.close rescue nil }
            raise AttachError, "malformed hand-off from #{endpoint} (#{data.to_s.bytesize} bytes)"
          end
          magic, version, size, _handle = data.unpack("L<2Q<2")
          if magic != HELLO_MAGIC || version != HELLO_VERSION || ios.empty?
            ios.each { |io| io.close rescue nil }
            raise AttachError, "malformed hand-off from #{endpoint}"
          end
          ios[1..].each { |io| io.close rescue nil }
          [ios[0], size]
        ensure
          sock.close rescue nil
        end
      end

      def self.map(io, size)
        libc = Fiddle.dlopen(nil)
        mmap = Fiddle::Function.new(libc["mmap"],
                                    [Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T, Fiddle::TYPE_INT,
                                     Fiddle::TYPE_INT, Fiddle::TYPE_INT, Fiddle::TYPE_LONG],
                                    Fiddle::TYPE_VOIDP)
        munmap = Fiddle::Function.new(libc["munmap"],
                                      [Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T], Fiddle::TYPE_INT)
        addr = mmap.call(nil, size, PROT_READ, MAP_SHARED, io.fileno, 0)
        raw = addr.to_i
        if raw == 0 || raw == -1 || raw == (2**64 - 1) || raw == (2**32 - 1)
          raise AttachError, "mmap of the #{size}-byte segment failed"
        end
        ptr = Fiddle::Pointer.new(raw, size)
        MappedMemory.new(ptr, size, ->(p, n) { munmap.call(p, n) })
      end
    end

    # Untested here: this machine is not Windows. Kept to the shape of
    # shm_attach::receive's Windows branch, one call per line.
    module Win32
      GENERIC_READ  = 0x80000000   # a DWORD past a signed int: passed as TYPE_UINT, or Fiddle raises on Windows
      OPEN_EXISTING = 3
      FILE_MAP_READ = 4
      ERROR_PIPE_BUSY = 231

      def self.kernel32
        @kernel32 ||= Fiddle.dlopen("kernel32.dll")
      end

      def self.fn(name, args, ret)
        Fiddle::Function.new(kernel32[name], args, ret)
      end

      def self.receive(endpoint)
        name = endpoint.start_with?("\\\\.\\pipe\\") ? endpoint : "\\\\.\\pipe\\#{endpoint}"
        wide = (name + "\0").encode("UTF-16LE")
        create = fn("CreateFileW", [Fiddle::TYPE_VOIDP, Fiddle::TYPE_UINT, Fiddle::TYPE_INT, Fiddle::TYPE_VOIDP,
                                    Fiddle::TYPE_INT, Fiddle::TYPE_INT, Fiddle::TYPE_VOIDP], Fiddle::TYPE_VOIDP)
        wait   = fn("WaitNamedPipeW", [Fiddle::TYPE_VOIDP, Fiddle::TYPE_INT], Fiddle::TYPE_INT)
        last   = fn("GetLastError", [], Fiddle::TYPE_INT)
        read   = fn("ReadFile", [Fiddle::TYPE_VOIDP, Fiddle::TYPE_VOIDP, Fiddle::TYPE_INT, Fiddle::TYPE_VOIDP,
                                 Fiddle::TYPE_VOIDP], Fiddle::TYPE_INT)
        close  = fn("CloseHandle", [Fiddle::TYPE_VOIDP], Fiddle::TYPE_INT)
        invalid = 2**64 - 1
        pipe = invalid
        3.times do
          pipe = create.call(wide, GENERIC_READ, 0, nil, OPEN_EXISTING, 0, nil).to_i
          break if pipe != invalid && pipe != -1
          break unless last.call == ERROR_PIPE_BUSY
          wait.call(wide, 1000)
        end
        raise AttachError, "open #{endpoint}: error #{last.call}" if pipe == invalid || pipe == -1
        buf = Fiddle::Pointer.malloc(HELLO_BYTES)
        got = Fiddle::Pointer.malloc(4)
        ok = read.call(pipe, buf, HELLO_BYTES, got, nil)
        close.call(pipe)
        n = got[0, 4].unpack1("L<")
        raise AttachError, "malformed hand-off from #{endpoint}" if ok == 0 || n != HELLO_BYTES
        magic, version, size, handle = buf[0, HELLO_BYTES].unpack("L<2Q<2")
        raise AttachError, "malformed hand-off from #{endpoint}" if magic != HELLO_MAGIC || version != HELLO_VERSION
        raise AttachError, "the engine could not duplicate its segment into this process" if handle == 0
        [handle, size]
      end

      def self.map(handle, size)
        mapv  = fn("MapViewOfFile", [Fiddle::TYPE_VOIDP, Fiddle::TYPE_INT, Fiddle::TYPE_INT, Fiddle::TYPE_INT,
                                     Fiddle::TYPE_SIZE_T], Fiddle::TYPE_VOIDP)
        unmap = fn("UnmapViewOfFile", [Fiddle::TYPE_VOIDP], Fiddle::TYPE_INT)
        addr = mapv.call(Fiddle::Pointer.new(handle), FILE_MAP_READ, 0, 0, size).to_i
        raise AttachError, "MapViewOfFile of the #{size}-byte segment failed" if addr == 0
        ptr = Fiddle::Pointer.new(addr, size)
        MappedMemory.new(ptr, size, ->(p, _n) { unmap.call(p) })
      end
    end
  end
end
