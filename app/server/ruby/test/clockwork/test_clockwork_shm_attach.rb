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

# The attach handshake and the mapping, against a fake endpoint that speaks
# the engine's protocol: connect, receive one hello with the descriptor
# riding alongside, map it read-only.

require_relative "../setup_test"
require_relative "fake_segment"
require_relative "../../lib/sonicpi/clockwork_shm_attach"

module SonicPi
  class ClockworkShmAttachTester < Minitest::Test
    F = FakeClockworkSegment
    A = ClockworkShmAttach

    def setup
      skip "Unix sockets only" if A.windows?
    end

    def with_env(vars)
      saved = vars.keys.to_h { |k| [k, ENV[k]] }
      vars.each { |k, v| v.nil? ? ENV.delete(k) : ENV[k] = v }
      yield
    ensure
      saved.each { |k, v| v.nil? ? ENV.delete(k) : ENV[k] = v }
    end

    # ── the endpoint name, derived from the port as the engine derives it ──

    def test_endpoint_under_the_session_runtime_dir
      with_env("XDG_RUNTIME_DIR" => "/run/user/1000/", "TMPDIR" => "/tmp/x") do
        assert_equal "/run/user/1000/clockwork-shm-4556.sock", A.default_endpoint(4556)
      end
    end

    def test_endpoint_falls_back_to_tmpdir
      with_env("XDG_RUNTIME_DIR" => nil, "TMPDIR" => "/var/folders/ab/T//") do
        assert_equal "/var/folders/ab/T/clockwork-shm-4556.sock", A.default_endpoint(4556)
      end
    end

    def test_endpoint_is_uid_tagged_under_tmp_when_neither_is_set
      with_env("XDG_RUNTIME_DIR" => nil, "TMPDIR" => nil) do
        assert_equal "/tmp/clockwork-shm-#{Process.euid}-4556.sock", A.default_endpoint(4556)
      end
    end

    def test_endpoint_on_windows_is_a_named_pipe
      assert_equal "\\\\.\\pipe\\clockwork-shm-4556", A.default_endpoint(4556, windows: true)
    end

    # ── the handshake ─────────────────────────────────────────────────────

    def test_receives_the_descriptor_and_the_segment_size
      served = F::Served.new(F.build)
      io, size = A.receive(served.path)
      assert_kind_of IO, io
      assert_equal served.file.size, size
      assert_equal 1, served.endpoint.connections
    ensure
      io&.close
      served&.close
    end

    def test_maps_the_segment_read_only_and_reads_it_back
      bytes = F.build(bpm: 99.5)
      served = F::Served.new(bytes)
      io, size = A.receive(served.path)
      mem = A.map(io, size)
      assert_equal bytes.bytesize, mem.size
      assert_equal bytes[0, 12], mem.bytes(0, 12)
      off = F.clock_state_offset
      assert_equal 99.5, mem.bytes(off, 8).unpack1("E")
      # Live: the server rewrites, the mapping sees it.
      served.set_clock(bpm: 77.0, origin_ntp: 1.0)
      assert_equal 77.0, mem.bytes(off, 8).unpack1("E")
      mem.unmap
      assert_raises(A::AttachError) { mem.bytes(0, 4) }
    ensure
      io&.close
      served&.close
    end

    def test_refuses_a_read_past_the_end_of_the_mapping
      served = F::Served.new(F.build)
      io, size = A.receive(served.path)
      mem = A.map(io, size)
      assert_raises(A::AttachError) { mem.bytes(size - 2, 4) }
    ensure
      mem&.unmap
      io&.close
      served&.close
    end

    def test_no_endpoint_is_a_connect_failure
      err = assert_raises(A::AttachError) { A.receive("/nonexistent/dir/clockwork-shm-1.sock") }
      assert_match(/connect/, err.message)
    end

    def test_a_wrong_magic_is_a_malformed_hand_off
      served = F::Served.new(F.build, magic: 0xDEADBEEF)
      err = assert_raises(A::AttachError) { A.receive(served.path) }
      assert_match(/malformed hand-off/, err.message)
    ensure
      served&.close
    end

    def test_a_wrong_version_is_a_malformed_hand_off
      served = F::Served.new(F.build, version: 2)
      err = assert_raises(A::AttachError) { A.receive(served.path) }
      assert_match(/malformed hand-off/, err.message)
    ensure
      served&.close
    end

    def test_a_short_hello_without_a_descriptor_is_a_malformed_hand_off
      served = F::Served.new(F.build, short: true)
      err = assert_raises(A::AttachError) { A.receive(served.path) }
      assert_match(/malformed hand-off/, err.message)
    ensure
      served&.close
    end

    def test_a_server_that_never_speaks_does_not_hang_the_caller
      served = F::Served.new(F.build, silent: true)
      t0 = Time.now
      err = assert_raises(A::AttachError) { A.receive(served.path, timeout: 0.2) }
      assert Time.now - t0 < 1.0, "receive waited #{Time.now - t0}s"
      assert_match(/timed out|malformed/, err.message)
    ensure
      served&.close
    end

    # The spider is started with RubyGems disabled (daemon.rb). fiddle is a
    # bundled gem from Ruby 3.5, so the reader has to find it by itself: this
    # runs the very Ruby the tests run under, the way the daemon runs the
    # spider, and requires the reader.
    def test_the_reader_loads_with_rubygems_disabled_as_the_spider_runs
      lib = File.expand_path("../../lib/sonicpi/clockwork_shm_attach.rb", __dir__)
      out = IO.popen([RbConfig.ruby, "--disable=gems", "--enable-frozen-string-literal",
                      "-e", "require #{lib.inspect}; print Fiddle::VERSION"],
                     err: [:child, :out], &:read)
      assert $?.success?, "spider-style load failed:\n#{out}"
      assert_match(/\A\d+\.\d+/, out)
    end

    def test_the_hello_is_twenty_four_bytes_in_native_order
      # The wire format the engine sends: magic 'CWAT', version 1, u64 size, u64 handle.
      assert_equal 0x43574154, A::HELLO_MAGIC
      assert_equal 1,          A::HELLO_VERSION
      assert_equal 24,         A::HELLO_BYTES
    end
  end
end
