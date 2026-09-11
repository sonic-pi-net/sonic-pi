#--
# Integration test: TcpOscClient against a real SuperSonic engine over TCP.
# Boots the engine headless with --tcp (path via SUPERSONIC_BIN, else the
# vendored external build), exercises: /status round trip, tokened /clock
# RPC, timestamped bundle scheduling (/sync via bundle), notify subscribe
# push, frame-atomicity under concurrent senders, and RTT sanity.
# Skips (with message) when no engine binary is available.
#++
require 'minitest/autorun'
require 'socket'
require 'timeout'
require_relative '../lib/sonicpi/osc/tcp_osc_client'
require_relative '../lib/sonicpi/osc/osc_types'
require_relative '../lib/sonicpi/promise'

class TcpOscClientIntegrationTest < Minitest::Test
  DEFAULT_ENGINE_CANDIDATES = [
    # Vendored external build (Windows / unix artefact layouts), else the
    # staged app binary. Override with SUPERSONIC_BIN.
    '../../../external/supersonic/build/native/SuperSonic_artefacts/Release/SuperSonic.exe',
    '../../../external/supersonic/build/native/SuperSonic_artefacts/Release/SuperSonic',
    '../../native/Sonic Pi - SuperSonic.exe',
    '../../native/Sonic Pi - SuperSonic'
  ].freeze
  ENGINE = ENV['SUPERSONIC_BIN'] ||
           DEFAULT_ENGINE_CANDIDATES
             .map { |p| File.expand_path(p, __dir__) }
             .find { |p| File.exist?(p) } ||
           File.expand_path(DEFAULT_ENGINE_CANDIDATES.first, __dir__)

  def free_port
    s = TCPServer.new('127.0.0.1', 0)
    p = s.addr[1]
    s.close
    p
  end

  def setup
    skip "no engine binary at #{ENGINE} (set SUPERSONIC_BIN)" unless File.exist?(ENGINE)
    @tcp_port = free_port
    @shm_port = free_port
    @pid = spawn(ENGINE, '--headless', '-u', @shm_port.to_s,
                 '--tcp', @tcp_port.to_s, '--max-connections', '16',
                 %i[out err] => File::NULL)
    @client = SonicPi::OSC::TcpOscClient.new('127.0.0.1', @tcp_port,
                                             name: 'test-client', connect_timeout: 30)
  end

  def teardown
    @client&.stop
    if @pid
      Process.kill('TERM', @pid) rescue Process.kill('KILL', @pid) rescue nil
      Process.wait(@pid) rescue nil
    end
  end

  def await(promise, timeout = 5)
    promise.get(timeout)
  end

  def test_status_round_trip
    p = SonicPi::Promise.new
    @client.add_method('/status.reply') { |args| p.deliver!(args) unless p.delivered? }
    @client.send(nil, nil, '/status')
    args = await(p)
    refute_nil args
  end

  def test_clock_rpc_token_round_trip
    p = SonicPi::Promise.new
    @client.add_method('/clockwork/clock/rpc/time_at_beat.reply') { |args| p.deliver!(args) unless p.delivered? }
    token = 424_242
    @client.send(nil, nil, '/clockwork/clock/rpc/time_at_beat',
                 SonicPi::OSC::Int64.new(4_000_000), 4.0, token)
    args = await(p)
    assert_equal token, args.last, 'engine must echo the correlation token'
    assert_kind_of Integer, args[0]
  end

  def test_timestamped_bundle_schedules
    p = SonicPi::Promise.new
    @client.add_method('/synced') { |args| p.deliver!(args) unless p.delivered? }
    # /sync inside a near-future timestamped bundle: engine must hold it
    # until due, then reply /synced with our id.
    ts = Time.now + 0.2
    @client.send_ts(ts, nil, nil, '/sync', 77)
    t0 = Time.now
    args = await(p)
    assert_equal 77, args[0]
    assert_operator (Time.now - t0), :>=, 0.1, 'reply must not arrive before the bundle is due'
  end

  def test_notify_subscribe_receives_reply_on_connection
    p = SonicPi::Promise.new
    @client.add_method('/clockwork/clock/notify/subscribe.reply') { |args| p.deliver!(args) unless p.delivered? }
    @client.send(nil, nil, '/clockwork/clock/notify/subscribe', 99)
    args = await(p)
    assert_equal 99, args.last
  end

  def test_concurrent_senders_do_not_interleave_frames
    got = Queue.new
    @client.add_method('/synced') { |args| got << args[0] }
    ids = (1..200).to_a
    ids.each_slice(50).map do |slice|
      Thread.new { slice.each { |i| @client.send(nil, nil, '/sync', i) } }
    end.each(&:join)
    received = ids.map { Timeout.timeout(10) { got.pop } }
    assert_equal ids.sort, received.sort, 'every /sync must survive concurrent framing'
  end

  def test_rtt_sanity
    p = nil
    @client.add_method('/status.reply') { |_| p.deliver!(true) if p && !p.delivered? }
    rtts = 20.times.map do
      p = SonicPi::Promise.new
      t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
      @client.send(nil, nil, '/status')
      await(p)
      Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0
    end
    median = rtts.sort[rtts.size / 2]
    assert_operator median, :<, 0.05, "median TCP RTT should be well under 50ms (got #{(median * 1000).round(1)}ms)"
  end
end
