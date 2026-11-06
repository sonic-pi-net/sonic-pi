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

require "fileutils"
require "tmpdir"
require_relative "./setup_test"
require_relative "../lib/sonicpi/studio"
require_relative "./lib/headless_engine"

module SonicPi
  # A sample load that fails ends the run that asked for it with an error
  # naming the file, and leaves nothing behind: the next request for the
  # file loads it afresh. A load with no answer counts as failed.
  #
  # A real Studio against a headless engine. Skipped when the engine is not
  # built, as the mix tests are.
  class SampleLoadFailuresTester < Minitest::Test
    AUDIO = File.join(Paths.samples_path, "bd_haus.flac")

    # Not a RuntimeError, as Timeout::Error is, so a hang cannot pass for a
    # failed load.
    class StillWaiting < StandardError; end

    def setup
      skip "engine not built at #{MixEngine::ENGINE}" unless HeadlessEngine.available?
      @dir = Dir.mktmpdir("sonic-pi-sample-load")
      @engine = HeadlessEngine.new("sample-load-test")
      @studio = Studio.new({scsynth_port: @engine.port, scsynth_send_port: @engine.port}, Queue.new,
                           HeadlessEngine::State.new, ->(*) {}, -> { nil })
      @server = @studio.server
    end

    def teardown
      puts "\n--- #{name}: the engine's log, #{@engine.log} ---", @engine.log_tail unless passed? || @engine.nil?
      @server&.shutdown rescue nil
      @engine&.stop
      EngineConnection.reset!
      FileUtils.rm_rf(@dir) if @dir
    end

    def test_a_file_that_is_not_audio_fails_the_run_naming_the_file
      path = not_audio("notes.wav")
      e = assert_raises(RuntimeError) { realise(path) }
      assert_includes e.message, path
    end

    def test_a_failed_load_is_forgotten_so_the_next_one_reads_the_file_again
      path = not_audio("kick.flac")
      assert_raises(RuntimeError) { realise(path) }
      refute @studio.sample_loaded?(path), "a load that failed is not a loaded sample"

      FileUtils.cp(AUDIO, path)
      buf, cached = @studio.load_sample(path)
      refute cached, "the failed load was handed back instead of a new one"
      assert_operator buf.num_frames, :>, 0
    end

    def test_a_load_with_no_answer_fails_the_run_and_the_next_one_plays
      path = File.join(@dir, "kick.flac")
      FileUtils.cp(AUDIO, path)
      lose_the_next_load

      e = assert_raises(RuntimeError) { realise(path) }
      assert_includes e.message, path
      refute @studio.sample_loaded?(path)

      buf, cached = @studio.load_sample(path)
      refute cached
      assert_operator buf.num_frames, :>, 0
    end

    private

    def not_audio(name)
      path = File.join(@dir, name)
      File.write(path, "these are notes, not sound\n" * 64)
      path
    end

    # What a run does with a sample before it plays: load it and read it. The
    # bound is well past the server's 5 s wait for an answer.
    def realise(path)
      buf, _ = @studio.load_sample(path)
      Timeout.timeout(15, StillWaiting) { buf.num_chans }
    end

    # The next /b_allocRead never reaches the engine, so nothing answers it.
    def lose_the_next_load
      lost = false
      @server.singleton_class.prepend(Module.new do
        define_method(:osc) do |path, *args|
          if path == "/b_allocRead" && !lost
            lost = true
            return
          end
          super(path, *args)
        end
      end)
    end
  end
end
