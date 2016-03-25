#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "../../setup_test"
require_relative "../../../lib/sonicpi/sample_loader"

module SonicPi
  class SampleLoaderTester < Minitest::Test
    def setup
      @fake_sample_dir = File.expand_path("#{File.dirname(__FILE__)}../../../fake_sample_dir")
      @fake_built_in_sample_dir = File.expand_path("#{File.dirname(__FILE__)}../../../fake_built_in_sample_dir")
      # Contains:
      # -  a_text_file.txt
      # -  bar_baz.aiff
      # -  buzz_100.flac
      # -  eggs.wav
      # -  foo.wav
      # -  woo_100.aiff
      # -  xylophone-test-samp.wave
      # -  subdir
      #    |
      #    - quux.wav
      @loader = SampleLoader.new(@fake_sample_dir)
      @loader.stubs(:default_samples_paths).returns([@fake_built_in_sample_dir])
    end

    def test_single_builtin_symbol
      res = @loader.find_candidates([:foo])
      assert_equal(res, ["#{@fake_built_in_sample_dir}/foo.wav"])
    end

    def test_path_and_symbol_does_not_include_builtin_samples
      res = @loader.find_candidates([@fake_sample_dir, :foo])
      refute_includes(res, "#{@fake_built_in_sample_dir}/foo.wav")
    end

    def test_globs_count
      res = @loader.find_candidates(["#{@fake_sample_dir}/**"])
      assert_equal(res.count, 8)
    end

    def test_non_globs_count
      res = @loader.find_candidates([@fake_sample_dir])
      assert_equal(res.count, 7)
    end

    def test_globs_filt
      res = @loader.find_candidates(["#{@fake_sample_dir}/**", :quux])
      assert_equal(["#{@fake_sample_dir}/subdir/quux.wav"], res)
    end

    def test_non_globs_filt_does_not_include_subdir
      res = @loader.find_candidates([@fake_sample_dir, :quux])
      assert_equal([], res)
    end

    def test_non_globs_symbol_matches_name_exactly
      res = @loader.find_candidates([@fake_sample_dir, :foo])
      assert_equal(res, ["#{@fake_sample_dir}/foo.wav"])
    end

    def test_idx
      res = @loader.find_candidates([@fake_sample_dir, 1])
      assert_equal(["#{@fake_sample_dir}/buzz_100.flac"], res)
    end

    def test_mult_idx_overrides
      res = @loader.find_candidates([@fake_sample_dir, 3, 5, 1])
      assert_equal(["#{@fake_sample_dir}/buzz_100.flac"], res)
    end

    def test_idx_wrap
      res = @loader.find_candidates([@fake_sample_dir, 7])
      assert_equal(["#{@fake_sample_dir}/bar_baz.aiff"], res)
    end

    def test_glob_idx_wrap
      res = @loader.find_candidates([@fake_sample_dir + "/**", 8])
      assert_equal(["#{@fake_sample_dir}/bar_baz.aiff"], res)
    end

    def test_str_filter
      res = @loader.find_candidates([@fake_sample_dir, "100"])
      assert_equal(["#{@fake_sample_dir}/buzz_100.flac",
                    "#{@fake_sample_dir}/woo_100.aiff"], res)
    end

    def test_regex_filter
      res = @loader.find_candidates([@fake_sample_dir, /b[uU]zz/])
      assert_equal(["#{@fake_sample_dir}/buzz_100.flac"], res)
    end

    def test_0_arity_proc_filter
      proc = lambda {/b[uU]zz/}
      res = @loader.find_candidates([@fake_sample_dir, proc])
      assert_equal(["#{@fake_sample_dir}/buzz_100.flac"], res)
    end

    def test_1_arity_proc_filter
      proc = lambda {|c| c.keep_if { |v| v.include? "foo"}}
      res = @loader.find_candidates([@fake_sample_dir, proc])
      assert_equal(["#{@fake_sample_dir}/first_foo.wav",
                    "#{@fake_sample_dir}/foo.wav"], res)
    end

    def test_multi_filters
      identity_proc = lambda {|c| c }
      res = @loader.find_candidates([@fake_sample_dir, 1, identity_proc, "100", /100/])
      assert_equal(["#{@fake_sample_dir}/woo_100.aiff"], res)
    end

    def test_arrays
      identity_proc = lambda {|c| c }
      res = @loader.find_candidates([@fake_sample_dir,[1], [[identity_proc, "100"], /100/]])
      assert_equal(["#{@fake_sample_dir}/woo_100.aiff"], res)
    end

    def test_string_filtering_with_hyphens
      res = @loader.find_candidates([@fake_sample_dir, "xylophone-"])
      assert_equal(["#{@fake_sample_dir}/xylophone-test-samp.wave"], res)
    end

  end
end
