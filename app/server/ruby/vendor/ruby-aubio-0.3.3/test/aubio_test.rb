require 'test_helper'

class AubioTest < Minitest::Test
  def setup
    @not_audio_file_path = File.expand_path("../sounds/not_an_audio_file.txt", __FILE__)
    @loop_amen_path = File.expand_path("../sounds/loop_amen.wav", __FILE__)
    @loop_amen_full_path = File.expand_path("../sounds/loop_amen_full.wav", __FILE__)
    @french_house_path = File.expand_path("../sounds/french_house_thing.wav", __FILE__)
    @bobby_mcferrin_path = File.expand_path("../sounds/bobby.wav", __FILE__)
  end

  def test_that_it_has_a_version_number
    refute_nil ::Aubio::VERSION
  end

  def test_it_checks_file_existence
    assert_raises Aubio::FileNotFound do
      result = Aubio.open(@loop_amen_path + "e")
    end
  end

  def test_it_checks_file_is_readable_audio
    assert_raises Aubio::InvalidAudioInput do
      Aubio.open(@not_audio_file_path)
    end
  end

  def test_it_checks_if_file_has_been_closed
    source = Aubio.open(@loop_amen_path)
    source.close
    assert_raises Aubio::AlreadyClosed do
      source.onsets
    end
  end

  def test_it_calculates_onsets
    result = Aubio.open(@loop_amen_path).onsets.to_a
    pairs = result.take(3).zip([
      {:s=>0.0, :ms=>0.0, :start=> 1, :end=>0},
      {:s=>0.21174603700637817, :ms=>211.74603271484375, :start=> 0, :end=>0},
      {:s=>0.4404761791229248, :ms=>440.4761657714844, :start=> 0, :end=>0},
    ])

    pairs.each do |expected, actual|
      assert_in_delta expected[:s], actual[:s], 0.25
    end
  end

  def test_it_calculates_pitches
    result = Aubio.open(@bobby_mcferrin_path, pitch_method: 'yinfast', confidence_thresh: 0.9).pitches.to_a
    assert_in_delta 50.0, result[0][:pitch], 1.0
  end

  def test_it_calculates_beats
    result = Aubio.open(@loop_amen_full_path).beats
    assert_equal [
      {:confidence=>1, :s=>0.0, :ms=>0.0, :sample_no=>0, :total_samples=>302400.0, :rel_start=>0.0, :rel_end=>0.2202149470899471}
    ], result.first(1)

    assert_equal 13, result.length
  end

  def test_it_filters_beats_based_on_minimum_interval
    result = Aubio.open(@loop_amen_full_path, minioi: 0.5).beats
    assert_equal 8, result.length
  end

  def test_it_calculates_bpm
    # TODO: see if this can be improved
    # the actual bpm of that sample is 125

    result = Aubio.open(@french_house_path).bpm
    assert_equal 126.66357966281865, result
  end

  def test_it_provides_default_bpm_as_fallback
    result = Aubio.open(@loop_amen_path).bpm
    assert_equal 60, result
  end
end
