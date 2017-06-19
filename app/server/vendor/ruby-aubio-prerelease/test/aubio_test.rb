require 'test_helper'

class AubioTest < Minitest::Test
  def setup
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
      Aubio.open("/Users/xriley/Projects/aubio/Gemfile")
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
    assert_equal [
      {:s=>0.0, :ms=>0.0, :start=> 1, :end=>0},
      {:s=>0.21174603700637817, :ms=>211.74603271484375, :start=> 0, :end=>0},
      {:s=>0.4404761791229248, :ms=>440.4761657714844, :start=> 0, :end=>0},
      {:s=>0.6651020646095276, :ms=>665.10205078125, :start=> 0, :end=>0},
      {:s=>0.795714259147644, :ms=>795.7142333984375, :start=> 0, :end=>0},
      {:s=>0.8873015642166138, :ms=>887.3015747070312, :start=> 0, :end=>0},
      {:s=>0.9989795684814453, :ms=>998.9795532226562, :start=> 0, :end=>0},
      {:s=>1.0950794219970703, :ms=>1095.0794677734375, :start=> 0, :end=>0},
      {:s=>1.3104535341262817, :ms=>1310.4534912109375, :start=> 0, :end=>0},
      {:s=>1.5354194641113281, :ms=>1535.41943359375, :start=> 0, :end=>0},
      {:s=>1.753310657596372, :ms=>0.001753310657596372, :start=>0, :end=>1}
    ], result
  end

  def test_it_calculates_pitches
    result = Aubio.open(@bobby_mcferrin_path).pitches.to_a
    assert_equal [
      {:pitch=>50.614505767822266, :confidence=>0.9164013862609863, :start=>0, :end=>0}
    ], result.first(1)
  end

  def test_it_calculates_beats
    result = Aubio.open(@loop_amen_full_path).beats.to_a
    assert_equal [
      {:confidence=>0.0, :s=>1.5100454092025757, :ms=>1510.04541015625, :start=>0, :end=>0}
    ], result.first(1)

    assert_equal 13, result.length
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
