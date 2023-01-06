# frozen_string_literal: true

Dir["#{File.expand_path('../../vendor', __dir__)}/*/lib/"].each do |vendor_lib|
  puts vendor_lib
  $LOAD_PATH.unshift vendor_lib
end
require 'fileutils'
require_relative '../setup_test'
require_relative '../../bin/i18n-tool-reference'

class ReferenceI18nTester < Minitest::Test
  def setup
    @generated_path = "#{__dir__}/generated"
    @lang_path = "#{__dir__}/lang"
    @pot_file_path = "#{@lang_path}/sonic-pi-reference.pot"
    @po_file_path = "#{@lang_path}/sonic-pi-reference-fr.po"

    @synthinfo_filtered = SonicPi::Synths::SynthInfo.get_all.select { |k, _v| %i[fx_autotuner bass_foundation].include?(k) }
    @samples_filtered = SonicPi::Synths::SynthInfo.grouped_samples.select { |k, _v| k == :ambi }
    @lang_filtered = SonicPi::Lang::Sound.docs.select { |k, _v| k == :all_sample_names }
    SonicPi::Paths.stubs(:docs_generated_path).returns(@generated_path)
    SonicPi::Paths.stubs(:lang_path).returns(@lang_path)

    @i18n_tool = ::I18nTool.new
    @i18n_tool.stubs(:locales).returns(['fr'])

    FileUtils.rm_rf(@generated_path) if File.exist?(@generated_path)
    FileUtils.rm_rf(@pot_file_path) if File.exist?(@pot_file_path)
    FileUtils.rm_rf(@po_file_path) if File.exist?(@po_file_path)
    FileUtils.touch(@po_file_path)
  end

  def teardown
    FileUtils.rm_rf(@generated_path) if File.exist?(@generated_path)
    FileUtils.rm_rf(@pot_file_path) if File.exist?(@pot_file_path)
    FileUtils.rm_rf(@po_file_path) if File.exist?(@po_file_path)
  end

  def test_run_to_extract_produces_a_file
    refute_path_exists(@pot_file_path)
    SonicPi::Synths::SynthInfo.expects(:get_all).twice.returns(@synthinfo_filtered)
    SonicPi::Synths::SynthInfo.expects(:grouped_samples).returns(@samples_filtered)
    ::I18nTool.expects(:docs).returns(@lang_filtered)
    @i18n_tool.run(["-x#{@pot_file_path}"])
    assert_path_exists(@pot_file_path)
  end

  def test_run_to_update_modifies_files
    assert_empty(File.read(@po_file_path))
    @i18n_tool.run(["-u#{@lang_path}/mock-sonic-pi-reference.pot"])
    refute_empty(File.read(@po_file_path))
  end

  def test_run_to_translate_creates_translated_files
    interpolated_template_path = "#{__dir__}/translate_test/interpolated_templates"
    SonicPi::Synths::SynthInfo.expects(:get_all).twice.returns(@synthinfo_filtered)
    SonicPi::Synths::SynthInfo.expects(:grouped_samples).returns(@samples_filtered)
    ::I18nTool.expects(:docs).returns(@lang_filtered)
    SonicPi::Paths.stubs(:docs_interpolated_template_path).returns(interpolated_template_path)
    @toml_file_path = "#{@generated_path}/fr/reference"

    refute_path_exists(@toml_file_path)
    @i18n_tool.run(['-t'])
    assert_path_exists(@toml_file_path)
    refute_empty(Dir["#{@toml_file_path}/**/*.toml"])
  end
end
