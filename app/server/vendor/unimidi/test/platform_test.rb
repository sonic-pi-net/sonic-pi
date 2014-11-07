require 'helper'

class UniMIDI::PlatformTest < Test::Unit::TestCase

  def platform_test(adapter, mod, device_class = nil, input_class = nil, output_class = nil)
    device_class ||= mod::Device
    input_class ||= mod::Input
    output_class ||= mod::Output
    assert_not_same(input_class, UniMIDI::Input)
    assert_not_same(output_class, UniMIDI::Output)
    assert_not_same(device_class, UniMIDI::Device)
    assert_equal(input_class.first.name, UniMIDI::Input.first.name)
    assert_equal(input_class.first.id, UniMIDI::Input.first.id)
    assert_not_same(output_class.first, UniMIDI::Output.first)
    assert_equal(output_class.first.name, UniMIDI::Output.first.name)
    assert_equal(output_class.first.id, UniMIDI::Output.first.id)
  end

  context "Platform" do

    should "recognize java" do
      if RUBY_PLATFORM.include?("java")
        platform_test(UniMIDI::Adapter::MIDIJRuby, ::MIDIJRuby)
      end
    end

    should "recognize linux" do
      if RUBY_PLATFORM.include?("linux")
        platform_test(UniMIDI::Adapter::AlsaRawMIDI, ::AlsaRawMIDI)
      end  
    end

    should "recognize osx" do
      if RUBY_PLATFORM.include?("darwin")
        platform_test(UniMIDI::Adapter::CoreMIDI, ::CoreMIDI, ::CoreMIDI::Endpoint, ::CoreMIDI::Source, ::CoreMIDI::Destination)
      end  
    end

    should "recognize windows" do
      if RUBY_PLATFORM.include?("mingw")
        platform_test(UniMIDI::Adapter::MIDIWinMM, ::MIDIWinMM)
      end
    end  

  end

end
