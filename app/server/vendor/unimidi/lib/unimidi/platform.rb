module UniMIDI

  # Deal with different dependencies between different user environments
  module Platform

    extend self
    
    # Loads the proper MIDI library and adapter for the user's environment
    def bootstrap
      lib = case RUBY_PLATFORM
        when /darwin/ then "ffi-coremidi"
        when /java/ then "midi-jruby"
        when /linux/ then "alsa-rawmidi"
        when /mingw/ then "midi-winmm"
      end
      require("unimidi/adapter/#{lib}")
      interface = case RUBY_PLATFORM
        when /darwin/ then Adapter::CoreMIDI
        when /java/ then Adapter::MIDIJRuby
        when /linux/ then Adapter::AlsaRawMIDI
        when /mingw/ then Adapter::MIDIWinMM
      end
      Loader.use(interface::Loader)
    end

  end

end
