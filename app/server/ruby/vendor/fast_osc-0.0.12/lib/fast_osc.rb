require "fast_osc/version"

module FastOsc

  # Your code goes here...

end

begin
  # Modifications made for Sonic Pi multi-platform compatibility:
  require 'rbconfig'
  ruby_api = RbConfig::CONFIG['ruby_version']
  require_relative "../../../rb-native/#{ruby_api}/fast_osc"
  module FastOsc
    # Your code goes here...
  end
rescue LoadError
  module FastOsc
    # Your code goes here...
  end
  warn "Failed to load the fast_osc c-extension, falling back to pure Ruby version"
  require "fast_osc/pure_ruby_fallback_encode.rb"
  require "fast_osc/pure_ruby_fallback_decode.rb"
end

if ENV['FAST_OSC_USE_FALLBACK'] == "true"
  warn "Using pure Ruby fallback"
  require "fast_osc/pure_ruby_fallback_encode.rb"
  require "fast_osc/pure_ruby_fallback_decode.rb"
end
