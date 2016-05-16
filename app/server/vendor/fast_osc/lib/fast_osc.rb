require "fast_osc/version"
# Original code:
begin
  require "fast_osc/fast_osc"
rescue LoadError

  # Modifications made for Sonic Pi multi-platform compatibility:
  require 'rbconfig'
  ruby_api = RbConfig::CONFIG['ruby_version']
  os = case RUBY_PLATFORM
       when /.*arm.*-linux.*/
         :raspberry
       when /.*linux.*/
         :linux
       when /.*darwin.*/
         :osx
       when /.*mingw.*/
         :windows
       else
         RUBY_PLATFORM
       end
  require_relative "../../../rb-native/#{os}/#{ruby_api}/fast_osc"
end
# End modifications



module FastOsc
  # Your code goes here...
end
