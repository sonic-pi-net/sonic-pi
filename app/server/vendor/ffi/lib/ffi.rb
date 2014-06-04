if !defined?(RUBY_ENGINE) || RUBY_ENGINE == 'ruby' || RUBY_ENGINE == 'rbx'
  Object.send(:remove_const, :FFI) if defined?(::FFI)
  begin
    if RUBY_VERSION =~ /1.8/
      require '1.8/ffi_c'
    elsif RUBY_VERSION =~ /1.9/
      require '1.9/ffi_c'
    elsif RUBY_VERSION =~ /2.0/
      require '2.0/ffi_c'
    else
      require 'ffi_c'
    end
  rescue Exception
  # Start modifications
  #
  # Original code:
  # require 'ffi_c'

  # Modifications made for Sonic Pi multi-platform compatibility:
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
  require_relative "../../../rb-native/#{os}/#{RUBY_VERSION}p#{RUBY_PATCHLEVEL}/ffi_c"

  end

  require 'ffi/ffi'

elsif defined?(RUBY_ENGINE)
  # Remove the ffi gem dir from the load path, then reload the internal ffi implementation
  $LOAD_PATH.delete(File.dirname(__FILE__))
  $LOAD_PATH.delete(File.join(File.dirname(__FILE__), 'ffi'))
  unless $LOADED_FEATURES.nil?
    $LOADED_FEATURES.delete(__FILE__)
    $LOADED_FEATURES.delete('ffi.rb')
  end
  require 'ffi.rb'
end
