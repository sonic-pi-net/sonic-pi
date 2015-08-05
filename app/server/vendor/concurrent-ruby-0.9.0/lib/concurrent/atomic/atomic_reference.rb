require 'concurrent/utility/native_extension_loader'
require 'concurrent/utility/engine'
require 'concurrent/atomic_reference/concurrent_update_error'
require 'concurrent/atomic_reference/mutex_atomic'

begin
  # force fallback impl with FORCE_ATOMIC_FALLBACK=1
  if /[^0fF]/ =~ ENV['FORCE_ATOMIC_FALLBACK']
    ruby_engine = 'mutex_atomic'
  else
    ruby_engine = Concurrent.ruby_engine
  end

  require "concurrent/atomic_reference/#{ruby_engine}"
rescue LoadError
  #warn 'Compiled extensions not installed, pure Ruby Atomic will be used.'
end

if defined? Concurrent::JavaAtomicReference

  # @!macro atomic_reference
  class Concurrent::AtomicReference < Concurrent::JavaAtomicReference
  end

elsif defined? Concurrent::RbxAtomicReference

  # @!macro atomic_reference
  class Concurrent::AtomicReference < Concurrent::RbxAtomicReference
  end

elsif defined? Concurrent::CAtomicReference

  # @!macro atomic_reference
  class Concurrent::AtomicReference < Concurrent::CAtomicReference
  end

else

  # @!macro atomic_reference
  class Concurrent::AtomicReference < Concurrent::MutexAtomicReference
  end
end

module Concurrent

  # @see Concurrent::AtomicReference
  # @deprecated Use Concurrent::AtomicReference instead.
  Atomic = AtomicReference
end
