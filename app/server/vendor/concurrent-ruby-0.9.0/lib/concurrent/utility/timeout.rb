require 'rbconfig'
require 'thread'

require 'concurrent/errors'
require 'concurrent/concern/deprecation'

module Concurrent
  extend Concern::Deprecation

  # [DEPRECATED] Wait the given number of seconds for the block operation to complete.
  # Intended to be a simpler and more reliable replacement to the Ruby
  # standard library `Timeout::timeout` method. It does not kill the task
  # so it finishes anyway. Advantage is that it cannot cause any ugly errors by
  # killing threads.
  #
  # @param [Integer] seconds The number of seconds to wait
  # @return [Object] The result of the block operation
  #
  # @raise [Concurrent::TimeoutError] when the block operation does not complete
  #   in the allotted number of seconds.
  #
  # @see http://ruby-doc.org/stdlib-2.2.0/libdoc/timeout/rdoc/Timeout.html Ruby Timeout::timeout
  #
  # @!macro monotonic_clock_warning
  #
  # @deprecated timeout is deprecated and will be removed
  def timeout(seconds, &block)
    deprecated 'timeout is deprecated and will be removed'

    future = Future.execute(&block)
    future.wait(seconds)
    if future.complete?
      future.value!
    else
      raise TimeoutError
    end
  end
  module_function :timeout
end
