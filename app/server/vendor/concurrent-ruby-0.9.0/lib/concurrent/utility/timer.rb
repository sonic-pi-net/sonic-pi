require 'concurrent/configuration'
require 'concurrent/concern/deprecation'

module Concurrent
  extend Concern::Deprecation

  # [DEPRECATED] Perform the given operation asynchronously after
  # the given number of seconds.
  #
  # @param [Fixnum] seconds the interval in seconds to wait before executing the task
  #
  # @yield the task to execute
  #
  # @return [Concurrent::ScheduledTask] IVar representing the task
  #
  # @see Concurrent::ScheduledTask
  #
  # @deprecated use `ScheduledTask` instead
  def timer(seconds, *args, &block)
    deprecated_method 'Concurrent.timer', 'ScheduledTask'
    raise ArgumentError.new('no block given') unless block_given?
    raise ArgumentError.new('interval must be greater than or equal to zero') if seconds < 0
    Concurrent.global_timer_set.post(seconds, *args, &block)
  end
  module_function :timer
end
