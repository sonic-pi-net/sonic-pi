module Concurrent

  Error = Class.new(StandardError)

  # Raised when errors occur during configuration.
  ConfigurationError = Class.new(Error)

  # Raised when an asynchronous operation is cancelled before execution.
  CancelledOperationError = Class.new(Error)

  # Raised when a lifecycle method (such as `stop`) is called in an improper
  # sequence or when the object is in an inappropriate state.
  LifecycleError = Class.new(Error)

  # Raised when an attempt is made to violate an immutability guarantee.
  ImmutabilityError = Class.new(Error)

  # Raised when an object's methods are called when it has not been
  # properly initialized.
  InitializationError = Class.new(Error)

  # Raised when an object with a start/stop lifecycle has been started an
  # excessive number of times. Often used in conjunction with a restart
  # policy or strategy.
  MaxRestartFrequencyError = Class.new(Error)

  # Raised when an attempt is made to modify an immutable object
  # (such as an `IVar`) after its final state has been set.
  MultipleAssignmentError = Class.new(Error)

  # Raised by an `Executor` when it is unable to process a given task,
  # possibly because of a reject policy or other internal error.
  RejectedExecutionError = Class.new(Error)

  # Raised when any finite resource, such as a lock counter, exceeds its
  # maximum limit/threshold.
  ResourceLimitError = Class.new(Error)

  # Raised when an operation times out.
  TimeoutError = Class.new(Error)

end
