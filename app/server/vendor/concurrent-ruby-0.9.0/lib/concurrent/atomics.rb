# @!macro [new] atomic_reference
#
#   An object reference that may be updated atomically.
#
#       Testing with ruby 2.1.2
#       
#       *** Sequential updates ***
#                                 user     system      total        real
#       no lock               0.000000   0.000000   0.000000 (  0.005502)
#       mutex                 0.030000   0.000000   0.030000 (  0.025158)
#       MutexAtomicReference  0.100000   0.000000   0.100000 (  0.103096)
#       CAtomicReference      0.040000   0.000000   0.040000 (  0.034012)
#       
#       *** Parallel updates ***
#                                 user     system      total        real
#       no lock               0.010000   0.000000   0.010000 (  0.009387)
#       mutex                 0.030000   0.010000   0.040000 (  0.032545)
#       MutexAtomicReference  0.830000   2.280000   3.110000 (  2.146622)
#       CAtomicReference      0.040000   0.000000   0.040000 (  0.038332)
#
#       Testing with jruby 1.9.3
#       
#       *** Sequential updates ***
#                                 user     system      total        real
#       no lock               0.170000   0.000000   0.170000 (  0.051000)
#       mutex                 0.370000   0.010000   0.380000 (  0.121000)
#       MutexAtomicReference  1.530000   0.020000   1.550000 (  0.471000)
#       JavaAtomicReference   0.370000   0.010000   0.380000 (  0.112000)
#       
#       *** Parallel updates ***
#                                 user     system      total        real
#       no lock               0.390000   0.000000   0.390000 (  0.105000)
#       mutex                 0.480000   0.040000   0.520000 (  0.145000)
#       MutexAtomicReference  1.600000   0.180000   1.780000 (  0.511000)
#       JavaAtomicReference   0.460000   0.010000   0.470000 (  0.131000)
#
#   @see http://docs.oracle.com/javase/8/docs/api/java/util/concurrent/atomic/AtomicReference.html
#   @see http://docs.oracle.com/javase/8/docs/api/java/util/concurrent/atomic/package-summary.html
#
#   @!method initialize
#     @!macro [new] atomic_reference_method_initialize
#       @param [Object] value The initial value.
#   
#   @!method get
#     @!macro [new] atomic_reference_method_get
#       Gets the current value.
#       @return [Object] the current value
#   
#   @!method set
#     @!macro [new] atomic_reference_method_set
#       Sets to the given value.
#       @param [Object] new_value the new value
#       @return [Object] the new value
#   
#   @!method get_and_set
#     @!macro [new] atomic_reference_method_get_and_set
#       Atomically sets to the given value and returns the old value.
#       @param [Object] new_value the new value
#       @return [Object] the old value
#   
#   @!method compare_and_set
#     @!macro [new] atomic_reference_method_compare_and_set
#
#       Atomically sets the value to the given updated value if
#       the current value == the expected value.
#
#       @param [Object] old_value the expected value
#       @param [Object] new_value the new value
#
#       @return [Boolean] `true` if successful. A `false` return indicates
#       that the actual value was not equal to the expected value.

require 'concurrent/atomic/atomic_reference'
require 'concurrent/atomic/atomic_boolean'
require 'concurrent/atomic/atomic_fixnum'
require 'concurrent/atomic/condition'
require 'concurrent/atomic/cyclic_barrier'
require 'concurrent/atomic/count_down_latch'
require 'concurrent/atomic/event'
require 'concurrent/atomic/read_write_lock'
require 'concurrent/atomic/semaphore'
require 'concurrent/atomic/thread_local_var'
