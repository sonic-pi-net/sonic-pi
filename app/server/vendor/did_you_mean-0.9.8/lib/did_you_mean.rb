require "interception"

require "did_you_mean/version"
require "did_you_mean/core_ext/name_error"
require "did_you_mean/finders"

module DidYouMean
  Interception.listen(->(exception, binding) {
    # On IRB/pry console, this event is called twice. In the second event,
    # we get IRB/pry binding. So it shouldn't override @frame_binding if
    # it's already defined.
    if exception.is_a?(NameError) && !exception.instance_variable_defined?(:@frame_binding)
      exception.instance_variable_set(:@frame_binding, binding)
    end
  })
end
