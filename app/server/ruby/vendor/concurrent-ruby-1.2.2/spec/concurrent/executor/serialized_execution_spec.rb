require 'concurrent/executor/serialized_execution_delegator'
require 'concurrent/executor/immediate_executor'
require_relative 'executor_service_shared'

module Concurrent

  RSpec.describe SerializedExecutionDelegator do

    subject { SerializedExecutionDelegator.new(ImmediateExecutor.new) }

    it_should_behave_like :executor_service
  end
end
