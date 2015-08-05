require_relative 'executor_service_shared'

module Concurrent

  describe ImmediateExecutor do

    subject { ImmediateExecutor.new }

    it_should_behave_like :executor_service
  end
end
