require 'concurrent/executor/immediate_executor'
require_relative 'executor_service_shared'

module Concurrent

  RSpec.describe ImmediateExecutor do

    subject { ImmediateExecutor.new }

    it_should_behave_like :executor_service
  end
end
