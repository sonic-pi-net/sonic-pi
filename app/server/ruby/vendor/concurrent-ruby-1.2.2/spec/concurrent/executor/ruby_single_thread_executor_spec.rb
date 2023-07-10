require 'concurrent/executor/ruby_single_thread_executor'
require_relative 'executor_service_shared'

module Concurrent

  RSpec.describe RubySingleThreadExecutor, :type=>:mri do

    after(:each) do
      subject.shutdown
      expect(subject.wait_for_termination(pool_termination_timeout)).to eq true
    end

    subject { RubySingleThreadExecutor.new }

    it_should_behave_like :executor_service
  end
end
