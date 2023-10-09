require 'concurrent/utility/engine'

if Concurrent.on_jruby?
  require_relative 'executor_service_shared'

  module Concurrent

    RSpec.describe JavaSingleThreadExecutor, :type=>:jruby do

      after(:each) do
        subject.shutdown
        expect(subject.wait_for_termination(pool_termination_timeout)).to eq true
      end

      subject { JavaSingleThreadExecutor.new }

      it_should_behave_like :executor_service
    end
  end
end
