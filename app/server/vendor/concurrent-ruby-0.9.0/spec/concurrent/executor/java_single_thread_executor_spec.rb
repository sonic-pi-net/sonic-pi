if Concurrent.on_jruby?

  require_relative 'executor_service_shared'

  module Concurrent

    describe JavaSingleThreadExecutor, :type=>:jruby do

      after(:each) do
        subject.kill
        sleep(0.1)
      end

      subject { JavaSingleThreadExecutor.new }

      it_should_behave_like :executor_service
    end
  end
end
