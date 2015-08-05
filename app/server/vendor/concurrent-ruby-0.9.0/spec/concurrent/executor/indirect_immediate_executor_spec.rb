require_relative 'executor_service_shared'

module Concurrent

  describe IndirectImmediateExecutor do

    subject { IndirectImmediateExecutor.new }

    it_should_behave_like :executor_service

    it "runs its tasks synchronously" do
      start = Time.now
      subject.post { sleep 0.1 }

      expect(Time.now - start).to be >= 0.1
    end

    it "runs the task on a separate thread" do
      used_thread = nil
      subject.post { used_thread = Thread.current }

      expect(used_thread).not_to be(Thread.current)
    end
  end
end
