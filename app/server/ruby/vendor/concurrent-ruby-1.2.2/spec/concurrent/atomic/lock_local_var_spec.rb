require 'concurrent/atomic/lock_local_var'

module Concurrent

  RSpec.describe LockLocalVar do
    mutex = Mutex.new
    mutex_owned_per_thread = mutex.synchronize do
      Fiber.new { mutex.owned? }.resume
    end

    it "uses FiberLocalVar if Mutex is per Fiber", if: !mutex_owned_per_thread do
      expect(LockLocalVar).to be(FiberLocalVar)
    end

    it "uses ThreadLocalVar if Mutex is per Thread", if: mutex_owned_per_thread do
      expect(LockLocalVar).to be(ThreadLocalVar)
    end
  end

end
