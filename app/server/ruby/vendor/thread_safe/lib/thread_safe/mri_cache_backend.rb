module ThreadSafe
  class MriCacheBackend < NonConcurrentCacheBackend
    # We can get away with a single global write lock (instead of a per-instance one) because of the GVL/green threads.
    #
    # The previous implementation used `Thread.critical` on 1.8 MRI to implement the 4 composed atomic operations (`put_if_absent`, `replace_pair`,
    # `replace_if_exists`, `delete_pair`) this however doesn't work for `compute_if_absent` because on 1.8 the Mutex class is itself implemented
    # via `Thread.critical` and a call to `Mutex#lock` does not restore the previous `Thread.critical` value (thus any synchronisation clears the
    # `Thread.critical` flag and we loose control). This poses a problem as the provided block might use synchronisation on its own.
    #
    # NOTE: a neat idea of writing a c-ext to manually perform atomic put_if_absent, while relying on Ruby not releasing a GVL while calling
    # a c-ext will not work because of the potentially Ruby implemented `#hash` and `#eql?` key methods.
    WRITE_LOCK = Mutex.new

    def []=(key, value)
      WRITE_LOCK.synchronize { super }
    end

    def compute_if_absent(key)
      if stored_value = _get(key) # fast non-blocking path for the most likely case
        stored_value
      else
        WRITE_LOCK.synchronize { super }
      end
    end

    def compute_if_present(key)
      WRITE_LOCK.synchronize { super }
    end

    def compute(key)
      WRITE_LOCK.synchronize { super }
    end

    def merge_pair(key, value)
      WRITE_LOCK.synchronize { super }
    end

    def replace_pair(key, old_value, new_value)
      WRITE_LOCK.synchronize { super }
    end

    def replace_if_exists(key, new_value)
      WRITE_LOCK.synchronize { super }
    end

    def get_and_set(key, value)
      WRITE_LOCK.synchronize { super }
    end

    def delete(key)
      WRITE_LOCK.synchronize { super }
    end

    def delete_pair(key, value)
      WRITE_LOCK.synchronize { super }
    end

    def clear
      WRITE_LOCK.synchronize { super }
    end
  end
end
