require_relative 'observer_set_shared'

module Concurrent
  module Collection

    describe CopyOnWriteObserverSet do
      it_behaves_like 'an observer set'
    end
  end
end
