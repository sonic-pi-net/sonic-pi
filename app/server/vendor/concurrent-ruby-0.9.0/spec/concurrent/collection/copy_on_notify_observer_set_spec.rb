require_relative 'observer_set_shared'

module Concurrent
  module Collection

    describe CopyOnNotifyObserverSet do
      it_behaves_like 'an observer set'
    end
  end
end
