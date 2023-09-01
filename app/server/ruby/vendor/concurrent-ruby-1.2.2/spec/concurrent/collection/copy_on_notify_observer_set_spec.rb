require 'concurrent/collection/copy_on_notify_observer_set'
require_relative 'observer_set_shared'

module Concurrent
  module Collection
    RSpec.describe CopyOnNotifyObserverSet do
      it_behaves_like 'an observer set'
    end
  end
end
