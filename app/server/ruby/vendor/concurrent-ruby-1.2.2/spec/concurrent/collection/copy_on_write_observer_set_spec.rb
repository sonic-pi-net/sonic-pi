require 'concurrent/collection/copy_on_write_observer_set'
require_relative 'observer_set_shared'

module Concurrent
  module Collection

    RSpec.describe CopyOnWriteObserverSet do
      it_behaves_like 'an observer set'
    end
  end
end
