require_relative 'struct_shared'
require 'concurrent/immutable_struct'

module Concurrent
  RSpec.describe ImmutableStruct do
    it_should_behave_like :struct
    it_should_behave_like :mergeable_struct
  end
end
