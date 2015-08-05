require_relative 'struct_shared'

module Concurrent
  describe ImmutableStruct do
    it_should_behave_like :struct
    it_should_behave_like :mergeable_struct
  end
end
