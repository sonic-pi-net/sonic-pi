require "spec_helper"

require "hamster/immutable"
require "hamster/stack"

describe Hamster::Stack do

  it "includes Immutable" do
    Hamster::Stack.should include(Hamster::Immutable)
  end

end
