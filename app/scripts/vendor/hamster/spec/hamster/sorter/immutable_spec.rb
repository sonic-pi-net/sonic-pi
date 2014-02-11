require "spec_helper"

require "hamster/immutable"
require "hamster/sorter"

describe Hamster::Sorter do

  it "includes Immutable" do
    Hamster::Sorter.should include(Hamster::Immutable)
  end

end
