require "spec_helper"

require "hamster/tuple"

describe Hamster::Tuple do

  [:dup, :clone].each do |method|

    describe "#{method}" do

      before do
        @original = Hamster::Tuple.new("A", "B")
        @result = @original.send(method)
      end

      it "returns self" do
        @result.should equal(@original)
      end

    end

  end

end
