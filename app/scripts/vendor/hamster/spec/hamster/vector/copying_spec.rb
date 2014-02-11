require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do

  [:dup, :clone].each do |method|

    [
      [],
      ["A"],
      %w[A B C],
    ].each do |values|

      describe "on #{values.inspect}" do

        before do
          @original = Hamster.vector(*values)
          @result = @original.send(method)
        end

        it "returns self" do
          @result.should equal(@original)
        end

      end

    end

  end

end
