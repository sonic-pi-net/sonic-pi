require "spec_helper"

require "hamster/list"

describe Hamster::List do

  describe "#to_list" do

    [
      [],
      ["A"],
      %w[A B C],
    ].each do |values|

      describe "on #{values.inspect}" do

        before do
          @original = Hamster.list(*values)
          @result = @original.to_list
        end

        it "returns self" do
          @result.should equal(@original)
        end

      end

    end

  end

end
