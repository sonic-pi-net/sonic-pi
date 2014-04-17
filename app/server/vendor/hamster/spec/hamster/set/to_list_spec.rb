require "spec_helper"

require "hamster/set"
require "hamster/list"

describe Hamster::Set do

  describe "#to_list" do

    [
      [],
      ["A"],
      %w[A B C],
    ].each do |values|

      describe "on #{values.inspect}" do

        before do
          set = Hamster.set(*values)
          @list = set.to_list
        end

        it "returns a list" do
          @list.is_a?(Hamster::List).should == true
        end

        describe "the returned list" do

          it "has the correct length" do
            @list.size.should == values.size
          end

          it "contains all values" do
            values.each do |value|
              @list.should include(value)
            end
          end

        end

      end

    end

  end

end
