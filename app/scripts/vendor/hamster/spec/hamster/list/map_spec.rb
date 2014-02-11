require "spec_helper"

require "hamster/list"

describe Hamster::List do

  [:map, :collect].each do |method|

    describe "##{method}" do

      it "is lazy" do
        -> { Hamster.stream { fail }.map { |item| item } }.should_not raise_error
      end

      [
        [[], []],
        [["A"], ["a"]],
        [%w[A B C], %w[a b c]],
      ].each do |values, expected|

        describe "on #{values.inspect}" do

          before do
            @original = Hamster.list(*values)
          end

          describe "with a block" do

            before do
              @result = @original.send(method, &:downcase)
            end

            it "preserves the original" do
              @original.should == Hamster.list(*values)
            end

            it "returns #{expected.inspect}" do
              @result.should == Hamster.list(*expected)
            end

            it "is lazy" do
              count = 0
              @original.send(method) { |item| count += 1 }
              count.should <= 1
            end

          end

          describe "without a block" do

            before do
              @result = @original.send(method)
            end

            it "returns self" do
              @result.should equal(@original)
            end

          end

        end

      end

    end

  end

end
