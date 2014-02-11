require "spec_helper"

require "hamster/list"

describe Hamster::List do

  [:remove, :reject, :delete_if].each do |method|

    describe "##{method}" do

      it "is lazy" do
        -> { Hamster.stream { fail }.send(method) { |item| false } }.should_not raise_error
      end

      [
        [[], []],
        [["A"], ["A"]],
        [%w[A B C], %w[A B C]],
        [%w[A b C], %w[A C]],
        [%w[a b c], []],
      ].each do |values, expected|

        describe "on #{values.inspect}" do

          before do
            @original = Hamster.list(*values)
          end

          describe "with a block" do

            before do
              @result = @original.send(method) { |item| item == item.downcase }
            end

            it "returns #{expected.inspect}" do
              @result.should == Hamster.list(*expected)
            end

            it "is lazy" do
              count = 0
              @original.send(method) do |item|
                count += 1
                false
              end
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
