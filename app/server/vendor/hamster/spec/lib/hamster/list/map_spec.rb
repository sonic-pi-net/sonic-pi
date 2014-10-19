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
        context "on #{values.inspect}" do
          let(:list) { Hamster.list(*values) }

          context "with a block" do
            it "preserves the original" do
              list.send(method, &:downcase)
              list.should eql(Hamster.list(*values))
            end

            it "returns #{expected.inspect}" do
              list.send(method, &:downcase).should eql(Hamster.list(*expected))
            end

            it "is lazy" do
              count = 0
              list.send(method) { |item| count += 1 }
              count.should <= 1
            end
          end

          context "without a block" do
            it "returns an Enumerator" do
              list.send(method).class.should be(Enumerator)
              list.send(method).each(&:downcase).should eql(Hamster.list(*expected))
            end
          end
        end
      end
    end
  end
end