require "spec_helper"
require "hamster/deque"

describe Hamster::Deque do
  describe "#clear" do
    [
      [],
      ["A"],
      %w[A B C],
    ].each do |values|
      context "on #{values}" do
        let(:deque) { Hamster.deque(*values) }

        it "preserves the original" do
          deque.clear
          deque.should eql(Hamster.deque(*values))
        end

        it "returns an empty deque" do
          deque.clear.should equal(Hamster.deque)
        end
      end
    end
  end

  context "from a subclass" do
    it "returns an instance of the subclass" do
      subclass = Class.new(Hamster::Deque)
      instance = subclass.new([1,2])
      instance.clear.should be_empty
      instance.clear.class.should be(subclass)
    end
  end
end