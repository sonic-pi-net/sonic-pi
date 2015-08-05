require "spec_helper"
require "hamster/deque"

describe Hamster::Deque do
  describe "#pop" do
    [
      [[], []],
      [["A"], []],
      [%w[A B C], %w[A B]],
    ].each do |values, expected|
      context "on #{values.inspect}" do
        let(:deque) { Hamster.deque(*values) }

        it "preserves the original" do
          deque.pop
          deque.should eql(Hamster.deque(*values))
        end

        it "returns #{expected.inspect}" do
          deque.pop.should eql(Hamster.deque(*expected))
        end
      end
    end

    context "on empty subclass" do
      let(:subclass) { Class.new(Hamster::Deque) }
      let(:empty_instance) { subclass.new }
      it "returns emtpy object of same class" do
        empty_instance.pop.class.should be subclass
      end
    end
  end
end