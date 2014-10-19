require "spec_helper"
require "hamster/set"

describe Hamster::Set do
  [:map, :collect].each do |method|
    describe "##{method}" do
      context "when empty" do
        it "returns self" do
          Hamster.set.send(method) {}.should equal(Hamster.set)
        end
      end

      context "when not empty" do
        let(:set) { Hamster.set("A", "B", "C") }

        context "with a block" do
          it "preserves the original values" do
            set.send(method, &:downcase)
            set.should eql(Hamster.set("A", "B", "C"))
          end

          it "returns a new set with the mapped values" do
            set.send(method, &:downcase).should eql(Hamster.set("a", "b", "c"))
          end
        end

        context "with no block" do
          it "returns an Enumerator" do
            set.send(method).class.should be(Enumerator)
            set.send(method).each(&:downcase).should == Hamster.set('a', 'b', 'c')
          end
        end
      end

      context "from a subclass" do
        it "returns an instance of the subclass" do
          subclass = Class.new(Hamster::Set)
          instance = subclass['a', 'b']
          instance.map { |item| item.upcase }.class.should be(subclass)
        end
      end

      context "when multiple items map to the same value" do
        it "filters out the duplicates" do
          set = Hamster::Set.new('aa'..'zz')
          result = set.map { |s| s[0] }
          result.should eql(Hamster::Set.new('a'..'z'))
          result.size.should == 26
        end
      end

      it "works on large sets" do
        set = Hamster::Set.new(1..1000)
        result = set.map { |x| x * 10 }
        result.size.should == 1000
        1.upto(1000) { |n| result.include?(n * 10).should == true }
      end
    end
  end
end