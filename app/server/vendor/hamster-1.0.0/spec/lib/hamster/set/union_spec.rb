require "spec_helper"
require "hamster/set"

describe Hamster::Set do
  [:union, :|, :+, :merge].each do |method|
    describe "##{method}" do
      [
        [[], [], []],
        [["A"], [], ["A"]],
        [["A"], ["A"], ["A"]],
        [[], ["A"], ["A"]],
        [%w[A B C], [], %w[A B C]],
        [%w[A B C], %w[A B C], %w[A B C]],
        [%w[A B C], %w[X Y Z], %w[A B C X Y Z]]
      ].each do |a, b, expected|
        context "for #{a.inspect} and #{b.inspect}" do
          let(:set_a) { Hamster.set(*a) }
          let(:set_b) { Hamster.set(*b) }

          it "returns #{expected.inspect}, without changing the original Sets" do
            set_a.send(method, set_b).should eql(Hamster.set(*expected))
            set_a.should eql(Hamster::Set.new(a))
            set_b.should eql(Hamster::Set.new(b))
          end
        end

        context "for #{b.inspect} and #{a.inspect}" do
          let(:set_a) { Hamster.set(*a) }
          let(:set_b) { Hamster.set(*b) }

          it "returns #{expected.inspect}, without changing the original Sets" do
            set_b.send(method, set_a).should eql(Hamster.set(*expected))
            set_a.should eql(Hamster::Set.new(a))
            set_b.should eql(Hamster::Set.new(b))
          end
        end

        context "when passed a Ruby Array" do
          it "returns the expected Set" do
            Hamster.set(*a).send(method, b.freeze).should eql(Hamster.set(*expected))
            Hamster.set(*b).send(method, a.freeze).should eql(Hamster.set(*expected))
          end
        end

        context "from a subclass" do
          it "returns an instance of the subclass" do
            subclass = Class.new(Hamster::Set)
            subclass.new(a).send(method, Hamster::Set.new(b)).class.should be(subclass)
            subclass.new(b).send(method, Hamster::Set.new(a)).class.should be(subclass)
          end
        end
      end

      context "when receiving a subset" do
        let(:set_a) { Hamster.set(*(1..300).to_a) }
        let(:set_b) { Hamster.set(*(1..200).to_a) }

        it "returns self" do
          set_a.send(method, set_b).should be(set_a)
        end
      end
    end
  end
end