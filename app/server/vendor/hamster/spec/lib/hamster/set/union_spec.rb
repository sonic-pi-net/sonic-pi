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
          end
        end
      end
    end
  end
end