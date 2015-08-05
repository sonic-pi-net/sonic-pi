require "spec_helper"
require "hamster/set"

describe Hamster::Set do
  [:head, :first].each do |method|
    describe "##{method}" do
      context "on an empty set" do
        it "returns nil" do
          Hamster.set.send(method).should be_nil
        end
      end

      context "on a non-empty set" do
        it "returns an arbitrary value from the set" do
          %w[A B C].include?(Hamster.set("A", "B", "C").send(method)).should == true
        end
      end

      it "returns nil if only member of set is nil" do
        Hamster.set(nil).send(method).should be(nil)
      end

      it "returns the first item yielded by #each" do
        10.times do
          set = Hamster::Set.new((rand(10)+1).times.collect { rand(10000 )})
          set.each { |item| break item }.should be(set.send(method))
        end
      end
    end
  end
end