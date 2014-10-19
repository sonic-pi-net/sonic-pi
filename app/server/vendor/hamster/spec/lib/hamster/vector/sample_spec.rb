require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do
  describe "#sample" do
    let(:vector) { Hamster::Vector.new(1..10) }

    it "returns a randomly chosen item" do
      chosen = 100.times.map { vector.sample }
      chosen.each { |item| vector.include?(item).should == true }
      vector.each { |item| chosen.include?(item).should == true }
    end
  end
end
