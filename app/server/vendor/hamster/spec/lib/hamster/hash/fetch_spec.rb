require "spec_helper"
require "hamster/hash"

describe Hamster::Hash do
  describe "#fetch" do
    context "with no default provided" do
      context "when the key exists" do
        it "returns the value associated with the key" do
          Hamster.hash("A" => "aye").fetch("A").should == "aye"
        end
      end

      context "when the key does not exist" do
        it "raises a KeyError" do
          -> { Hamster.hash("A" => "aye").fetch("B") }.should raise_error(KeyError)
        end
      end
    end

    context "with a default value" do
      context "when the key exists" do
        it "returns the value associated with the key" do
          Hamster.hash("A" => "aye").fetch("A", "default").should == "aye"
        end
      end

      context "when the key does not exist" do
        it "returns the default value" do
          Hamster.hash("A" => "aye").fetch("B", "default").should == "default"
        end
      end
    end

    context "with a default block" do
      context "when the key exists" do
        it "returns the value associated with the key" do
          Hamster.hash("A" => "aye").fetch("A") { "default".upcase }.should == "aye"
        end
      end

      context "when the key does not exist" do
        it "invokes the default block with the missing key as paramter" do
          Hamster.hash("A" => "aye").fetch("B") { |key| key.should == "B" }
          Hamster.hash("A" => "aye").fetch("B") { "default".upcase }.should == "DEFAULT"
        end
      end
    end

    it "gives precedence to default block over default argument if passed both" do
      Hamster.hash("A" => "aye").fetch("B", 'one') { 'two' }.should == 'two'
    end

    it "raises an ArgumentError when not passed one or 2 arguments" do
      -> { Hamster.hash.fetch }.should raise_error(ArgumentError)
      -> { Hamster.hash.fetch(1, 2, 3) }.should raise_error(ArgumentError)
    end
  end
end