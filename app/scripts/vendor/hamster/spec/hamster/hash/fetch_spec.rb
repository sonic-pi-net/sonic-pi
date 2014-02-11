require "spec_helper"

require "hamster/hash"

describe Hamster::Hash do

  describe "#fetch" do

    describe "with no default provided" do

      describe "when the key exists" do

        before do
          @hash = Hamster.hash("A" => "aye")
        end

        it "returns the value associated with the key" do
          @hash.fetch("A").should == "aye"
        end

      end

      describe "when the key does not exist" do

        before do
          @hash = Hamster.hash("A" => "aye")
        end

        it "raises a KeyError" do
          -> { @hash.fetch("B") }.should raise_error(KeyError)
        end

      end

    end

    describe "with a default value" do

      describe "when the key exists" do

        before do
          @hash = Hamster.hash("A" => "aye")
        end

        it "returns the value associated with the key" do
          @hash.fetch("A", "default").should == "aye"
        end

      end

      describe "when the key does not exist" do

        before do
          @hash = Hamster.hash("A" => "aye")
        end

        it "returns the default value" do
          @hash.fetch("B", "default").should == "default"
        end

      end

    end

    describe "with a default block" do

      describe "when the key exists" do

        before do
          @hash = Hamster.hash("A" => "aye")
        end

        it "returns the value associated with the key" do
          @hash.fetch("A") { "default".upcase }.should == "aye"
        end

      end

      describe "when the key does not exist" do

        before do
          @hash = Hamster.hash("A" => "aye")
        end

        it "returns the default value" do
          @hash.fetch("B") { "default".upcase }.should == "DEFAULT"
        end

      end

    end

  end

end
