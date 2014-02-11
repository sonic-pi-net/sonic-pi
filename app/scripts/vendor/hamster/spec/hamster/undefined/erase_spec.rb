require "spec_helper"

require "hamster/undefined"

describe Hamster::Undefined do

  describe "#erase" do

    describe "with Undefined" do

      before do
        @result = Hamster::Undefined.erase(Hamster::Undefined)
      end

      it "returns nil" do
        @result.should be_nil
      end

    end

    describe "with nil" do

      before do
        @result = Hamster::Undefined.erase(nil)
      end

      it "returns nil" do
        @result.should be_nil
      end

    end

    describe "otherwise" do

      before do
        @result = Hamster::Undefined.erase("Hello")
      end

      it "returns the value unchanged" do
        @result.should == "Hello"
      end

    end

  end

end
