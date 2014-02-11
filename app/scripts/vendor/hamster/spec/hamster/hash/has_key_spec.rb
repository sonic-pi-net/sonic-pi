require "spec_helper"

require "hamster/hash"

describe Hamster::Hash do

  [:key?, :has_key?, :include?, :member?].each do |method|

    describe "##{method}" do

      before do
        @hash = Hamster.hash("A" => "aye", "B" => "bee", "C" => "see", nil => "NIL", 2.0 => "two")
      end

      ["A", "B", "C", nil, 2.0].each do |key|

        it "returns true for an existing key (#{key.inspect})" do
          @hash.send(method, key).should == true
        end

      end

      it "returns false for a non-existing key" do
        @hash.send(method, "D").should == false
      end

      it "uses #eql? for equality" do
        @hash.send(method, 2).should == false
      end

    end

  end

end
