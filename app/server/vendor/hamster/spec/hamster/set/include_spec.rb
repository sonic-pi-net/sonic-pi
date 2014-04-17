require "spec_helper"

require "hamster/set"

describe Hamster::Set do

  [:include?, :member?, :contains?, :elem?].each do |method|

    describe "##{method}" do

      before do
        @set = Hamster.set("A", "B", "C", 2.0, nil)
      end

      ["A", "B", "C", 2.0, nil].each do |value|

        it "returns true for an existing value (#{value.inspect})" do
          @set.send(method, value).should == true
        end

      end

      it "returns false for a non-existing value" do
        @set.send(method, "D").should == false
      end

      it "uses #eql? for equality" do
        @set.send(method, 2).should == false
      end

    end

  end

end
