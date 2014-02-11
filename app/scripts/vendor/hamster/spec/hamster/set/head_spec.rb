require "spec_helper"

require "hamster/set"

describe Hamster::Set do

  [:head, :first].each do |method|

    describe "##{method}" do

      describe "on an empty set" do

        before do
          @result = Hamster.set.send(method)
        end

        it "returns nil" do
          @returns.should be_nil
        end

      end

      describe "on a non-empty set" do

        before do
          @result = Hamster.set("A", "B", "C").send(method)
        end

        it "returns an arbitrary value from the set" do
          %w[A B C].should include(@result)
        end

      end

    end

  end

end
