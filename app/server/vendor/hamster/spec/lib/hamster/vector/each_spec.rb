require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do
  [:each, :foreach].each do |method|
    describe "##{method}" do
      describe "with no block" do
        let(:vector) { Hamster.vector("A", "B", "C") }

        it "returns an Enumerator" do
          vector.send(method).class.should be(Enumerator)
          vector.send(method).to_a.should == vector
        end
      end

      [31, 32, 33, 1023, 1024, 1025].each do |size|
        context "on a #{size}-item vector" do
          describe "with a block" do
            let(:vector) { V.new(1..size) }

            it "returns self" do
              items = []
              vector.send(method) { |item| items << item }.should be(vector)
            end

            it "yields all the items" do
              items = []
              vector.send(method) { |item| items << item }
              items.should == (1..size).to_a
            end

            it "iterates over the items in order" do
              vector.send(method).first.should == 1
              vector.send(method).to_a.last.should == size
            end
          end
        end
      end

      context "on an empty vector" do
        it "doesn't yield anything" do
          Hamster.vector.each { fail }
        end
      end
    end
  end
end