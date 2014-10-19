require "spec_helper"
require "hamster/list"

describe Hamster::List do
  [:each, :foreach].each do |method|
    describe "##{method}" do
      context "on a really big list" do
        it "doesn't run out of stack" do
          -> { Hamster.interval(0, STACK_OVERFLOW_DEPTH).send(method) { |item| } }.should_not raise_error
        end
      end

      [
        [],
        ["A"],
        %w[A B C],
      ].each do |values|
        context "on #{values.inspect}" do
          let(:list) { Hamster.list(*values) }

          context "with a block" do
            it "iterates over the items in order" do
              yielded = []
              list.send(method) { |item| yielded << item }
              yielded.should == values
            end

            it "returns nil" do
              list.send(method) { |item| item }.should be_nil
            end
          end

          context "without a block" do
            it "returns an Enumerator" do
              list.send(method).class.should be(Enumerator)
              list.send(method).to_list.should eql(list)
            end
          end
        end
      end
    end
  end
end