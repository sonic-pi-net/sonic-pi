require "spec_helper"
require "hamster/list"

describe Hamster::List do
  [
    [[], :car, nil],
    [["A"], :car, "A"],
    [%w[A B C], :car, "A"],
    [%w[A B C], :cadr, "B"],
    [%w[A B C], :caddr, "C"],
    [%w[A B C], :cadddr, nil],
    [%w[A B C], :caddddr, nil],
    [[], :cdr, Hamster.list],
    [["A"], :cdr, Hamster.list],
    [%w[A B C], :cdr, Hamster.list("B", "C")],
    [%w[A B C], :cddr, Hamster.list("C")],
    [%w[A B C], :cdddr, Hamster.list],
    [%w[A B C], :cddddr, Hamster.list],
  ].each do |values, method, expected|
    describe "##{method}" do
      it "is responded to" do
        Hamster.list.respond_to?(method).should == true
      end

      context "on #{values.inspect}" do
        let(:list) { Hamster.list(*values) }

        it "preserves the original" do
          list.send(method)
          list.should eql(Hamster.list(*values))
        end

        it "returns #{expected.inspect}" do
          list.send(method).should == expected
        end
      end
    end
  end
end