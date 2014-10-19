require "spec_helper"
require "hamster/list"

describe Hamster::List do
  [:reduce, :inject, :fold].each do |method|
    describe "##{method}" do
      context "on a really big list" do
        it "doesn't run out of stack" do
          -> { Hamster.interval(0, STACK_OVERFLOW_DEPTH).send(method, &:+) }.should_not raise_error
        end
      end

      [
        [[], 10, 10],
        [[1], 10, 9],
        [[1, 2, 3], 10, 4],
      ].each do |values, initial, expected|
        context "on #{values.inspect}" do
          context "with an initial value of #{initial} and a block" do
            it "returns #{expected.inspect}" do
              Hamster.list(*values).send(method, initial) { |memo, item| memo - item }.should == expected
            end
          end
        end
      end

      [
        [[], nil],
        [[1], 1],
        [[1, 2, 3], -4],
      ].each do |values, expected|
        context "on #{values.inspect}" do
          context "with no initial value and a block" do
            it "returns #{expected.inspect}" do
              Hamster.list(*values).send(method) { |memo, item| memo - item }.should == expected
            end
          end
        end
      end

      context "with no block and a symbol argument" do
        it "uses the symbol as the name of a method to reduce with" do
          Hamster.list(1, 2, 3).send(method, :+).should == 6
        end
      end

      context "with no block and a string argument" do
        it "uses the string as the name of a method to reduce with" do
          Hamster.list(1, 2, 3).send(method, '+').should == 6
        end
      end
    end
  end

  describe "#foldr" do
    [
      [[], 10, 10],
      [[1], 10, 9],
      [[1, 2, 3], 10, 4],
    ].each do |values, initial, expected|
      context "on #{values.inspect}" do
        context "with an initial value of #{initial} and a block" do
          it "returns #{expected.inspect}" do
            Hamster.list(*values).foldr(initial) { |memo, item| memo - item }.should == expected
          end
        end
      end
    end

    [
      [[], nil],
      [[1], 1],
      [[1, 2, 3], 0],
      [[1, 2, 3, 4], -2]
    ].each do |values, expected|
      context "on #{values.inspect}" do
        context "with no initial value and a block" do
          it "returns #{expected.inspect}" do
            Hamster.list(*values).foldr { |memo, item| memo - item }.should == expected
          end
        end
      end
    end

    context "with no block and a symbol argument" do
      it "uses the symbol as the name of a method to reduce with" do
        Hamster.list(1, 2, 3).foldr(:+).should == 6
      end
    end

    context "with no block and a string argument" do
      it "uses the string as the name of a method to reduce with" do
        Hamster.list(1, 2, 3).foldr('+').should == 6
      end
    end
  end
end
