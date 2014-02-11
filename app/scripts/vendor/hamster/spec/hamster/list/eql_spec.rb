require "spec_helper"

require "hamster/list"

describe Hamster::List do

  [:eql?, :==].each do |method|

    describe "##{method}" do

      describe "on a really big list" do

        before do
          @a = Hamster.interval(0, STACK_OVERFLOW_DEPTH)
          @b = Hamster.interval(0, STACK_OVERFLOW_DEPTH)
        end

        it "doesn't run out of stack" do
          -> { @a.send(method, @b) }.should_not raise_error
        end

      end

      describe "returns false when comparing with" do

        before do
          @list = Hamster.list("A", "B", "C")
        end

        it "an array with the same contents" do
          @list.send(method, %w[A B C]).should == false
        end

        it "an aribtrary object" do
          @list.send(method, Object.new).should == false
        end

      end

      it "returns false when comparing an empty list with an empty array" do
        Hamster.list.send(method, []).should == false
      end

      [
        [[], [], true],
        [[], [nil], false],
        [["A"], [], false],
        [["A"], ["A"], true],
        [["A"], ["B"], false],
        [%w[A B], ["A"], false],
        [%w[A B C], %w[A B C], true],
        [%w[C A B], %w[A B C], false],
      ].each do |a, b, expected|

        describe "returns #{expected.inspect}" do

          before do
            @a = Hamster.list(*a)
            @b = Hamster.list(*b)
          end

          it "for lists #{a.inspect} and #{b.inspect}" do
            @a.send(method, @b).should == expected
          end

          it "for lists #{b.inspect} and #{a.inspect}" do
            @b.send(method, @a).should == expected
          end

        end

      end

    end

  end

end
