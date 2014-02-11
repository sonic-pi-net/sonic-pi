require "spec_helper"

require "hamster/stack"

describe Hamster::Stack do

  [:eql?, :==].each do |method|

    describe "##{method}" do

      describe "returns false when comparing with" do

        before do
          @stack = Hamster.stack("A", "B", "C")
        end

        it "a list" do
          @stack.send(method, Hamster.list("C", "B", "A")).should == false
        end

        it "an aribtrary object" do
          @stack.send(method, Object.new).should == false
        end

      end

      [
        [[], [], true],
        [["A"], [], false],
        [["A"], ["A"], true],
        [["A"], ["B"], false],
        [%w[A B], ["A"], false],
        [%w[A B C], %w[A B C], true],
        [%w[C A B], %w[A B C], false],
      ].each do |a, b, expected|

        describe "returns #{expected.inspect}" do

          before do
            @a = Hamster.stack(*a)
            @b = Hamster.stack(*b)
          end

          it "for #{a.inspect} and #{b.inspect}" do
            @a.send(method, @b).should == expected
          end

          it "for #{b.inspect} and #{a.inspect}" do
            @b.send(method, @a).should == expected
          end

        end

      end

    end

  end

end
