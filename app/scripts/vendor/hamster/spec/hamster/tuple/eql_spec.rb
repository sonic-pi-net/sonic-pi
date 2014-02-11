require "spec_helper"

require "hamster/tuple"

describe Hamster::Tuple do

  [:eql?, :==].each do |method|

    describe "##{method}" do

      describe "returns false when comparing with" do

        before do
          @tuple = Hamster::Tuple.new("A", "B", "C")
        end

        it "an array with the same values" do
          @tuple.send(method, %w[A B C]).should == false
        end

        it "an aribtrary object" do
          @tuple.send(method, Object.new).should == false
        end

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
            @a = Hamster::Tuple.new(*a)
            @b = Hamster::Tuple.new(*b)
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
