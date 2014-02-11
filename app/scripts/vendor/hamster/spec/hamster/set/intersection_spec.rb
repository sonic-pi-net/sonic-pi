require "spec_helper"

require "hamster/set"

describe Hamster::Set do

  [:intersection, :intersect, :&].each do |method|

    describe "##{method}" do

      [
        [[], [], []],
        [["A"], [], []],
        [["A"], ["A"], ["A"]],
        [%w[A B C], ["B"], ["B"]],
        [%w[A B C], %w[A C], %w[A C]],
      ].each do |a, b, expected|

        describe "returns #{expected.inspect}" do

          before do
            @a = Hamster.set(*a)
            @b = Hamster.set(*b)
          end

          it "for #{a.inspect} and #{b.inspect}"  do
            @result = @a.send(method, @b)
          end

          it "for #{b.inspect} and #{a.inspect}"  do
            @result = @b.send(method, @a)
          end

          after  do
            @result.should == Hamster.set(*expected)
          end

        end

      end

    end

  end

end
