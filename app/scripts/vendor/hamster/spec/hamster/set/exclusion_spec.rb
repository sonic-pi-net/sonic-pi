require "spec_helper"

require "hamster/set"

describe Hamster::Set do

  [:exclusion, :^].each do |method|

    describe "##{method}" do

      [
        [[], [], []],
        [["A"], [], ["A"]],
        [["A"], ["A"], []],
        [%w[A B C], ["B"], %w[A C]],
        [%w[A B C], %w[B C D], %w[A D]],
        [%w[A B C], %w[D E F], %w[A B C D E F]],
      ].each do |a, b, expected|

        describe "for #{a.inspect} and #{b.inspect}" do

          before do
            @result = Hamster.set(*a).send(method, Hamster.set(*b))
          end

          it "returns #{expected.inspect}"  do
            @result.should == Hamster.set(*expected)
          end

        end

      end

    end

  end

end
