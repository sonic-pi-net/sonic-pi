require "spec_helper"
require "hamster/sorted_set"

describe Hamster::SortedSet do
  [:intersection, :intersect, :&].each do |method|
    describe "##{method}" do
      [
        [[], [], []],
        [["A"], [], []],
        [["A"], ["A"], ["A"]],
        [%w[A B C], ["B"], ["B"]],
        [%w[A B C], %w[A C], %w[A C]],
        [%w[A M T X], %w[B C D E F G H I M P Q T U], %w[M T]]
      ].each do |a, b, expected|
        context "for #{a.inspect} and #{b.inspect}" do
          it "returns #{expected.inspect}" do
            Hamster.sorted_set(*a).send(method, Hamster.sorted_set(*b)).should eql(Hamster.sorted_set(*expected))
          end
        end

        context "for #{b.inspect} and #{a.inspect}" do
          it "returns #{expected.inspect}" do
            Hamster.sorted_set(*b).send(method, Hamster.sorted_set(*a)).should eql(Hamster.sorted_set(*expected))
          end
        end
      end
    end
  end
end