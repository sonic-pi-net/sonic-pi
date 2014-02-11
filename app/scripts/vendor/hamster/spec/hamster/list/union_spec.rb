require "spec_helper"

require "hamster/list"

describe Hamster::List do

  [:union, :|].each do |method|

    describe "##{method}" do

      it "is lazy" do
        -> { Hamster.stream { fail }.union(Hamster.stream { fail }) }.should_not raise_error
      end

      [
        [[], [], []],
        [["A"], [], ["A"]],
        [%w[A B C], [], %w[A B C]],
        [%w[A A], ["A"], ["A"]],
      ].each do |a, b, expected|

        describe "returns #{expected.inspect}" do

          before do
            @a = Hamster.list(*a)
            @b = Hamster.list(*b)
          end

          it "for #{a.inspect} and #{b.inspect}"  do
            @result = @a.send(method, @b)
          end

          it "for #{b.inspect} and #{a.inspect}"  do
            @result = @b.send(method, @a)
          end

          after  do
            @result.should == Hamster.list(*expected)
          end

        end

      end

    end

  end

end
