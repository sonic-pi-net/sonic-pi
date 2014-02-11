require "spec_helper"

require "hamster/list"

describe Hamster::List do
  let(:list) { Hamster.list("A", "B", "C", "D") }

  describe "#to_ary" do
    context "on a really big list" do
      let(:list) { Hamster.interval(0, STACK_OVERFLOW_DEPTH) }

      it "doesn't run out of stack" do
        -> { list.to_ary }.should_not raise_error
      end
    end

    context "enables implicit conversion to" do
      it "block parameters" do
        def func(&block)
          yield(list)
        end

        func do |a, b, *c|
          expect(a).to eq("A")
          expect(b).to eq("B")
          expect(c).to eq(%w[C D])
        end
      end

      it "method arguments" do
        def func(a, b, *c)
          expect(a).to eq("A")
          expect(b).to eq("B")
          expect(c).to eq(%w[C D])
        end
        func(*list)
      end

      it "works with splat" do
        array = *list
        expect(array).to eq(%w[A B C D])
      end
    end
  end
end
