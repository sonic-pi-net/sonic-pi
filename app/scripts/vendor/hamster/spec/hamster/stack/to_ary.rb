require "spec_helper"
require "hamster/stack"

describe Hamster::Stack do
  let(:stack) { Hamster.stack("D", "C", "B", "A") }

  describe "#to_ary" do
    context "enables implicit conversion to" do
      it "block parameters" do
        def func(&block)
          yield(stack)
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

        func(*stack)
      end

      it "works with splat" do
        array = *stack
        expect(array).to eq(%w[A B C D])
      end
    end
  end
end
