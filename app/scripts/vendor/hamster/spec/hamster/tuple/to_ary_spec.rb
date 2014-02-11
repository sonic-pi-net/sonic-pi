require "spec_helper"

require "hamster/tuple"

describe Hamster::Tuple do
  let(:tuple) { Hamster::Tuple.new("A", "B", "C", "D") }

  describe "#to_ary" do
    context "enables implicit conversion to" do
      it "block parameters" do
        def func(&block)
          yield(tuple)
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

        func(*tuple)
      end

      it "works with splat" do
        array = *tuple
        expect(array).to eq(%w[A B C D])
      end
    end
  end
end
