require "spec_helper"
require "hamster/queue"

describe Hamster::Queue do
  let(:queue) { Hamster.queue("A", "B", "C", "D") }

  describe "#to_ary" do
    context "enables implicit conversion to" do
      it "block parameters" do
        def func(&block)
          yield(queue)
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
        func(*queue)
      end

      it "works with splat" do
        array = *queue
        expect(array).to eq(%w[A B C D])
      end
    end
  end
end
