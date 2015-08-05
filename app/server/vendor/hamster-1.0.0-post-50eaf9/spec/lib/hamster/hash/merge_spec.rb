require "spec_helper"
require "hamster/hash"

describe Hamster::Hash do
  [:merge, :+].each do |method|
    describe "##{method}" do
      [
        [{}, {}, {}],
        [{"A" => "aye"}, {}, {"A" => "aye"}],
        [{"A" => "aye"}, {"A" => "bee"}, {"A" => "bee"}],
        [{"A" => "aye"}, {"B" => "bee"}, {"A" => "aye", "B" => "bee"}],
        [(1..300).zip(1..300), (150..450).zip(150..450), (1..450).zip(1..450)]
      ].each do |a, b, expected|
        context "for #{a.inspect} and #{b.inspect}" do
          let(:hash_a) { Hamster.hash(a) }
          let(:hash_b) { Hamster.hash(b) }
          let(:result) { hash_a.send(method, hash_b) }

          it "returns #{expected.inspect} when passed a Hamster::Hash"  do
            result.should eql(Hamster.hash(expected))
          end

          it "returns #{expected.inspect} when passed a Ruby Hash" do
            Hamster.hash(a).send(method, ::Hash[b]).should eql(Hamster.hash(expected))
          end

          it "doesn't change the original Hashes" do
            result
            hash_a.should eql(Hamster.hash(a))
            hash_b.should eql(Hamster.hash(b))
          end
        end
      end

      context "when merging with an empty Hash" do
        it "returns self" do
          hash = Hamster.hash(a: 1, b: 2)
          hash.send(method, Hamster.hash).should be(hash)
        end
      end

      context "when called on a subclass" do
        it "returns an instance of the subclass" do
          subclass = Class.new(Hamster::Hash)
          instance = subclass.new(a: 1, b: 2)
          instance.merge(c: 3, d: 4).class.should be(subclass)
        end
      end

      it "sets any duplicate key to the value of block if passed a block" do
        h1 = Hamster.hash(:a => 2, :b => 1, :d => 5)
        h2 = Hamster.hash(:a => -2, :b => 4, :c => -3)
        r = h1.merge(h2) { |k,x,y| nil }
        r.should eql(Hamster.hash(:a => nil, :b => nil, :c => -3, :d => 5))

        r = h1.merge(h2) { |k,x,y| "#{k}:#{x+2*y}" }
        r.should eql(Hamster.hash(:a => "a:-2", :b => "b:9", :c => -3, :d => 5))

        lambda {
          h1.merge(h2) { |k, x, y| raise(IndexError) }
        }.should raise_error(IndexError)

        r = h1.merge(h1) { |k,x,y| :x }
        r.should eql(Hamster.hash(:a => :x, :b => :x, :d => :x))
      end

      it "yields key/value pairs in the same order as #each" do
        hash = Hamster.hash(a: 1, b: 2, c: 3)
        each_pairs = []
        merge_pairs = []
        hash.each { |k, v| each_pairs << [k, v] }
        hash.merge(hash) { |k, v1, v2| merge_pairs << [k, v1] }
        each_pairs.should == merge_pairs
      end
    end
  end
end