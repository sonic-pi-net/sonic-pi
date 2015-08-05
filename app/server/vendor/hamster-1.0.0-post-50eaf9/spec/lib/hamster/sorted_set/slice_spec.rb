require "spec_helper"
require "hamster/sorted_set"

describe Hamster::SortedSet do
  let(:sorted_set) { Hamster.sorted_set(1,2,3,4) }
  let(:big) { Hamster::SortedSet.new(1..10000) }

  [:slice, :[]].each do |method|
    describe "##{method}" do
      context "when passed a positive integral index" do
        it "returns the element at that index" do
          sorted_set.send(method, 0).should be(1)
          sorted_set.send(method, 1).should be(2)
          sorted_set.send(method, 2).should be(3)
          sorted_set.send(method, 3).should be(4)
          sorted_set.send(method, 4).should be(nil)
          sorted_set.send(method, 10).should be(nil)

          big.send(method, 0).should be(1)
          big.send(method, 9999).should be(10000)
        end

        it "leaves the original unchanged" do
          sorted_set.should eql(Hamster.sorted_set(1,2,3,4))
        end
      end

      context "when passed a negative integral index" do
        it "returns the element which is number (index.abs) counting from the end of the sorted_set" do
          sorted_set.send(method, -1).should be(4)
          sorted_set.send(method, -2).should be(3)
          sorted_set.send(method, -3).should be(2)
          sorted_set.send(method, -4).should be(1)
          sorted_set.send(method, -5).should be(nil)
          sorted_set.send(method, -10).should be(nil)

          big.send(method, -1).should be(10000)
          big.send(method, -10000).should be(1)
        end
      end

      context "when passed a positive integral index and count" do
        it "returns 'count' elements starting from 'index'" do
          sorted_set.send(method, 0, 0).should eql(Hamster.sorted_set)
          sorted_set.send(method, 0, 1).should eql(Hamster.sorted_set(1))
          sorted_set.send(method, 0, 2).should eql(Hamster.sorted_set(1,2))
          sorted_set.send(method, 0, 4).should eql(Hamster.sorted_set(1,2,3,4))
          sorted_set.send(method, 0, 6).should eql(Hamster.sorted_set(1,2,3,4))
          sorted_set.send(method, 0, -1).should be_nil
          sorted_set.send(method, 0, -2).should be_nil
          sorted_set.send(method, 0, -4).should be_nil
          sorted_set.send(method, 2, 0).should eql(Hamster.sorted_set)
          sorted_set.send(method, 2, 1).should eql(Hamster.sorted_set(3))
          sorted_set.send(method, 2, 2).should eql(Hamster.sorted_set(3,4))
          sorted_set.send(method, 2, 4).should eql(Hamster.sorted_set(3,4))
          sorted_set.send(method, 2, -1).should be_nil
          sorted_set.send(method, 4, 0).should eql(Hamster.sorted_set)
          sorted_set.send(method, 4, 2).should eql(Hamster.sorted_set)
          sorted_set.send(method, 4, -1).should be_nil
          sorted_set.send(method, 5, 0).should be_nil
          sorted_set.send(method, 5, 2).should be_nil
          sorted_set.send(method, 5, -1).should be_nil
          sorted_set.send(method, 6, 0).should be_nil
          sorted_set.send(method, 6, 2).should be_nil
          sorted_set.send(method, 6, -1).should be_nil

          big.send(method, 0, 3).should eql(Hamster.sorted_set(1,2,3))
          big.send(method, 1023, 4).should eql(Hamster.sorted_set(1024,1025,1026,1027))
          big.send(method, 1024, 4).should eql(Hamster.sorted_set(1025,1026,1027,1028))
        end

        it "leaves the original unchanged" do
          sorted_set.should eql(Hamster.sorted_set(1,2,3,4))
        end
      end

      context "when passed a negative integral index and count" do
        it "returns 'count' elements, starting from index which is number 'index.abs' counting from the end of the array" do
          sorted_set.send(method, -1, 0).should eql(Hamster.sorted_set)
          sorted_set.send(method, -1, 1).should eql(Hamster.sorted_set(4))
          sorted_set.send(method, -1, 2).should eql(Hamster.sorted_set(4))
          sorted_set.send(method, -1, -1).should be_nil
          sorted_set.send(method, -2, 0).should eql(Hamster.sorted_set)
          sorted_set.send(method, -2, 1).should eql(Hamster.sorted_set(3))
          sorted_set.send(method, -2, 2).should eql(Hamster.sorted_set(3,4))
          sorted_set.send(method, -2, 4).should eql(Hamster.sorted_set(3,4))
          sorted_set.send(method, -2, -1).should be_nil
          sorted_set.send(method, -4, 0).should eql(Hamster.sorted_set)
          sorted_set.send(method, -4, 1).should eql(Hamster.sorted_set(1))
          sorted_set.send(method, -4, 2).should eql(Hamster.sorted_set(1,2))
          sorted_set.send(method, -4, 4).should eql(Hamster.sorted_set(1,2,3,4))
          sorted_set.send(method, -4, 6).should eql(Hamster.sorted_set(1,2,3,4))
          sorted_set.send(method, -4, -1).should be_nil
          sorted_set.send(method, -5, 0).should be_nil
          sorted_set.send(method, -5, 1).should be_nil
          sorted_set.send(method, -5, 10).should be_nil
          sorted_set.send(method, -5, -1).should be_nil

          big.send(method, -1, 1).should eql(Hamster.sorted_set(10000))
          big.send(method, -1, 2).should eql(Hamster.sorted_set(10000))
          big.send(method, -6, 2).should eql(Hamster.sorted_set(9995,9996))
        end
      end

      context "when passed a Range" do
        it "returns the elements whose indexes are within the given Range" do
          sorted_set.send(method, 0..-1).should eql(Hamster.sorted_set(1,2,3,4))
          sorted_set.send(method, 0..-10).should eql(Hamster.sorted_set)
          sorted_set.send(method, 0..0).should eql(Hamster.sorted_set(1))
          sorted_set.send(method, 0..1).should eql(Hamster.sorted_set(1,2))
          sorted_set.send(method, 0..2).should eql(Hamster.sorted_set(1,2,3))
          sorted_set.send(method, 0..3).should eql(Hamster.sorted_set(1,2,3,4))
          sorted_set.send(method, 0..4).should eql(Hamster.sorted_set(1,2,3,4))
          sorted_set.send(method, 0..10).should eql(Hamster.sorted_set(1,2,3,4))
          sorted_set.send(method, 2..-10).should eql(Hamster.sorted_set)
          sorted_set.send(method, 2..0).should eql(Hamster.sorted_set)
          sorted_set.send(method, 2..2).should eql(Hamster.sorted_set(3))
          sorted_set.send(method, 2..3).should eql(Hamster.sorted_set(3,4))
          sorted_set.send(method, 2..4).should eql(Hamster.sorted_set(3,4))
          sorted_set.send(method, 3..0).should eql(Hamster.sorted_set)
          sorted_set.send(method, 3..3).should eql(Hamster.sorted_set(4))
          sorted_set.send(method, 3..4).should eql(Hamster.sorted_set(4))
          sorted_set.send(method, 4..0).should eql(Hamster.sorted_set)
          sorted_set.send(method, 4..4).should eql(Hamster.sorted_set)
          sorted_set.send(method, 4..5).should eql(Hamster.sorted_set)
          sorted_set.send(method, 5..0).should be_nil
          sorted_set.send(method, 5..5).should be_nil
          sorted_set.send(method, 5..6).should be_nil

          big.send(method, 159..162).should eql(Hamster.sorted_set(160,161,162,163))
          big.send(method, 160..162).should eql(Hamster.sorted_set(161,162,163))
          big.send(method, 161..162).should eql(Hamster.sorted_set(162,163))
          big.send(method, 9999..10100).should eql(Hamster.sorted_set(10000))
          big.send(method, 10000..10100).should eql(Hamster.sorted_set)
          big.send(method, 10001..10100).should be_nil

          sorted_set.send(method, 0...-1).should eql(Hamster.sorted_set(1,2,3))
          sorted_set.send(method, 0...-10).should eql(Hamster.sorted_set)
          sorted_set.send(method, 0...0).should eql(Hamster.sorted_set)
          sorted_set.send(method, 0...1).should eql(Hamster.sorted_set(1))
          sorted_set.send(method, 0...2).should eql(Hamster.sorted_set(1,2))
          sorted_set.send(method, 0...3).should eql(Hamster.sorted_set(1,2,3))
          sorted_set.send(method, 0...4).should eql(Hamster.sorted_set(1,2,3,4))
          sorted_set.send(method, 0...10).should eql(Hamster.sorted_set(1,2,3,4))
          sorted_set.send(method, 2...-10).should eql(Hamster.sorted_set)
          sorted_set.send(method, 2...0).should eql(Hamster.sorted_set)
          sorted_set.send(method, 2...2).should eql(Hamster.sorted_set)
          sorted_set.send(method, 2...3).should eql(Hamster.sorted_set(3))
          sorted_set.send(method, 2...4).should eql(Hamster.sorted_set(3,4))
          sorted_set.send(method, 3...0).should eql(Hamster.sorted_set)
          sorted_set.send(method, 3...3).should eql(Hamster.sorted_set)
          sorted_set.send(method, 3...4).should eql(Hamster.sorted_set(4))
          sorted_set.send(method, 4...0).should eql(Hamster.sorted_set)
          sorted_set.send(method, 4...4).should eql(Hamster.sorted_set)
          sorted_set.send(method, 4...5).should eql(Hamster.sorted_set)
          sorted_set.send(method, 5...0).should be_nil
          sorted_set.send(method, 5...5).should be_nil
          sorted_set.send(method, 5...6).should be_nil

          big.send(method, 159...162).should eql(Hamster.sorted_set(160,161,162))
          big.send(method, 160...162).should eql(Hamster.sorted_set(161,162))
          big.send(method, 161...162).should eql(Hamster.sorted_set(162))
          big.send(method, 9999...10100).should eql(Hamster.sorted_set(10000))
          big.send(method, 10000...10100).should eql(Hamster.sorted_set)
          big.send(method, 10001...10100).should be_nil

          sorted_set.send(method, -1..-1).should eql(Hamster.sorted_set(4))
          sorted_set.send(method, -1...-1).should eql(Hamster.sorted_set)
          sorted_set.send(method, -1..3).should eql(Hamster.sorted_set(4))
          sorted_set.send(method, -1...3).should eql(Hamster.sorted_set)
          sorted_set.send(method, -1..4).should eql(Hamster.sorted_set(4))
          sorted_set.send(method, -1...4).should eql(Hamster.sorted_set(4))
          sorted_set.send(method, -1..10).should eql(Hamster.sorted_set(4))
          sorted_set.send(method, -1...10).should eql(Hamster.sorted_set(4))
          sorted_set.send(method, -1..0).should eql(Hamster.sorted_set)
          sorted_set.send(method, -1..-4).should eql(Hamster.sorted_set)
          sorted_set.send(method, -1...-4).should eql(Hamster.sorted_set)
          sorted_set.send(method, -1..-6).should eql(Hamster.sorted_set)
          sorted_set.send(method, -1...-6).should eql(Hamster.sorted_set)
          sorted_set.send(method, -2..-2).should eql(Hamster.sorted_set(3))
          sorted_set.send(method, -2...-2).should eql(Hamster.sorted_set)
          sorted_set.send(method, -2..-1).should eql(Hamster.sorted_set(3,4))
          sorted_set.send(method, -2...-1).should eql(Hamster.sorted_set(3))
          sorted_set.send(method, -2..10).should eql(Hamster.sorted_set(3,4))
          sorted_set.send(method, -2...10).should eql(Hamster.sorted_set(3,4))

          big.send(method, -1..-1).should eql(Hamster.sorted_set(10000))
          big.send(method, -1..9999).should eql(Hamster.sorted_set(10000))
          big.send(method, -1...9999).should eql(Hamster.sorted_set)
          big.send(method, -2...9999).should eql(Hamster.sorted_set(9999))
          big.send(method, -2..-1).should eql(Hamster.sorted_set(9999,10000))

          sorted_set.send(method, -4..-4).should eql(Hamster.sorted_set(1))
          sorted_set.send(method, -4..-2).should eql(Hamster.sorted_set(1,2,3))
          sorted_set.send(method, -4...-2).should eql(Hamster.sorted_set(1,2))
          sorted_set.send(method, -4..-1).should eql(Hamster.sorted_set(1,2,3,4))
          sorted_set.send(method, -4...-1).should eql(Hamster.sorted_set(1,2,3))
          sorted_set.send(method, -4..3).should eql(Hamster.sorted_set(1,2,3,4))
          sorted_set.send(method, -4...3).should eql(Hamster.sorted_set(1,2,3))
          sorted_set.send(method, -4..4).should eql(Hamster.sorted_set(1,2,3,4))
          sorted_set.send(method, -4...4).should eql(Hamster.sorted_set(1,2,3,4))
          sorted_set.send(method, -4..0).should eql(Hamster.sorted_set(1))
          sorted_set.send(method, -4...0).should eql(Hamster.sorted_set)
          sorted_set.send(method, -4..1).should eql(Hamster.sorted_set(1,2))
          sorted_set.send(method, -4...1).should eql(Hamster.sorted_set(1))

          sorted_set.send(method, -5..-5).should be_nil
          sorted_set.send(method, -5...-5).should be_nil
          sorted_set.send(method, -5..-4).should be_nil
          sorted_set.send(method, -5..-1).should be_nil
          sorted_set.send(method, -5..10).should be_nil

          big.send(method, -10001..-1).should be_nil
        end

        it "leaves the original unchanged" do
          sorted_set.should eql(Hamster.sorted_set(1,2,3,4))
        end
      end
    end

    context "when passed a subclass of Range" do
      it "works the same as with a Range" do
        subclass = Class.new(Range)
        sorted_set.send(method, subclass.new(1,2)).should eql(Hamster.sorted_set(2,3))
        sorted_set.send(method, subclass.new(-3,-1,true)).should eql(Hamster.sorted_set(2,3))
      end
    end

    context "on a subclass of SortedSet" do
      it "with index and count or a range, returns an instance of the subclass" do
        subclass = Class.new(Hamster::SortedSet)
        instance = subclass.new([1,2,3])
        instance.send(method, 0, 0).class.should be(subclass)
        instance.send(method, 0, 2).class.should be(subclass)
        instance.send(method, 0..0).class.should be(subclass)
        instance.send(method, 1..-1).class.should be(subclass)
      end
    end
  end
end