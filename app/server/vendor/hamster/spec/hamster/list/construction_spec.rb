require "spec_helper"

require "hamster/list"

describe Hamster do

  describe ".list" do

    describe "with no arguments" do

      before do
        @list = Hamster.list
      end

      it "always returns the same instance" do
        @list.should equal(Hamster.list)
      end

      it "returns an empty list" do
        @list.should be_empty
      end

    end

    describe "with a number of items" do

      before do
        @list = Hamster.list("A", "B", "C")
      end

      it "always returns a different instance" do
        @list.should_not equal(Hamster.list("A", "B", "C"))
      end

      it "is the same as repeatedly using #cons" do
        @list.should == Hamster.list.cons("C").cons("B").cons("A")
      end

    end

  end

  describe ".stream" do

    describe "with no block" do

      before do
        @list = Hamster.stream
      end

      it "returns an empty list" do
        @list.should == Hamster.list
      end

    end

    describe "with a block" do

      before do
        count = 0
        @list = Hamster.stream { count += 1 }
      end

      it "repeatedly calls the block" do
        @list.take(5).should == Hamster.list(1, 2, 3, 4, 5)
      end

    end

  end

  [:interval, :range].each do |method|

    describe ".#{method}" do

      describe "for numbers" do

        before do
          @list = Hamster.send(method, 98, 102)
        end

        it "is equivalent to a list with explicit values" do
          @list.should == Hamster.list(98, 99, 100, 101, 102)
        end

      end

      describe "for strings" do

        before do
          @list = Hamster.send(method, "A", "AA")
        end

        it "is equivalent to a list with explicit values" do
          @list.should == Hamster.list("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "AA")
        end

      end

    end

  end

  describe ".repeat" do

    before do
      @list = Hamster.repeat("A")
    end

    it "returns an infinite list with specified value for each element" do
      @list.take(5).should == Hamster.list("A", "A", "A", "A", "A")
    end

  end

  describe ".replicate" do

    before do
      @list = Hamster.replicate(5, "A")
    end

    it "returns a list with the specified value repeated the specified number of times" do
      @list.should == Hamster.list("A", "A", "A", "A", "A")
    end

  end

  describe ".iterate" do

    before do
      @list = Hamster.iterate(1) { |item| item * 2 }
    end

    it "returns an infinite list where the first item is calculated by applying the block on the initial argument, the second item by applying the function on the previous result and so on" do
      @list.take(10).should == Hamster.list(1, 2, 4, 8, 16, 32, 64, 128, 256, 512)
    end

  end

  describe ".enumerate" do
    let(:counter) do
      i = 0
      -> { i += 1 }
    end

    let(:enum) do
      Enumerator.new do |yielder|
        yielder << 1
        yielder << 2
        yielder << 3
        raise "list fully realized"
      end
    end

    let(:list) { Hamster.enumerate(enum) }

    it "should return a list based on the values yielded from the enumerator" do
      expect(list.take(2)).to eq Hamster.list(1, 2)
    end

    it "should realize values as they are needed" do
      expect { list.take(3).to_a }.to raise_exception
    end
  end

end
