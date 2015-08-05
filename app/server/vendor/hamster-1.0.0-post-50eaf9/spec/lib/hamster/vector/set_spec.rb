require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do
  let(:vector) { Hamster.vector(*values) }

  describe "#set" do
    context "when empty" do
      let(:vector) { Hamster.vector }

      it "always raises an error" do
        (-1..1).each do |i|
          expect { vector.set(i) }.to raise_error
        end
      end
    end

    context "when not empty" do
      let(:vector) { Hamster.vector("A", "B", "C") }

      context "with a block" do
        context "and a positive index" do
          context "within the absolute bounds of the vector" do
            it "passes the current value to the block" do
              vector.set(1) { |value| value.should == "B" }
            end

            it "replaces the value with the result of the block" do
              result = vector.set(1) { |value| "FLIBBLE" }
              result.should eql(Hamster.vector("A", "FLIBBLE", "C"))
            end

            it "supports to_proc methods" do
              result = vector.set(1, &:downcase)
              result.should eql(Hamster.vector("A", "b", "C"))
            end
          end

          context "just past the end of the vector" do
            it "passes nil to the block and adds a new value" do
              result = vector.set(3) { |value| value.should be_nil; "D" }
              result.should eql(Hamster.vector("A", "B", "C", "D"))
            end
          end

          context "further outside the bounds of the vector" do
            it "raises an error" do
              expect { vector.set(4) {} }.to raise_error
            end
          end
        end

        context "and a negative index" do
          context "within the absolute bounds of the vector" do
            it "passes the current value to the block" do
              vector.set(-2) { |value| value.should == "B" }
            end

            it "replaces the value with the result of the block" do
              result = vector.set(-2) { |value| "FLIBBLE" }
              result.should eql(Hamster.vector("A", "FLIBBLE", "C"))
            end

            it "supports to_proc methods" do
              result = vector.set(-2, &:downcase)
              result.should eql(Hamster.vector("A", "b", "C"))
            end
          end

          context "outside the absolute bounds of the vector" do
            it "raises an error" do
              expect { vector.set(-vector.size.next) {} }.to raise_error
            end
          end
        end
      end

      context "with a value" do
        context "and a positive index" do
          context "within the absolute bounds of the vector" do
            let(:set) { vector.set(1, "FLIBBLE") }

            it "preserves the original" do
              vector.should eql(Hamster.vector("A", "B", "C"))
            end

            it "sets the new value at the specified index" do
              set.should eql(Hamster.vector("A", "FLIBBLE", "C"))
            end
          end

          context "just past the end of the vector" do
            it "adds a new value" do
              result = vector.set(3, "FLIBBLE")
              result.should eql(Hamster.vector("A", "B", "C", "FLIBBLE"))
            end
          end

          context "outside the absolute bounds of the vector" do
            it "raises an error" do
              expect { vector.set(4, "FLIBBLE") }.to raise_error
            end
          end
        end

        context "with a negative index" do
          let(:set) { vector.set(-2, "FLIBBLE") }

          it "preserves the original" do
            set
            vector.should eql(Hamster.vector("A", "B", "C"))
          end

          it "sets the new value at the specified index" do
            set.should eql(Hamster.vector("A", "FLIBBLE", "C"))
          end
        end

        context "outside the absolute bounds of the vector" do
          it "raises an error" do
            expect { vector.set(-vector.size.next, "FLIBBLE") }.to raise_error
          end
        end
      end
    end

    context "from a subclass" do
      it "returns an instance of the subclass" do
        subclass = Class.new(Hamster::Vector)
        instance = subclass[1,2,3]
        instance.set(1, 2.5).class.should be(subclass)
      end
    end

    [10, 31, 32, 33, 1000, 1023, 1024, 1025, 2000].each do |size|
      context "on a #{size}-item vector" do
        it "works correctly" do
          array = (1..size).to_a
          vector = V.new(array)

          [0, 1, 10, 31, 32, 33, 100, 500, 1000, 1023, 1024, 1025, 1998, 1999].select { |n| n < size }.each do |i|
            value = rand(10000)
            array[i] = value
            vector = vector.set(i, value)
            vector[i].should be(value)
          end

          0.upto(size-1) do |i|
            vector.get(i).should == array[i]
          end
        end
      end
    end
  end
end
