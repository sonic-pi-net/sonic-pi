require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do
  describe "#group_by" do
    context "with a block" do
      [
        [[], []],
        [[1], [true => Hamster.vector(1)]],
        [[1, 2, 3, 4], [true => Hamster.vector(1, 3), false => Hamster.vector(2, 4)]],
      ].each do |values, expected|
        context "on #{values.inspect}" do
          let(:vector) { Hamster.vector(*values) }

          it "returns #{expected.inspect}" do
            vector.group_by(&:odd?).should eql(Hamster.hash(*expected))
            vector.should eql(Hamster::Vector.new(values)) # make sure it hasn't changed
          end
        end
      end
    end

    context "without a block" do
      [
        [[], []],
        [[1], [1 => Hamster.vector(1)]],
        [[1, 2, 3, 4], [1 => Hamster.vector(1), 2 => Hamster.vector(2), 3 => Hamster.vector(3), 4 => Hamster.vector(4)]],
      ].each do |values, expected|
        context "on #{values.inspect}" do
          let(:vector) { Hamster.vector(*values) }

          it "returns #{expected.inspect}" do
            vector.group_by.should eql(Hamster.hash(*expected))
            vector.should eql(Hamster::Vector.new(values)) # make sure it hasn't changed
          end
        end
      end
    end

    context "on an empty vector" do
      it "returns an empty hash" do
        Hamster.vector.group_by { |x| x }.should eql(Hamster.hash)
      end
    end

    it "returns a hash without default proc" do
      Hamster.vector(1,2,3).group_by { |x| x }.default_proc.should be_nil
    end

    context "from a subclass" do
      it "returns an Hash whose values are instances of the subclass" do
        subclass = Class.new(Hamster::Vector)
        instance = subclass.new([1, 'string', :symbol])
        instance.group_by { |x| x.class }.values.each { |v| v.class.should be(subclass) }
      end
    end
  end
end