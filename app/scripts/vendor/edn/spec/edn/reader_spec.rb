require 'spec_helper'

describe EDN::Reader do
  let(:reader) { EDN::Reader.new("[1 2] 3 :a {:b c}") }

  it "should respond to count" do
    reader.count.should == 4
  end

  it "should respond to each" do
    reader.each do |element|
      element.should_not be_nil
    end
  end

  it "should return an Enumerator from each if no block given" do
    reader.each.should be_a(Enumerator)
  end

  it "should respond to map" do
    reader.map { |x| x }.should == [[1, 2], 3, :a, {:b => ~"c"}]
  end
end
