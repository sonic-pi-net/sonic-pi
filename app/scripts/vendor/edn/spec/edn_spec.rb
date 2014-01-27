require 'spec_helper'

describe EDN do
  include RantlyHelpers

  context "#read" do
    it "reads single elements" do
      EDN.read("1").should == 1
      EDN.read("3.14").should == 3.14
      EDN.read("3.14M").should == BigDecimal("3.14")
      EDN.read('"hello\nworld"').should == "hello\nworld"
      EDN.read(':hello').should == :hello
      EDN.read(':hello/world').should == :"hello/world"
      EDN.read('hello').should == EDN::Type::Symbol.new('hello')
      EDN.read('hello/world').should == EDN::Type::Symbol.new('hello/world')
      EDN.read('true').should == true
      EDN.read('false').should == false
      EDN.read('nil').should == nil
      EDN.read('\c').should == "c"
    end

    it "should support M suffix without decimals" do
      EDN.read(123412341231212121241234.to_edn).should == 123412341231212121241234
      EDN.read("123412341231212121241234M").should == 123412341231212121241234
    end

    it "reads #inst tagged elements" do
      EDN.read('#inst "2012-09-10T16:16:03-04:00"').should == DateTime.new(2012, 9, 10, 16, 16, 3, '-04:00')
    end

    it "reads vectors" do
      EDN.read('[]').should == []
      EDN.read('[1]').should == [1]
      EDN.read('["hello" 1 2]').should == ['hello', 1, 2]
      EDN.read('[[1 [:hi]]]').should == [[1, [:hi]]]
    end

    it "reads lists" do
      EDN.read('()').should == []
      EDN.read('(1)').should == [1]
      EDN.read('("hello" 1 2)').should == ['hello', 1, 2]
      EDN.read('((1 (:hi)))').should == [[1, [:hi]]]
    end

    it "reads maps" do
      EDN.read('{}').should == {}
      EDN.read('{:a :b}').should == {:a => :b}
      EDN.read('{:a 1, :b 2}').should == {:a => 1, :b => 2}
      EDN.read('{:a {:b :c}}').should == {:a => {:b => :c}}
    end

    it "reads sets" do
      EDN.read('#{}').should == Set.new
      EDN.read('#{1}').should == Set[1]
      EDN.read('#{1 "abc"}').should == Set[1, "abc"]
      EDN.read('#{1 #{:abc}}').should == Set[1, Set[:abc]]
    end

    it "reads any valid element" do
      elements = rant(RantlyHelpers::ELEMENT)
      elements.each do |element|
        begin
          if element == "nil"
            EDN.read(element).should be_nil
          else
            EDN.read(element).should_not be_nil
          end
        rescue Exception => ex
          puts "Bad element: #{element}"
          raise ex
        end
      end
    end
  end

  context "#register" do
    it "uses the identity function when no handler is given" do
      EDN.register "some/tag"
      EDN.read("#some/tag {}").class.should == Hash
    end
  end

  context "writing" do
    it "writes any valid element" do
      elements = rant(RantlyHelpers::ELEMENT)
      elements.each do |element|
        expect {
          begin
            EDN.read(element).to_edn
          rescue Exception => ex
            puts "Bad element: #{element}"
            raise ex
          end
        }.to_not raise_error
      end
    end

    it "writes equivalent edn to what it reads" do
      elements = rant(RantlyHelpers::ELEMENT)
      elements.each do |element|
        ruby_element = EDN.read(element)
        ruby_element.should == EDN.read(ruby_element.to_edn)
        if ruby_element.respond_to?(:metadata)
          ruby_element.metadata.should == EDN.read(ruby_element.to_edn).metadata
        end
      end
    end
  end
end
