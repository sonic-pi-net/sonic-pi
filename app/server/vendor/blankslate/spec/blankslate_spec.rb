require 'spec_helper'

describe BlankSlate do
  let(:blank_slate) { BlankSlate.new }
  
  def call(obj, meth, *args)
    BlankSlate.find_hidden_method(meth).bind(obj).call(*args)
  end
  
  describe "cleanliness" do
    it "should not have many methods" do
      BlankSlate.instance_methods.
        map(&:to_s).sort.
        should == ["__id__", "__send__", "instance_eval"]
    end 
  end
  
  context "when methods are added to Object" do
    after(:each) { 
      class Object
        undef :foo
      end
    }

    it "should still be blank" do
      class Object 
        def foo
        end
      end
      Object.new.foo
      
      lambda {
        BlankSlate.new.foo
      }.should raise_error(NoMethodError)
    end 
    
  end
end