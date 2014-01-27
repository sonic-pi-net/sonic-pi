require 'spec_helper'

describe EDN::Transform do
  context "integer" do
    it "should emit an integer" do
      subject.apply(:integer => "1", :precision => nil).should == 1
    end
  end

  context "float" do
    it "should emit an float" do
      subject.apply(:float => "1.0", :precision => nil).should == 1.0
    end

    it "should emit a BigDecimal if suffixed with M" do
      subject.apply(:float => "1.0", :precision => "M").should == BigDecimal("1.0")
    end
  end

  context "string" do
    it "should emit a string with control characters substituted" do
      subject.apply(:string => 'hello\nworld').should == "hello\nworld"
    end

    it "should not evaluate interpolated Ruby code" do
      subject.apply(:string => 'hello\n#{world}').should == "hello\n\#{world}"
    end
  end

  context "keyword" do
    it "should emit a Ruby symbol" do
      subject.apply(:keyword => "test").should == :test
    end
  end

  context "symbol" do
    it "should emit an EDN symbol" do
      subject.apply(:symbol => "test").should == ~'test'
    end
  end

  context "boolean" do
    it "should emit true or false" do
      subject.apply(:true => "true").should == true
      subject.apply(:false => "false").should == false
    end
  end

  context "nil" do
    it "should emit nil" do
      subject.apply(:nil => "nil").should == nil
    end
  end

  context "character" do
    it "should emit a string" do
      subject.apply(:character => "&").should == "&"
    end

    it "should handle newline, space, and tab special cases" do
      subject.apply(:character => "newline").should == "\n"
      subject.apply(:character => "space").should == " "
      subject.apply(:character => "tab").should == "\t"
    end
  end

  context "vector" do
    it "should emit an array" do
      subject.apply(:vector => []).should == []
      subject.apply(:vector => [{:integer => "1", :precision => nil}, {:string => "abc"}]).should == [1, "abc"]
      subject.apply(:vector => [{:vector => [{:integer => "1", :precision => nil}, {:string => "abc"}]}, {:float => "3.14", :precision => nil}]).should == [[1, "abc"], 3.14]
    end
  end

  context "list" do
    it "should emit a list" do
      subject.apply(:list => []).should == EDN.list
      subject.apply(:list => [{:integer => "1", :precision => nil}, {:string => "abc"}]).should == ~[1, "abc"]
      subject.apply(:list => [{:list => [{:integer => "1", :precision => nil}, {:string => "abc"}]}, {:float => "3.14", :precision => nil}]).should == \
        ~[~[1, "abc"], 3.14]
    end

    it "should be type-compatible with arrays" do
      subject.apply(:list => [{:integer => "1", :precision => nil}, {:string => "abc"}]).should == [1, "abc"]
    end
  end

  context "set" do
    it "should emit a set" do
      subject.apply(:set => []).should == Set.new
      subject.apply(:set => [1, "abc", 2]).should == Set.new([1, "abc", 2])
    end
  end

  context "map" do
    it "should emit a hash" do
      map_tree = {:map=>
        [ {:key=>{:keyword=>{:symbol=>"a"}}, :value=>{:integer=>"1", :precision => nil}},
          {:key=>{:keyword=>{:symbol=>"b"}}, :value=>{:integer=>"2", :precision => nil}}
        ]
      }

      subject.apply(map_tree).should == {:a => 1, :b => 2}
    end

    it "should work with nested maps" do
      map_tree = {:map=>
        [{:key=>{:keyword=>{:symbol=>"a"}}, :value=>{:integer=>"1", :precision => nil}},
          {:key=>{:keyword=>{:symbol=>"b"}}, :value=>{:integer=>"2", :precision => nil}},
          {:key=>
            {:map=>
              [{:key=>{:keyword=>{:symbol=>"c"}}, :value=>{:integer=>"3", :precision => nil}}]},
            :value=>{:integer=>"4", :precision => nil}}]}
      subject.apply(map_tree).should == {:a => 1, :b => 2, {:c => 3} => 4}
    end
  end

  context "tagged element" do
    it "should emit a EDN::Type::Unknown if the tag is not registered" do
      subject.apply({:tag => {:symbol => 'uri'}, :element => {:string => 'http://google.com'}}).should == EDN::Type::Unknown.new("uri", "http://google.com")
    end

    it "should emit the transformed element if the tag is registered" do
      EDN.register("uri", lambda { |uri| URI(uri) })
      subject.apply({:tag => {:symbol => 'uri'}, :element => {:string => 'http://google.com'}}).should == URI("http://google.com")
      EDN.unregister("uri") # cleanup
    end

    it "should work with nested elements" do
      tree = {
        :tag=>{:symbol=>"cnd/awesome"},
        :element=>
        {:vector=>
          [{:keyword=>{:symbol=>"a"}},
            {:list=>
              [{:integer=>"1", :precision=>nil},
                {:list=>
                  [{:integer=>"2", :precision=>nil},
                    {:integer=>"3", :precision=>nil}]}]},
            {:keyword=>{:symbol=>"b"}},
            {:keyword=>{:symbol=>"c"}},
            {:set=>
              [{:integer=>"1", :precision=>nil},
                {:integer=>"2", :precision=>nil},
                {:integer=>"3", :precision=>nil}]},
            {:vector=>
              [{:vector=>
                  [{:vector=>[{:integer=>"42", :precision=>nil}]},
                    {:integer=>"42", :precision=>nil}]}]}]}}
      expected = EDN::Type::Unknown.new('cnd/awesome',
                                    [:a, [1, [2, 3]], :b, :c,
                                          Set.new([1, 2, 3]), [[[42], 42]]])
      subject.apply(tree).should == expected
    end
  end

  context "element with metadata" do
    it "should tag the element with metadata" do
      tree = {
        :metadata =>
          [:map =>
            [
              {:key=>{:keyword=>{:symbol=>"a"}}, :value=>{:integer=>"1", :precision => nil}},
              {:key=>{:keyword=>{:symbol=>"b"}}, :value=>{:integer=>"2", :precision => nil}}]],
        :element => {:vector => [{:integer => "1", :precision => nil}, {:string => "abc"}]}}

      element = subject.apply(tree)
      element.should == [1, "abc"]
      element.metadata.should == {:a => 1, :b => 2}
    end
  end
end
