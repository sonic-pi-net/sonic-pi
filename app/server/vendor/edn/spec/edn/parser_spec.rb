require 'spec_helper'

describe EDN::Parser do
  include RantlyHelpers

  let(:parser) { EDN::Parser.new }

  it "can contain comments" do
    edn = ";; This is some sample data\n[1 2 ;; the first two values\n3]"
    parser.should parse(edn)
    parser.parse(edn).should ==
      {:vector=>[{:integer=>"1", :precision=>nil},
                 {:integer=>"2", :precision=>nil},
                 {:integer=>"3", :precision=>nil}]}
  end

  it "can discard using the discard reader macro" do
    edn = "[1 2 #_3 {:foo #_bar baz}]"
    parser.should parse(edn)
    parser.parse(edn).should ==
      {:vector=>[{:integer=>"1", :precision=>nil},
                 {:integer=>"2", :precision=>nil},
                 {:map => [{:key=>{:keyword=>{:symbol=>"foo"}}, :value=>{:symbol=>"baz"}}]}]}
  end

  context "element" do
    it "should consume nil" do
      parser.element.should parse("nil")
    end

    it "should consume metadata with the element" do
      parser.element.should parse('^{:doc "test"} [1 2]')
    end
  end

  context "comment" do
    it "should consume and throw away comments" do
      comment = "; this is a comment"
      parser.comment.should parse(comment)
      parser.comment.parse(comment).should == "; this is a comment"
    end
  end

  context "boolean" do
    it "should consume true" do
      parser.boolean.should parse("true")
    end

    it "should consume false" do
      parser.boolean.should parse("false")
    end
  end

  context "integer" do
    it "should consume integers" do
      rant(RantlyHelpers::INTEGER).each do |int|
        parser.integer.should parse(int)
      end
    end

    it "should consume integers prefixed with a +" do
      rant(RantlyHelpers::INTEGER).each do |int|
        parser.integer.should parse("+#{int.to_i.abs.to_s}")
      end
    end
  end

  context "float" do
    it "should consume simple floats" do
      rant(RantlyHelpers::FLOAT).each do |float|
        parser.float.should parse(float)
      end
    end

    it "should consume floats with exponents" do
      rant(RantlyHelpers::FLOAT_WITH_EXP).each do |float|
        parser.float.should parse(float)
      end
    end

    it "should consume floats prefixed with a +" do
      rant(RantlyHelpers::FLOAT).each do |float|
        parser.float.should parse("+#{float.to_f.abs.to_s}")
      end
    end
  end

  context "symbol" do
    it "should consume any symbols" do
      rant(RantlyHelpers::SYMBOL).each do |symbol|
        parser.symbol.should parse("#{symbol}")
      end
    end

    context "special cases" do
      it "should consume '/'" do
        parser.symbol.should parse('/')
      end

      it "should consume '.'" do
        parser.symbol.should parse('.')
      end

      it "should consume '-'" do
        parser.symbol.should parse('-')
      end
    end
  end

  context "keyword" do
    it "should consume any keywords" do
      rant(RantlyHelpers::SYMBOL).each do |symbol|
        parser.keyword.should parse(":#{symbol}")
      end
    end
  end

  context "tag" do
    it "should consume any tags" do
      rant(RantlyHelpers::TAG).each do |tag|
        parser.tag.should parse(tag)
      end
    end
  end

  context "string" do
    it "should consume any string" do
      rant(RantlyHelpers::STRING).each do |string|
        parser.string.should parse(string)
      end
    end
  end

  context "character" do
    it "should consume any character" do
      rant(RantlyHelpers::CHARACTER).each do |char|
        parser.character.should parse(char)
      end
    end
  end

  context "vector" do
    it "should consume an empty vector" do
      parser.vector.should parse('[]')
      parser.vector.should parse('[  ]')
    end

    it "should consume vectors of mixed elements" do
      rant(RantlyHelpers::VECTOR).each do |vector|
        parser.vector.should parse(vector)
      end
    end
  end

  context "list" do
    it "should consume an empty list" do
      parser.list.should parse('()')
      parser.list.should parse('( )')
    end

    it "should consume lists of mixed elements" do
      rant(RantlyHelpers::LIST).each do |list|
        parser.list.should parse(list)
      end
    end
  end

  context "set" do
    it "should consume an empty set" do
      parser.set.parse_with_debug('#{}')
      parser.set.should parse('#{}')
      parser.set.should parse('#{ }')
    end

    it "should consume sets of mixed elements" do
      rant(RantlyHelpers::SET).each do |set|
        parser.set.should parse(set)
      end
    end
  end

  context "map" do
    it "should consume an empty map" do
      parser.map.should parse('{}')
      parser.map.should parse('{ }')
    end

    it "should consume maps of mixed elements" do
      rant(RantlyHelpers::MAP).each do |map|
        parser.map.should parse(map)
      end
    end
  end

  context "tagged element" do
    context "#inst" do
      it "should consume #inst" do
        rant(RantlyHelpers::INST).each do |element|
          parser.tagged_element.should parse(element)
        end
      end
    end

    it "should consume tagged elements" do
      rant(RantlyHelpers::TAGGED_ELEMENT).each do |element|
        parser.tagged_element.should parse(element)
      end
    end
  end
end
