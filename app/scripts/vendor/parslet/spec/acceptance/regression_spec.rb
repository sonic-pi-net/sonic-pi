# Encoding: UTF-8

require 'spec_helper'

require 'parslet'

describe "Regressions from real examples" do
  # This parser piece produces on the left a subtree that is keyed (a hash)
  # and on the right a subtree that is a repetition of such subtrees. I've
  # for now decided that these would merge into the repetition such that the
  # return value is an array. This avoids maybe loosing keys/values in a 
  # hash merge. 
  #
  class ArgumentListParser
    include Parslet

    rule :argument_list do
      expression.as(:argument) >> 
        (comma >> expression.as(:argument)).repeat
    end
    rule :expression do
      string
    end
    rule :string do
      str('"') >> 
      (
        str('\\') >> any |
        str('"').absent? >> any
      ).repeat.as(:string) >>
      str('"') >> space?
    end
    rule :comma do
      str(',') >> space?
    end
    rule :space? do
      space.maybe
    end
    rule :space do
      match("[ \t]").repeat(1)
    end
    
    def parse(str)
      argument_list.parse(str)
    end
  end
  describe ArgumentListParser do
    let(:instance) { ArgumentListParser.new }
    it "should have method expression" do
      instance.should respond_to(:expression)
    end 
    it 'should parse "arg1", "arg2"' do
      result = ArgumentListParser.new.parse('"arg1", "arg2"')
      
      result.should have(2).elements
      result.each do |r|
        r[:argument]
      end
    end
    it 'should parse "arg1", "arg2", "arg3"' do
      result = ArgumentListParser.new.parse('"arg1", "arg2", "arg3"')
      
      result.should have(3).elements
      result.each do |r|
        r[:argument]
      end
    end
  end

  class ParensParser < Parslet::Parser
    rule(:balanced) {
      str('(').as(:l) >> balanced.maybe.as(:m) >> str(')').as(:r)
    }
  
    root(:balanced)
  end
  describe ParensParser do
    let(:instance) { ParensParser.new }
    
    context "statefulness: trying several expressions in sequence" do
      it "should not be stateful" do
        # NOTE: Since you've come here to read this, I'll explain why
        # this is broken and not fixed: You're looking at the tuning branch, 
        # which rewrites a bunch of stuff - so I have failing tests to 
        # remind me of what is left to be done. And to remind you not to 
        # trust this code. 
        instance.parse('(())')
        lambda {
          instance.parse('((()))')
          instance.parse('(((())))')
        }.should_not raise_error
      end 
    end
    context "expression '(())'" do
      let(:result) { instance.parse('(())') }

      it "should yield a doubly nested hash" do
        result.should be_a(Hash)
        result.should have_key(:m)
        result[:m].should be_a(Hash)   # This was an array earlier
      end 
      context "inner hash" do
        let(:inner) { result[:m] }
        
        it "should have nil as :m" do
          inner[:m].should be_nil
        end 
      end
    end
  end

  class ALanguage < Parslet::Parser
    root(:expressions)

    rule(:expressions) { (line >> eol).repeat(1) | line }
    rule(:line) { space? >> an_expression.as(:exp).repeat }
    rule(:an_expression) { str('a').as(:a) >> space? }

    rule(:eol) { space? >> match["\n\r"].repeat(1) >> space? }

    rule(:space?) { space.repeat }
    rule(:space) { multiline_comment.as(:multi) | line_comment.as(:line) | str(' ') }

    rule(:line_comment) { str('//') >> (match["\n\r"].absent? >> any).repeat }
    rule(:multiline_comment) { str('/*') >> (str('*/').absent? >> any).repeat >> str('*/') }
  end
  describe ALanguage do
    def remove_indent(s)
      s.to_s.lines.map { |l| l.chomp.strip }.join("\n")
    end
    
    it "should count lines correctly" do
      cause = catch_failed_parse {
        subject.parse('a
          a a a 
          aaa // ff
          /* 
          a
          */
          b
        ')
      }

      remove_indent(cause.ascii_tree).should == remove_indent(%q(
      Expected one of [(LINE EOL){1, }, LINE] at line 1 char 1.
      |- Extra input after last repetition at line 7 char 11.
      |  `- Failed to match sequence (LINE EOL) at line 7 char 11.
      |     `- Failed to match sequence (SPACE? [\n\r]{1, } SPACE?) at line 7 char 11.
      |        `- Expected at least 1 of [\n\r] at line 7 char 11.
      |           `- Failed to match [\n\r] at line 7 char 11.
      `- Don't know what to do with "\n         " at line 1 char 2.).strip)
    end 
  end

  class BLanguage < Parslet::Parser
    root :expression
    rule(:expression) { b.as(:one) >> b.as(:two) }
    rule(:b) { str('b') }
  end
  describe BLanguage do
    it "should parse 'bb'" do
      subject.should parse('bb').as(:one => 'b', :two => 'b')
    end 
    it "should transform with binding constraint" do
      transform = Parslet::Transform.new do |t|
        t.rule(:one => simple(:b), :two => simple(:b)) { :ok }
      end
      transform.apply(subject.parse('bb')).should == :ok
    end 
  end

  class UnicodeLanguage < Parslet::Parser
    root :gobble
    rule(:gobble) { any.repeat }
  end
  describe UnicodeLanguage do
    it "should parse UTF-8 strings" do
      subject.should parse('éèäöü').as('éèäöü')
      subject.should parse('RubyKaigi2009のテーマは、「変わる／変える」です。 前回の').as('RubyKaigi2009のテーマは、「変わる／変える」です。 前回の')
    end 
  end
  
  class UnicodeSentenceLanguage < Parslet::Parser
    rule(:sentence) { (match('[^。]').repeat(1) >> str("。")).as(:sentence) }
    rule(:sentences) { sentence.repeat }
    root(:sentences)
  end
  describe UnicodeSentenceLanguage do
    let(:string) {
      "RubyKaigi2009のテーマは、「変わる／変える」です。 前回の" +
      "RubyKaigi2008のテーマであった「多様性」の言葉の通り、 " +
      "2008年はRubyそのものに関しても、またRubyの活躍する舞台に関しても、 " +
      "ますます多様化が進みつつあります。RubyKaigi2008は、そのような " +
      "Rubyの生態系をあらためて認識する場となりました。 しかし、" +
      "こうした多様化が進む中、異なる者同士が単純に距離を 置いたままでは、" +
      "その違いを認識したところであまり意味がありません。 異なる実装、" +
      "異なる思想、異なる背景といった、様々な多様性を理解しつつ、 " +
      "すり合わせるべきものをすり合わせ、変えていくべきところを " +
      "変えていくことが、豊かな未来へとつながる道に違いありません。"
    }
    
    it "should parse sentences" do
      subject.should parse(string)
    end 
  end

  class TwoCharLanguage < Parslet::Parser
    root :twochar
    rule(:twochar) { any >> str('2') }
  end
  describe TwoCharLanguage do
    def di(s)
      s.strip.to_s.lines.map { |l| l.chomp.strip }.join("\n")
    end

    it "should raise an error" do
      error = catch_failed_parse {
        subject.parse('123') }
      di(error.ascii_tree).should == di(%q(
        Failed to match sequence (. '2') at line 1 char 2.
        `- Don't know what to do with "3" at line 1 char 3.
      ))
    end 
  end

  # Issue #68: Extra input reporting, written by jmettraux
  class RepetitionParser < Parslet::Parser
    rule(:nl)      { match('[\s]').repeat(1) }
    rule(:nl?)     { nl.maybe }
    rule(:sp)      { str(' ').repeat(1) }
    rule(:sp?)     { str(' ').repeat(0) }
    rule(:line)    { sp >> str('line') }
    rule(:body)    { ((line | block) >> nl).repeat(0) }
    rule(:block)   { sp? >> str('begin') >> sp >> match('[a-z]') >> nl >>
                     body >> sp? >> str('end') }
    rule(:blocks)  { nl? >> block >> (nl >> block).repeat(0) >> nl? }

    root(:blocks)
  end
  describe RepetitionParser do
    def di(s)
      s.strip.to_s.lines.map { |l| l.chomp.strip }.join("\n")
    end

    it 'parses a block' do
      subject.parse(%q{
        begin a
        end
      })
    end
    it 'parses nested blocks' do
      subject.parse(%q{
        begin a
          begin b
          end
        end
      })
    end
    it 'parses successive blocks' do
      subject.parse(%q{
        begin a
        end
        begin b
        end
      })
    end
    it 'fails gracefully on a missing end' do
      error = catch_failed_parse {
        subject.parse(%q{
          begin a
            begin b
          end
        }) }
      
      di(error.ascii_tree).should == di(%q(
        Failed to match sequence (NL? BLOCK (NL BLOCK){0, } NL?) at line 2 char 11.
        `- Failed to match sequence (SP? 'begin' SP [a-z] NL BODY SP? 'end') at line 5 char 9.
           `- Premature end of input at line 5 char 9.
        ))
    end
    it 'fails gracefully on a missing end (2)' do
      error = catch_failed_parse {
        subject.parse(%q{
          begin a
          end
          begin b
            begin c
          end
        }) }

      di(error.ascii_tree).should == di(%q(
        Failed to match sequence (NL? BLOCK (NL BLOCK){0, } NL?) at line 3 char 14.
        `- Don't know what to do with "begin b\n  " at line 4 char 11.
        ))
    end
    it 'fails gracefully on a missing end (deepest reporter)' do
      error = catch_failed_parse {
        subject.parse(%q{
            begin a
            end
            begin b
              begin c
                li
              end
            end
          },
          :reporter => Parslet::ErrorReporter::Deepest.new) }

      di(error.ascii_tree).should == di(%q(
        Failed to match sequence (NL? BLOCK (NL BLOCK){0, } NL?) at line 3 char 16.
        `- Expected "end", but got "li\n" at line 6 char 17.
        ))
    end
  end
end
