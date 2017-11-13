# encoding: UTF-8

# A small example contributed by John Mettraux (jmettraux) that demonstrates
# working with Unicode. This only works on Ruby 1.9.

$:.unshift File.dirname(__FILE__) + "/../lib"

require 'parslet'

class Parser < Parslet::Parser
  rule(:sentence) { (match('[^。]').repeat(1) >> str("。")).as(:sentence) }
  rule(:sentences) { sentence.repeat }
  root(:sentences)
end

class Transformer < Parslet::Transform
  rule(:sentence => simple(:sen)) { sen.to_s }
end

string =
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

parser = Parser.new
transformer = Transformer.new

tree = parser.parse(string)
p transformer.apply(tree)
