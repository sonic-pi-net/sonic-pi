package whatever.mine

import foo.bar.{Foo, Bar => Baz}

/* This file /* which is totally legal scala */ should be highlighted
   correcty by rouge */

object ⌘ {
  val `interface` = """
A
"Multiline"
String
"""

  val s = 'symbol

  @tailrec
  val foo_+ = "foo plus"
  val foo_⌬⌬ = "double benzene"

  def main(argv: Array[String]) {
    println(⌘.interface + " " + foo_+ + " " + foo_⌬⌬ )
  }

  def noWhitespaceType[Foo](arg:ClassName[Foo])(implicit evidence: Foo =:= Int) {}

  val char = 'a'
  var unicodeChar = '\uAB23'

  val constant = true

  import more.stuff._
}

abstract case class Foo[+A, B <: List[A]](a: A) {
  type Bar = Baz

  def apply[C >: A](c: Foo[C]): Foo[C]
}

class why_would_you_name_a_class_this_way_oh_well_we_need_to_highlight_it(a: Int) extends Foo(a) {
  def this(b: Float) = this(b.toInt)

  def ints = 4
  def floats = 4f
  def doubles = 0.4
  def longs = 4L
  def hex = 0x123
}
