import scala.util.parsing.combinator.RegexParsers

// lisp-like tiny language
// (- (+ 3 2) (div 8 2)) => 4

object TinyLisp {
  def main(args: Array[String]): Unit = {
    val parser = new TinyLisp
    println(parser.parse("hello"))
  }
}

class TinyLisp extends RegexParsers {
  def expr = "hello"
  def parse(str: String) = parseAll(expr, str)
}
