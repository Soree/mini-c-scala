import scala.util.parsing.combinator.RegexParsers

// lisp-like tiny language
// (- (+ 3 2) (div 8 2)) => 4

object TinyLisp {
  def main(args: Array[String]): Unit = {
    val parser = new TinyLisp
    println(parser.parse("(+ (* 3 (+ 4 1)) 2)"))
  }
}

class TinyLisp extends RegexParsers {
  def expr : Parser[Any] = lclose ~ op ~ expr ~ expr ~ rclose | intLit
  def lclose = "("
  def rclose = ")"
  def intLit = """[1-9][0-9]*|0""".r
  def op = "+" | "-" | "*" | "div" 
  def parse(str: String) = parseAll(expr, str)
}
