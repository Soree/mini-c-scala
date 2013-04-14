import scala.util.parsing.combinator.RegexParsers

// lisp-like tiny language
// (- (+ 3 2) (div 8 2)) => 4

// current status
// [1.8] Add(Num(1),Num(2))
// [1.14] parsed: Add(Num(1),Minus(Num(3),Num(1)))
// [1.20] parsed: Add(Mul(Num(3),Add(Num(4),Num(1))),Num(2))
// [1.20] parsed: Add(Mul(Num(3),Div(Num(4),Num(1))),Num(2))

object TinyLisp {
  def main(args: Array[String]): Unit = {
    val parser = new TinyLisp
    println(parser.parse("(+ 1 2)"))
    println(parser.parse("(+ 1 (- 3 1))"))
    println(parser.parse("(+ (* 3 (+ 4 1)) 2)"))
    println(parser.parse("(+ (* 3 (/ 4 1)) 2)"))
  }
}

class TinyLisp extends RegexParsers {

  trait AST
  case class Num(v: Int) extends AST
  case class Add(left: AST, right: AST) extends AST
  case class Minus(left: AST, right: AST) extends AST
  case class Mul(left: AST, right: AST) extends AST
  case class Div(left: AST, right: AST) extends AST

  def expr : Parser[AST] = 
    lclose ~> op ~ expr ~ expr <~ rclose ^^ {
      case "+" ~ left ~ right => Add(left, right)
      case "-" ~ left ~ right => Minus(left, right)
      case "*" ~ left ~ right => Mul(left, right)
      case "/" ~ left ~ right => Div(left, right)
    } |
    intLit 
  def intLit = negNum | num
  def num = """[1-9][0-9]*|0""".r ^^ {
    case value => Num(value.toInt) 
  }
  def negNum = lclose ~ "-" ~> num <~ rclose ^^ {
    case Num(x) => Num( (-1) * x )
  }

  def lclose = "("
  def rclose = ")"
  def op = "+" | "-" | "*" | "/" 

  def parse(str: String) = parseAll(expr, str)
}
