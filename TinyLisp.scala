import scala.collection.mutable._
import scala.util.parsing.combinator.RegexParsers

// lisp-like tiny language
// easy step to mini-c compiler

object TinyLisp {
  def main(args: Array[String]): Unit = {

    val N = 4
    val es = new ListBuffer[String]()
    es += "(+ 1 2)"
    es += "(+ 1 (- 3))"
    es += "(+ (* 3 (+ 4 1)) 2)"
    es += "(+ (* 3 (/ 4 1)) (- 2))"

    for(i <- 0 until N){
      println( es(i) )
    }

    val parser = new TinyLispParser
    val ss = new ListBuffer[AST]()
    for(i <- 0 until N){
      ss += parser.parse( es(i) ).get 
      println( ss(i) )
    }

    val evaluator = new TinyLispEval
    val rs = new ListBuffer[Int]()
    for(i <- 0 until N){
      rs += evaluator.eval( ss(i) )
      println( rs(i) )
    }
  }
}

trait AST
case class Num(v: Int) extends AST
case class Add(left: AST, right: AST) extends AST
case class Minus(left: AST, right: AST) extends AST
case class Mul(left: AST, right: AST) extends AST
case class Div(left: AST, right: AST) extends AST

class TinyLispParser extends RegexParsers {

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

class TinyLispEval {
  def eval(ast: AST): Int = {
    ast match {
      case Num(v) => {
        v
      }
      case Add(left, right) => {
        eval(left) + eval(right)
      }
      case Minus(left, right) => {
        eval(left) - eval(right)
      }
      case Mul(left, right) => {
        eval(left) * eval(right)
      }
      case Div(left, right) => {
        eval(left) / eval(right)
      }
    }
  }
}
