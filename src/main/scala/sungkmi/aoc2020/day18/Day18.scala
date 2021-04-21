package sungkmi.aoc2020.day18

sealed trait Token

final case class Digit(n: BigInt) extends OpDigit

sealed trait OpParen extends Token

enum Op extends OpParen with OpDigit:
  case Plus, Multiply

enum Parentheses extends OpParen:
  case Open, Close

sealed trait OpDigit extends Token

def precBasic(opp: OpParen): Int = opp match
    case _: Parentheses => 0
    case _: Op => 1

def precAdvanced(opp: OpParen): Int = opp match
    case _: Parentheses => 0
    case Op.Multiply => 1
    case Op.Plus => 2

def toPostfix(s: String, prec: OpParen => Int): List[OpDigit] =
  def mergeDigitStackToOut(digitStack: List[Char], out: List[OpDigit]): List[OpDigit] =
    if digitStack.isEmpty then out else
      Digit(BigInt(digitStack.reverse.mkString)) :: out
  def filterOpAndReverse(input: List[OpParen]): List[Op] =
    input.foldLeft(List.empty[Op]):
      (acc, op) => op match
        case op: Op => op :: acc
        case p: Parentheses => acc
  val (out, digitStack, opStack) = 
    s.toList.foldLeft((List.empty[OpDigit], List.empty[Char], List.empty[OpParen])):
      case ((out, digitStack, opStack), char) if char.isDigit =>
        (out, char :: digitStack, opStack)
      case ((out, digitStack, opStack), char) =>
        //println(s"===> ($out, $digitStack, $opStack), '$char'")
        val out0 = mergeDigitStackToOut(digitStack, out)
        val (out1, opStack1) = char match
          case ' ' => (out0, opStack)
          case '(' => (out0, Parentheses.Open :: opStack)
          case ')' =>
            val (f, b) = opStack.span(_ != Parentheses.Open)
            (filterOpAndReverse(f) ::: out0, b.tail)
          case opChar if opChar =='+' || opChar == '*' =>
            val op: Op = opChar match
              case '+' => Op.Plus
              case '*' => Op.Multiply
            val (f, b) = opStack.span(prec(_) >= prec(op))
            (filterOpAndReverse(f) ::: out0, op :: b)
        (out1, Nil, opStack1)
  
  (filterOpAndReverse(opStack) ::: mergeDigitStackToOut(digitStack, out)).reverse

def evalPostFix(postFix: List[OpDigit]): BigInt =
  //println(s"===> Postfix: $postFix")
  postFix.foldLeft(List.empty[BigInt]):
    case (stack, Digit(n)) => n :: stack
    case (stack, Op.Plus) => stack match
      case x :: y :: ints => (x + y) :: ints
      case _ => throw Exception(s"Wrong stack: $stack")
    case (stack, Op.Multiply) => stack match
      case x :: y :: ints => (x * y) :: ints
      case _ => throw Exception(s"Wrong stack: $stack")
  .head

def evalBasic(line: String): BigInt =
  evalPostFix(toPostfix(line, precBasic))

def evalAdvanced(line: String): BigInt =
  evalPostFix(toPostfix(line, precAdvanced))

def solve1(input: String): BigInt =
  input.split("\n").map(evalBasic).sum

def solve2(input: String): BigInt = 
  input.split("\n").map(evalAdvanced).sum

@main def part1: Unit =
  val ans = solve1(input)
  println(ans)

@main def part2: Unit = 
  val ans = solve2(input)
  println(ans)

lazy val input: String = """9 * 7 + ((6 + 9 * 2 + 6 + 7 + 5) * (5 + 9 + 7) * 6) * (4 + 8) + 5 * 8
3 * (4 * 9 + 8 + 4 * 4 + 7) + 7 * (8 * 9)
4 * ((6 * 5 * 9) + 2) + 9 * 4
((4 * 6 * 2 * 9) + 7 * 4) + 4 * 6 + 5 * 6 + 6
9 * 8 * ((5 + 7 * 8 * 2) * 6) + 3
6 + (3 * 7) * 3 + (9 * (6 * 6 + 5 + 9 + 9) * 7 * 9 + (4 * 8) + 8)
6 * 4 + 2 + 8 + 7
(5 * (9 * 8) * 6) + 2 + 8 * 5 * 4 * 3
(3 + 2 * 2 * 8 * 5) + (7 + 6 * 4 * 8) * (6 * 9 * 5 * 9)
(4 * 2 + (9 + 2 * 4) * 5 + 9) + (9 * 3)
7 * 6 + 6
4 + (8 + (5 * 9) + 9 * 8)
9 + 8 + 5 + 4 * 2 * 9
8 * (5 + (8 + 2) * 9 + 4 + 7) + 8
2 * 2 + 9 * 6 + (2 * (8 * 2 + 3 * 8) * (9 + 7 * 4 * 8 * 2)) * 2
4 + (5 * 6 + 7 * 5) + 4
4 + (9 * 4) + 4 * (4 + 7 + 9 * 4) * 3
(4 * (2 + 2) * 9 * 8 * 3) + 9 * 3
6 * (5 * 3 + 4 + (4 + 4 + 5 * 7 * 4)) * 4
8 * (6 * 9 + 3 * (6 * 6 * 2) + 4 + 9) * (4 * 8 * (9 * 4 + 8 + 7 * 7) * 5) + 4 + 7
(7 * 3 * 2) + 3 + 8 * (5 + 7 * 3 + 4 * 2 * (8 + 6 * 4)) * 3 * (8 + 9 + 7 * 3 * 3)
8 * 7 + 4 + 7 * (3 * 3 + 7 + (3 * 5 * 8 + 8 + 3 * 4))
(7 * (4 * 3 + 8 * 7 * 3 + 9) * 4 + (4 * 7)) * 8 + ((2 * 9) + 6 * 7 + (9 + 3) * 5 + 2) * 8 + 6
6 + 8 * (7 * (5 + 3 + 8 * 5 + 2 * 7) * 4 + 2 + 7 + 7)
(6 * 3 * 7) * 7 * (7 * 7 * 9) + 8
((8 * 7 + 2 + 3) * 4) + 4 * 7 * 6 + 5
2 * 5 * 4 * 9 + ((2 * 4 * 4) + 4 * (5 + 4))
((3 + 5 * 5) + (5 * 9 + 5)) * 3
8 * (3 * (9 * 3 * 9) + 3 + 5) * 2
4 * (8 * 9 * 7 * 2 + 3) + ((9 * 8) * 9) * (4 * 5 + 6 * 8) * 3
(5 + 2 * 9 + 5 + 8) + 9 + 6 * 2
(5 * 9 + 9 + 3 + (5 + 9 + 4 + 3)) + 8 * 7
(7 + 7 * 2 + 8) * (2 * 4 + 3 * 4) + 5 * 4
3 + (4 + 6) + 8 * 5
(6 + 5 * (8 + 2 + 6) * (2 + 9)) * ((3 + 2 * 8 + 4 * 9 * 3) * 6 * 9)
9 + (5 * (2 + 2 * 2) + 5 * 8) * 7 + 3 + 9
2 + 8 * ((2 + 2) * 2 + 4 * 3 + 3) * 5
(2 + 5 * 3 * (4 * 3 + 8 + 6) + 6) + ((7 + 9) + 4 * 3) + 6 * 9
(8 + 7 * (8 + 3 * 7 + 4 + 7) + (5 + 6)) + 4 * (9 * 2 + (8 + 7 * 4 * 6 + 3) + 7 + 2) + (3 * (2 + 2 * 3))
(3 * 6 * 4 * 8 + 4) + 6 + 3 * 3 + 4
8 + ((6 * 2) + 6 * (8 + 5 * 2 * 2 * 9 + 8)) * 5 * 7
7 + 3 + 5 + ((3 + 8 + 2) * 5 * 9 * 4 * 9 + 5)
4 * 5 * (8 + 6)
9 * 7 * 4 + (4 + 5) * 4
9 + 6 + (8 * 5 * (3 + 2 * 7 + 2 + 2 + 9) + 8 * (8 * 5 + 5 * 5 * 3 * 7) * (5 * 3 + 8 + 7 * 4)) + 7 + 2 * 5
6 + ((2 * 3 + 9 * 8 * 8) + 6 * (5 * 5)) * 2 + 3 * (2 + (7 * 8 * 9 * 4 + 7 * 3) * 2 * 7 * (7 + 3 + 2 * 7 + 9 + 2)) + (3 * 9 + 2 * 8 * 4 * 2)
(7 * 4) * 2 + 7 + 2 + 3
(5 + 6 + 8) + 8 + 3 + (5 + 8 * 5 + 5 + 8) + 3
2 * (6 * (3 + 5)) + 4 + (3 + 9 * 5 + 8 + (6 + 4 + 2 + 2 * 2 * 4)) * 3 + 8
(2 + 7 + 3) + 7 + 2 + 6 * (6 * (4 + 7 * 8 * 5)) * 4
7 * 5 + 3 + 3
4 * (2 + 3 + 5) * 2 * 3
(8 * 8 + (7 * 4 * 6 * 6 + 2 * 6) + (8 + 6 + 2) * 7 * 8) + (3 * (8 + 9 + 6) + 4 * 7) + 5 * 8
(7 * 8 * 2 + 3 * 9) * ((4 * 4 + 5) + (6 + 4 + 9 + 5) + 3 * 2 * (2 + 5 * 7)) + 4 + 8 * ((8 * 8 + 5 * 4 * 3 + 4) * 5 + 4 * 4 + 4 + 7)
(9 + 8 + (4 + 2 * 9) + 7 + 7) + 7 * 8
(3 * 5 + (7 + 5 * 2 + 2)) * 4
(5 + 8) * 6 + (6 + (7 + 4 + 3 + 2 + 6 + 9))
4 * 2 + 2 + 9 + 3 + (2 * 6 * (2 * 2 * 8) + 6 * (3 + 2 * 3 * 4 + 9) * 7)
(7 + (9 + 8 + 6 * 6 * 2)) + 6 * (8 * 9 * (5 * 7 + 6 + 3)) + 4
(3 * (9 + 2) + 6 + 6) + 4 * 4 * 4 * 7
4 * 4 + 6 * 3 * 2 * 7
3 + (7 + 3 + (4 + 4 * 3 + 5 + 4) * 9 + (2 + 5 + 7 * 7 * 2) * 4)
5 * (3 * 6 * (7 + 2 * 2) + 7 + 7) + (2 + 7 + 3 * 7 * 6) + 3 * 4 + 4
2 * 4 * 2 * (8 * (7 * 7) * 7 * (2 * 8 + 7)) + 5
4 + 6 + (8 * 9 + 4 + 8 + 4) * 3
5 * 9 * 9 + 4 + 2 + (3 + (5 + 9))
(9 + 8 + (6 * 5 + 7 * 5 * 4) + (6 + 7 * 2 + 8 + 6)) * 4 + 8 * 6 + 8
7 * 2 + ((7 + 3 * 7 * 7 * 2) * (9 * 9 * 8 + 7) * (7 + 9 * 4 * 2 + 3) + 6) * 6
8 * 4 + 9 + 7 * (6 + 9)
6 + 3 + 3 * 7 + ((8 + 9) * 4) * 6
7 + 6 * (2 * (6 + 7) * (8 * 6)) + 5 + 7
7 * (5 + 9 * 6 * 5) * 7 + 6 * 2 * 2
7 * 3 * (2 + (9 + 3 * 6) + 8 * (9 * 2 * 2 + 2) * 3 * 5)
6 + 5 * 7 * 9 * (8 * 2 * 6 * 5 + 9 * 6) * 2
5 * 3 + (8 * (9 + 3 * 2 + 9 * 4) * 2 * 7) + 2 * (2 + 8 * 6 + 5 + 7 * 3)
9 * 4 * (6 + (2 + 6 + 4 + 2 * 5) * 8 + 2 + 8) + 4 * 3 * (9 + 8)
3 * 6 + 9 + 6 * 3 + (7 + 7 * 2)
7 * (8 + (3 + 9 * 3) * 3 + 2 + 9 * 5) + (3 + (3 + 3 + 8 * 9))
2 * 2 * ((3 * 6) + 7 + 2 * (9 + 8 * 3 * 6) + 5) * (4 + (2 * 2 + 8 + 8 + 6 + 4) + (7 * 2 * 2 + 9 * 8 + 8) + 9 + 3)
(7 + 2 * 8 * 3) + 9
8 * 8 + (9 + 2 + 3 + 2)
(3 + 2 * 4 * 6 * 3) + 7
8 * 4 * 3 + (7 + 5 + 6 * 5 * 6) + 9
(2 * 3) + (8 * 2 * (6 * 3) * 5) * ((7 * 9 + 4 * 4) + 2 * (4 * 9 + 3) + 9 * (5 * 5) * 4)
2 + 6 * (3 + 5 * 7) * 2
(6 + (3 + 4) * 6 * 8) + 8
8 * 5 * 2 + (7 * (5 + 3 + 4 + 4) * 4 + 9)
5 * 9 * (5 + 8 + 9 * (2 + 7 * 2 * 6)) * 9
(5 + 9 * (6 * 7 + 9 + 6 * 8) * 8) * (3 + 7 + 2) * 2 + (9 + 6 * 4 * (9 * 3 * 4) * 5 * (7 * 4 + 3)) * 8
7 * 3 * 7 * 3 * ((3 + 2 * 9) + 6 * (3 * 2) + 4)
7 + 4 * (7 + 9 * 8) * 7 * 5 + 7
(7 + 9 * (2 + 4 * 9 * 4 * 7 + 4) * 3) * (8 * (2 * 4 + 6 * 2) * 6)
((3 * 6 + 4 * 6 * 4) * 6 * 8 * (9 * 4 + 2 + 5 + 3 + 9) * 5) * 6 + (9 + 8)
(3 * 3 + 4) * 4
(4 * (7 * 6 + 4 * 4 * 5) * 3 + 3) + 6 * 6 * (2 * 3 * (8 * 3 + 2 * 5 * 6 + 7) + 9 + 5) * 4 * 7
(2 * (5 * 6 + 9) * 7) + 3
5 + 9 + 2 * ((6 * 6) * (3 + 7 + 6 + 6) + 3 + 5)
4 * 2 * 9 + (3 + 4 + 5 * (4 + 8 * 5) * 9 + 5)
7 + 4 * 6 + 9 * 7 + 3
(8 * 7) + 7 * 3 * (7 * 9 * (4 + 4 + 7 * 5 + 7 * 2) + 9 * (5 * 5 * 8 + 7) * 3) + ((9 * 8 + 8) + 2 * 2) + 6
2 + 7 * 4 * 7 * (4 * 3 + 6 + 8 * 6) + 7
6 + (9 + 8 * (5 * 7) * 4) * (5 * 8 * 2 * (8 * 6 + 6) * 5 + 3) + 4 * 7
6 + 5 + (7 + (8 + 2) * 4 * 3 * 2 * 9) * 7
7 + 3 * 9
(5 * 5 + 8 * (9 * 4 * 3 + 6 + 8 * 2)) * (2 * 6 + 8) + 6 * 7
(2 + 8) * 9 + 4 + 4 * 2 + (3 * 3 + 9 + 2 + 9 + 5)
((5 * 4) + (3 * 4 + 3 + 4) + (8 + 3 + 3 + 9 + 2) + 4) * 9 * 7 * 9 + 6
9 + 3 * 4 * (9 + 6 * (9 + 8) + (8 * 4) * 5 + 2)
(4 + (3 + 2 * 4 + 7) + 4 + 2) * 4 * 7 * 3 * (2 * 8 * 9 * 5 + (3 * 3 + 8) * 3)
2 * 7 * (2 * 5 + 5 * (2 + 7 + 4)) * 7 + 2 + 4
4 * 2 * (6 * 6 + 2 + 2 + 6 * (2 * 2 * 8)) + 3 * 9
(5 * 9) + 2 + (3 * 8 * (8 + 4 * 3 + 7 * 9 * 5) * 2 * 6)
2 * 9
6 * 9 * 2 + 9 * (4 + 3) * (8 + 6 * 9 * 3 + 7 * (2 * 2 + 7 + 8 + 3 * 7))
3 * 8 + 9 + 4
9 + (4 * 9 + (6 * 9 + 6) + 2 * 6 * 9) * (2 * 4 * (3 * 5 * 2 + 2 + 8 * 6)) * 6
(3 * (3 * 6 + 8 + 3 + 7) * 9 * (8 * 3 * 4) + 6 + 3) + 8 * (4 + 9 + 3 + 2 + (4 + 4 * 6 + 2))
6 + 5 * 2 + 7 + ((9 + 2 + 4) * 6 + 5 * 8) * ((6 + 5 + 6 + 2) + (6 * 3 + 7) + 7 * 2 + (7 * 7 + 3 * 4 * 3 + 2))
((2 + 3 * 6 * 5) * 7 * 9 * 5 * 2 * 8) + 8 + 3 * 3 * 6
7 * (8 * 6) * 4 + 2 * 7 * 8
2 + 4 + (9 * 5 + 3)
5 + ((7 * 8) + 8 + 6 + 6 * 2 * 3) * 2 * 3
9 + (4 * 5 * (6 * 6 * 9 * 5 * 6 * 8)) * (8 + 5 + 3 * 7) + 9 * 3 * 4
8 + 4 * 5 * 2
5 + 2 + 8 + 4 * (9 * 2 * 6) * 3
4 * 6 * (5 * 2 + 3 + 5 + (8 + 5 * 7 * 4) + 4) * 8 + 2
(9 * 7 + 2 + (3 * 5 * 4 + 6 + 3 * 2) * (4 * 8)) * 3 + ((9 * 2 + 6) * 6 + 5 + 5 + 5) * 8
(8 + (2 + 2)) * 7 + (9 * 9 + 3) + ((5 * 4 * 9) + (2 * 6 + 9)) + 3 * 3
6 * 8 + 8 + 2 + 7 + (5 + 3)
2 + 2 * (4 + 5) * 6 + 4 * (6 * 8 + 4 + 2 * (9 + 5) * 3)
(5 * 5 + 6) + 8 + 3 * (2 * 3 * 3 + 6 * 7 * (6 + 7 + 8 * 6 * 4))
7 * 8 + 2 + 5 * (9 + 6 + (5 + 5 * 6 * 3 + 7) * 2 * 5 + (7 + 8))
6 * (9 + 5 * 3) * 7 * (9 * 5) + 3 + 4
(9 + (3 + 2 * 2) * 6) * 2 + 5 * 6 + ((9 * 2 + 4 + 5 * 7) * 2 * 7 + 5)
6 + (5 + 7 + 5 + 5 + 9 + 5) * (8 * 4 * 8 + 4) + 4 * 3 * 9
2 * 9 * 9 * (4 + 2) * 7 + (2 * (4 * 7 + 3) + 8 * 9 * 3 + 9)
5 * 5 * (6 * (8 * 2) + 8 * 8)
(4 * (5 * 9 * 2 * 8 * 5) * 8 + (6 + 3)) + (8 * 9 + 4 * (5 * 5 + 6)) * 4 + 6 * 4
9 * 6 + 6 + (7 * (3 + 3 * 9 + 7 * 7 + 5) + 3)
5 * ((5 * 3 + 5 + 7 + 2 + 3) + 8) * 6 + 9 * 7
(4 * 9 + 4 + 2 * (8 + 9)) * 6 * 4 + 9 + 5
5 * 4 + 3 + 2
(5 * (9 + 7 + 3 + 4) * (8 + 3 + 9 + 9 + 9) + 5 + 9) * (3 * 3) * 7 + (2 * 6) + 8
(7 * 6 + 7) * 7 + 9
((6 + 4 * 9 * 7 * 8) * 3 + 3 * 5 + 7) * 5 * 9 + (5 * 2 + (2 * 7 + 8 + 3)) + 5
(3 + 8 * 4 * 3 * 5 + 9) + ((8 + 3 + 9 + 3) * 8 * 5 * 5 * 3) * 4 + 9 * 6
4 * 3 + 2 * (9 * 6 * 3 + (9 + 3 + 4 + 8 * 4 * 4) + 3 + 6) * ((2 * 9) + 7) + 5
(6 * 9) * 2
8 * (5 * (7 * 4 * 5 * 3 + 4))
2 * 9 + 7 + (8 + 5 * 8 * 8)
8 + 5 + (2 + 3 * (7 * 8 + 3)) * 4 + 8 + 6
(6 * 4 + (3 * 6 + 7) + 9 + 5) + 4 + 4 + (5 * (9 * 7)) + 7 * 6
6 * (7 * 9 + 3 * 3 + 4) * 4 * 4 + 7 * 6
(2 * 5 + (5 + 5 * 9 * 3 * 4 * 9) * 7) * 8 + 9 * (6 + (9 * 4 + 3 + 2 + 9 * 3) * 4)
(6 + 6 * 6 + 6) + 3 + 6
3 + 5 * 6 + 8 * ((5 * 5 + 8 * 4) * 9 + 7 + 6)
8 * 9 + 5
4 * 3 + (8 * 2 + (3 + 5 * 4 + 9))
(4 + 9) * 3 * 3 + (7 * (6 + 5 + 7 + 9) * 4 * 2 * 2 + (9 * 4 + 7 + 4 + 7 * 3))
(2 + 8 * 6 + 2 + 8 * 6) * 2 + 7 * 5 + 3
8 + 3 + 4 + 7 * (5 + 8 + 3 * 8 * 2 * 3)
(2 * 3 + (2 * 6 * 2 + 2 + 8 + 2)) * 9 + (2 + 9) + 4 + 3 + 9
9 + 4 * 4 * ((4 * 6 + 2 + 8 * 7) * (3 + 8 + 6 + 3) * 3 * 3) + 4 + 6
(7 + 8 * 9 + 5) + (4 * 5 * 4 * 9 * 5) * 9
7 * (2 * 5 + 9) * 6 * 3 * 3
(9 * (4 * 9 * 9)) + (5 + (7 * 9 * 2 * 7 + 4) + (3 * 8 * 2 * 2 * 6 * 4) * 6 + (3 + 3 * 2 + 2)) + 2 + (5 * (2 * 5 + 3 * 3 * 4 * 7) * (8 * 4 + 7 + 8 * 5 * 2) * 3)
9 + ((2 * 6) * 3) + (9 * 2 * 8 * (7 + 3 * 5 * 3 * 7 * 8) * 7) + 4 * 4
6 + 9 * (2 + 7 + (7 + 5 + 2) * 5)
(7 * 2 * 4 + 8 * 2) + 8 * 4 + (7 * 3 * 2 * (5 + 8 + 6) * 3) * 6
5 + 9 + (4 * 4 * 3 * 5 + 9)
6 * 2 * 3
((6 + 5) + (8 * 7 + 3 + 6) + 5 + (3 + 2 + 9) + 9) * 6
7 * 9 + 2 + 7 + (8 * 9 * 9 + 2)
(8 + (7 * 4 * 2 + 5) * 8) * 3 + 8 + 2 + 9
((2 * 6 * 3) * (9 * 3) + (3 + 4 + 9) * 2 + (3 * 9 + 9 * 9 * 6 + 3) * (4 * 6 * 5 + 8 + 2)) * ((5 + 4 * 8) * 2 * 5) * 4
4 + (3 + 9 + 2 * 5 * 9) + 8 + (8 * 4 * (8 + 2 + 7 + 7 + 8 * 9) * 6 + 8 + 8) * 5
(4 * (7 + 5 + 8 * 4 * 4)) + 8 * 9 + 9
(7 + 2 * (7 * 4) * (7 * 4 + 6) + 5) + 3
9 * 4 * 5 + 3 + 9 + 5
4 * (5 + (4 * 8 + 4 * 5) * 6) + 4
6 + (7 + (7 * 8 * 3 * 3 * 9 * 3) + (7 + 9 * 7 * 9 + 3 * 6) * 4 + 3 + 4) + 6 + ((4 + 3 * 4 * 7 + 7) * 6 + 5) * 9
9 * 9 * 7 * (8 * (6 * 6 + 2 * 8 * 8 + 8) * 4 * 7 + 4)
(2 + 5 * 6 * (3 + 9)) * (3 + (7 + 9)) * 3 * 2
8 + 8 + 5 * (4 * (3 * 3) + 9 + 6 + 7 + 6) + ((5 * 4 + 7 + 5) + (6 * 9 + 9) + 5 * (9 + 9 + 2 * 4 + 6 * 4) + (4 + 5 + 7 + 8 + 6)) + 6
8 + (4 * (3 + 8) * 7) + 6
6 + 9
4 * (8 + 4 * 5 * 6) * 6 + 2 * 3
6 * 7 * 8 + 9 * 7
((9 * 5 * 2) + 9 * (7 + 6) + 4) + 5 + 3 + 3
9 + 4 * (5 + (8 * 3 + 5 * 9 + 9 * 5) * 2) + (3 + (5 + 4 * 3) * (5 + 2) + (7 * 9)) * 3
2 + (9 + 9 + 5 + 6 + (2 + 9 * 9) * 6) * (9 + 4 + 3 * 7 * 7 + 4)
3 + (8 * 9 * 6 * 2 + (2 + 9 * 3 + 9) * 5) + 7 * 6 + (8 * 5 + (8 + 3))
(6 * (3 + 5) * 2) + 6 * (4 * 7 * 2 + 2 + 8 * 3) + 8 + 4 * 9
3 * 9 * ((9 + 3 + 6 * 7 + 8) * 2 * (9 * 6 * 6 + 5 + 9 * 2)) * 6 * 2 + 3
9 * (6 + 8 + 6 * 6 * 2) + 5 * 3
(3 + 8 + 7 * 8 + 6 + (3 + 2)) * 4 + 2 + 4
((7 + 7 * 4 + 5) * 9 * 9 * 8) + 9 * 8 + 7
3 + ((4 * 9 + 4 * 5 * 2 * 7) + 7 + 2 + 8 * 2) + 5 * 6 + 8 + 5
(4 * 6 + 4 + 5) + 7 * ((6 + 9 * 2 + 8 * 3 * 4) * 9 * 9 + 4) + 3 + 3
6 + (9 * 7 + 2 * (7 + 6 + 6 + 4 * 8 * 9) + 3 * 3) * 2 + 8 * (4 * 9)
9 + 4 + 6 + ((6 + 8 * 2 * 7 * 2) * 8) + 8
(6 + 5) + 2 + 4
(4 * (2 + 5 * 9) + 4) + 4 + 2 + 3 * (5 + 8 + (9 + 9 + 4)) * 6
(7 + 8 * (9 + 8 + 8)) + 5 * 7 + 9
5 + 6 + (9 * 6 * (3 * 4 * 5 * 2) + (5 * 4 + 4 * 2 + 6 + 5)) + 9
((2 + 4) * 5) * 5 * 3 + 3 + 2
8 + (8 + 9 + (9 * 5 * 4)) * 8 * (6 + 8 * 2 + 6 * (3 + 6 * 9 + 2)) + 8 * (5 + (8 + 3 + 6 * 3))
6 * (2 * 7 * 5) + 8
4 * 5 + 8 * (7 + 4)
(7 + 4 + 8 + 4 * 4) * 8 * 5 + ((7 * 6 * 4) + 5 * 6)
7 * ((7 * 3 + 6 * 6 * 2 * 3) * 4 + 8 * 5 + 5) + (9 * 3 + 2 + 2 + 3 + 4) + (7 + 2 + (8 * 8 + 6 + 7 + 7)) * ((5 + 6 * 4 * 3 + 7 + 5) + 2 * 5 + 4 + 5) * 4
4 * (9 * 7 + (7 * 7 + 7 + 3) * 8)
3 + (8 * 8 * 9 + 5 * (3 * 2 + 2 + 2 * 2 + 8)) + (5 * (8 * 4) * 2 + 7 * (7 * 4 + 7 + 2 * 4 * 3) + 6) + 9 * 2 + (4 * (2 + 9 * 6 + 7 + 5 + 7) * 2 + (7 * 5 * 5 + 6 + 6) + 4 * (8 + 5))
((6 * 9 + 3 * 3 + 8) * (3 + 9) * 2 * 2 + 3 * 8) * 3
2 + 5 + (3 + 4 + 8 * 8) * 4 + 6 + 4
(6 * 6) + 9 * ((2 * 2 + 4) + 6 + 9 * 8 * (7 * 4 * 3 * 8 * 3)) * 7 * 2 + 3
9 * 4 + 2 * (3 * 6) * 8 + 3
(9 + 7) * 2 * 7 * 3
(8 * 7 + 2 + 8 * 3) * (3 * 7 * 2 * 2) + 4 * 3 * 3 + 6
(2 + 7 + 7) * 9 + 7 * 7
4 * (9 * 4 + 8 + 9 * 9) * 3 + (3 + 2 + (4 * 2 + 2 + 4 + 9 * 8) * 4) * 9 * 9
4 * 6 * ((4 * 7 + 4) * (2 + 8 + 7 * 5 + 2 * 7)) + 2 + 3 * 5
2 + 2 + 6 + (4 + (7 * 3)) + 5
(9 * (3 + 8 * 3 + 9) + (8 + 4 * 2 * 8 * 2 * 6) * 9 + (5 * 7 * 7) + 5) * 3 + 3
3 * 8 + (6 * 6 * 7 + 3 + 5 + (2 * 7 * 4 + 2 * 3 + 4)) + 8 + 7
7 * 7 + (6 * (3 * 4 * 4 + 7 + 4) * 7 * 7 + (7 + 4 * 4 + 6 * 9 * 7)) * 2 + 8
7 + 3 + 4 + (2 * 4 + 9 + 6 + 8) * 5 + 5
((5 * 7 + 4 + 8 + 8 * 5) + 3 * (3 + 3 + 5 * 9 + 2) * 6) + 2
6 * (8 * 4 + 7 + 7 * 8) * (5 * (2 + 2 + 4 + 8 + 6)) + 6 * 5
3 + 7 + 7 * (3 + 9 * 6 * 6) + 4 * 5
3 * ((4 + 3 * 9) * 4 + 2 * 5 + 6 + (5 + 2 + 2 + 4 + 8)) * 6 * 9 + 3 * (9 + 5 + (9 + 9 + 6 * 6 + 6 + 2) * 3)
(8 + 5 * 6 * 4 + 2 * 4) + (2 + 5 * (9 + 3 * 8 * 8) + 8 * 2 * 4) * (3 + 7 * (8 * 4 * 4 * 8)) * 9 + 8
6 * 6 + (6 + 3 + (8 * 8 * 8 + 5 + 3 * 6) + 7)
3 + 7 + ((3 * 6 + 4 + 9 * 3) * 9 * 7 + 2) + (8 + 2 * 3 * 8 * 7) * 6 + 5
2 + ((7 * 7 * 3) + (4 + 8 + 3 + 5) + 3 + 7 * 9 + 7)
9 * 9 * 7 + ((8 + 7 * 8 * 8 + 6 + 8) + 3 + 2 + 6 + 9)
3 + ((3 + 8 * 5) + 7 * 2) + (2 + 9)
((7 * 2 * 3 * 3 * 9) * 6 * 2 * (3 * 4) + 5) * 4 + 6 + 6 + 2
6 * 3 * 2 + 2 + 4 * 9
4 * 4 * ((2 * 4 * 4 * 2 + 9 + 7) + 9 + 8 + 5) * 9
6 + 4 * 4 * (7 + 6 + (3 * 7 * 6)) + 5
9 + 3 * ((7 * 9 * 7 * 7 + 5 * 4) * 6 * 3 + 3) * 7 * 5
3 + (8 + 6 + (7 * 3 * 9 + 4) + 9 + (3 + 8 + 9 * 8 + 5 * 6) + 8) + ((9 * 2 + 3 * 9 + 6) * 5 * 2 * 7 + 2 + 3) * (6 + (5 * 6 * 5 * 5 * 2) + 4 + 4 + 9) * 7 + (9 * (5 + 3 + 3 * 4 * 7) * 7 * 9 * 8 + 4)
2 * 4 * 3 + (4 + 7 * 4)
(4 * (6 + 4)) * 2 * 9
((8 + 8 + 2 + 4) + 5 * 8 + 2 + 5) * (5 + 9 * 6 + 5 + 9) * 8 * 4 + 7
2 + (7 * 6 * 7 * 4 * 2)
((7 * 5 + 4 * 8) * 5) + 4 + 7 * 8 * 8 + 3
4 + ((6 + 6 + 5 + 8) * 5 + 5 * 2)
(9 + 6 * (6 * 6 + 9 + 5) + 4) * 3 + ((6 + 5 * 2 * 7) * 2)
(3 * (6 * 9 + 9) * 9 * 3) + 2 + 7 + 5
5 * ((6 + 9 + 6) + 7)
2 + 5 + ((9 + 3 + 6 + 4 + 2 + 7) * 7 * 8 * 4 + 8 * 6) * 2 * 2
((4 * 7) + 9 + 7 + 8 + 3) + 6 * 5
(3 + 6 + 7 + (7 * 5)) * 6 + 8 * 9
9 + 5 * 6 * 9 + (8 + 6 * 6)
(7 * (9 + 8 * 4 * 7 + 3) + 8 * 3 * 3 * (6 * 8 * 8 * 3 + 9)) + 8 * 5 + 6 * 6
3 * 4 * ((9 + 2) * (7 * 4 * 9 + 5 + 7)) + (6 + 7 + 8) + 2 * 9
4 + 4 + 4 + 3
6 + 6 * 6 * ((5 + 4 + 5 * 5 + 6 * 9) * (6 * 3 * 9) + 7 + (2 * 4 + 3 * 4) + 6) + 9 + 3
4 * 5 + 5 * (8 + 4 * 7 + (3 * 6 + 3 * 6 + 5 + 9) * (4 + 5 + 6 * 7 + 7 * 8)) * 3
4 * (5 + (2 + 8 + 8 * 7 * 3 * 9) * 4 + 4 * 4) + 3
5 + (3 + (9 + 5 * 7) + 9) + 2
3 * (4 + 9)
9 + 5 * 7 + (7 + 5 + 8 * 3 * 7) + 8 + (2 + 3 * 9 * 4 + 3)
8 + (9 + 8) + 5 * 2 + 9 * ((4 * 8 + 5) * 3 * (8 + 3 + 6 + 5 + 7 * 4) * 5)
7 * 8 * 9 + 9 * (4 + (7 * 9 * 7 + 9))
7 * ((5 * 6 * 4 * 2) * 9 * 7 + 3) + 3
2 + 9 * 4
2 * 2 + (5 + 2 + 4 * 5 + 5) + 5 * 5
4 * 8 + 9 * (7 * 9 + 9 + 8) + 8
9 + ((6 + 6 + 2 * 2 * 9 * 9) * 8 * 9 * 4 * 6)
((2 + 8 + 9 * 4 + 7) * 7 * 5 + 3 + 9) * 4 * 3 + 7
(8 * 8 + (5 + 3 + 9 * 9)) + ((7 + 5 + 5) + 2 + 7) * (9 + 4 + 4) + 8 + 4 * 8
(9 * 6 * 3) * 3 * (6 * 2 + (3 + 4 + 2) * (7 + 9 * 5 + 5)) + 8 + 6
(8 + (3 + 8 * 3) * 3 * 6 + 5) + 5 + 7
((9 + 8) + (6 + 2) * 7) * 2 + 6 * 4 * 2
3 + 6 + (3 + 2 + 2 + 6 * 5 + 3)
(6 + 4 + (2 + 3 * 4 + 3) + 4 + (3 * 2 + 5) * 8) + 9
(3 + 3 + (9 * 4) * 2) + 5 * 7 * 3 * (8 + 4) + 6
(3 + 7) + 7 * (4 + 7 * (9 + 2 * 7) + 8 * 5 * 8) * 3
((2 * 4 * 5 * 4) + 3 + 3 * 7 + (8 * 2 + 6)) + 8
(5 + 6 + 9 + 8 + 8 + 3) * 2 + 6
4 + 6 + 8 + (9 * 8 * 2 + 6 * 4 + 3) * 5
(2 * 2 * 5 * 7 + 4) * 6 + 7 + 5 * (9 + 7)
5 * 9 + (7 * 4 + (9 * 7) * 5 * 7) + 5
(3 * 6 + 8) + 7 + 8 + 3 + 4
4 + 3 * 3 * 3 + 7 * (5 + 9 + 6 + 5 + 6)
3 + 9 + 8 * 4 + (6 * 7 * 3 + 7 * 5)
8 * 3 * (9 * (4 * 8 * 7 * 9 + 3 * 9) * 2 + 5 * 7) * 4 * 4
5 * 7 * (4 * (9 * 7 * 7 + 5) * (7 + 5 * 9 * 6 + 8) + 4 * 6 * (4 + 6 + 6)) + 3
4 + ((4 + 2) + 7 * (3 * 4 + 4 * 9 + 6 + 8)) + 7 * 4 + 5 * 7
((6 + 2 * 6) * 2) + (5 * 9 + 9 + 7) + 7 * 4 * 2
4 + 7 * 8 * 7 + (3 * 7 + (4 * 6 * 5 * 2)) * 5
6 + (9 + 6 * 4 + 6 * 6 + 3) * 5 * 2
8 * 3 * 7 * (6 + 3 + (7 * 6 * 5 + 9 + 4) + 6 * (2 * 7 + 5 + 6) + 3) * (6 * 4 + 2 * 9 * 7 + 6)
2 * (5 * (9 + 6)) * (4 + (5 * 2 + 3) + 7 * 6 * (8 * 4 * 4) * 4) * 5
2 * 2 * (3 + 8 + (4 * 3) + (6 + 5 + 6 + 8 * 5 * 4) + 8) + 3 * 3 + 8
4 * (8 * 4 * (3 * 6) + (5 + 3 + 4 * 6 + 4 + 7) * 4) + ((8 * 2) * (2 * 7) + (9 + 7 * 2 + 2))
3 * 4 + (7 * 8) + (5 + 7 * 8 + 9) * 9
(9 * 4) * 5 * 3 * 9 + 5 + 7
(7 * (9 * 4 + 9 + 5) * (2 + 3 * 3 + 8 + 2)) * 5
((9 * 2 * 9 + 8 + 8 * 3) * 5 * 7 + (2 * 2 * 3 * 2) + 6) + (7 + 8 * (4 + 2 * 9 * 6 + 3 * 9) * 4 * 2) + ((4 + 9 * 9 * 5 * 5) + 2 + 8 * 9 * 2) + 9 * 6
9 + 7 + 9 * (9 + 4 * 5) * 7
7 + 8 + ((2 + 3) * 4) * 6
6 * 4
(9 + 4 * (6 * 2 + 2 + 7 * 6) * 4) * ((6 + 8 + 7 + 4 + 9) + (7 + 9 + 8 + 6)) + (2 * (3 + 8) + 7 * (4 + 3) * 5)
(9 + (2 + 4 * 3 + 3 + 2 + 2) + 4 + (2 + 3) + 3) * ((6 + 6 + 7 * 6) * 2 + 3 + 9 * 4 + 8) * 2 * 2 + (9 + (5 * 6 * 8 * 3))
(8 * (4 + 9 * 3 + 6 + 6) * 7 * 6) * 5 + 4 + 8 * 8
3 * (8 * (4 * 2 + 6 * 6) * 5 + (9 * 6 * 3 + 4 * 4) + (5 * 2 + 7 + 5 + 5))
3 * ((6 + 7 + 7 * 9 + 4) * 6 + 8) + 8 + ((7 * 9 + 5) + 5 + 6) * (5 * 6 + 2 * 6 * 6 + 7)
(3 + (8 + 9) * 5 * 6 + 8) * (2 + 6 + (7 + 4 + 6) * 4 + 3) + 6 + 2 * (7 + 8 * 5 + 7)
5 * (2 + 2 * 6 * (6 + 2 * 4 + 8) + 4 * 5)
4 * 9 * 7 + 7 * 3
((7 + 7 * 6) * (5 * 9 * 6)) * 5 + 5 + 3 + 8 + 4
(4 + (5 + 7 + 7 + 7)) * 4 * (7 * 7 + 3) * 6 * 3 * ((4 * 7) + 4 * 9)
2 * 2 + 6 * ((8 * 9 + 7 * 7) * (5 * 9 * 6 * 6 * 7 + 6) + 5 * 4) + (8 * 7 + 5 * 5)
9 + 6 * ((9 + 5 + 2 + 7 + 3) * 3 * 6) + 9 + 7 * 9
4 * (8 * (4 + 6 * 4 + 8 + 2 + 3) * 5 + 8 * 5) * ((5 + 4 + 2 + 3 * 2 + 9) * (4 * 8 + 9 + 4 * 4 + 4)) + 2
9 + (3 * 3 + 4 + 5 + 6) + 3 + (5 + 7) + (3 + 5)
((3 * 3) + 9 + 4 + 8 + 9 + 2) * (7 * (8 + 8 * 7) * 4 + (4 * 4 + 2 * 5 * 2) * 9)
2 + (7 + (6 * 2 * 4 + 2 * 9 * 2) * 4 * 5) + (4 + 5)
2 + (4 + 3 * 5 * 3 + 3) + 5
6 * 6 + 4 + 8 + 4
((3 + 4 + 2 + 6) + 8) + 2
(4 + 2 * 4 + 6 + 8 * 3) + (3 * 8 + 9 + 7 * 5 + 2) + 7 * 6 * (6 * 3 * 5 * 6) * 6
5 * 6 + (8 * (6 + 6 * 4 + 2) * (2 + 5 + 4 * 7 + 3 * 6) + 8 + 4) * 6 * 6
3 * 6 * 5 * (3 + 4 + 3) * 3 * 5
9 * (4 * (3 * 9 * 4 * 9 + 6 * 7) * 7 + 7 * 7)
7 * 4 + 2 + (7 + (3 * 4) * 9 * 8) * 2
(3 * (9 * 3 + 4) * 3) + 2 + ((3 + 7 + 6 + 4 * 6 + 9) + (3 * 7)) * 8
(4 + 3 + 6 * 7) * (7 * 7 * (8 * 4 + 2 * 8 * 2 + 3) + 4 + 8 * 3) * 8 + 2
3 + (9 + 2 * 3)
7 * 7 * 5 + (2 * (3 + 8 + 2) + 7)
4 * (7 + 7 * 9) * ((8 + 2 * 4 * 6 + 4 + 9) * 5 * 6 + 3 + 3) + (4 + 4 * 2 * (4 + 4 + 3 + 3 + 7) + 2)
2 * 2 + 2 * 9 + 9
9 * 4 + (2 * 5) + 8 * 5
(8 + 9 * (2 * 2 + 3 * 9 * 3 + 3) * 8 * 8) * (9 * 4 * 6 * 7)
7 * (6 * 8 * 8 * (3 * 2 + 4 * 7 + 9 + 6) + (4 + 9 * 3 * 3 + 7 + 3)) + (4 * 3) * 5 * 8 * 4
4 * 8
7 * 2 * (4 + 2 * 7) + ((3 * 2 + 2 * 9 * 6 + 7) + 8 * 6 * 6 + 9) * 7 + 5
(4 * (2 + 4 * 4 + 4) * (8 + 7)) * 7 * 8 * 9 * 9
9 + 5 * 3 * (8 + (9 * 5 + 4 * 3) * 7 * 7 * (6 + 4 * 5 + 7 * 2 + 5)) + 3
(4 * 4 * 9 * 8 + (7 * 2 + 9 * 6 + 4)) + 5
(8 * 4 * 8) + 9 * (7 * 5) * 5
(3 * 4 * 9 * 9 + (5 * 8 * 5 + 4 + 7) + (7 * 8 * 5 + 9 + 5 + 5)) + 2
(8 * 3 + 2 + 9 + 2 + 2) + 3
8 * (9 + 7) + 7 * 2 * (8 + 3 + 5 + 7 + 6) + 2
2 * (2 * 6 + 8 + (7 + 6 + 2) + 3 * 6) * (5 + 8 + 7 + (9 + 9 * 3 * 7)) * 7 + 4
(5 + 5 + 8 * (5 + 2 + 3)) * 9 * 9 + ((2 * 6 * 6 * 9) * 5 + 4 * 6) + 5
5 + 3 * 4 * (3 + 4 + 8) * (3 * 9 + 9 + 7 * 8)
6 * 4 + ((7 + 3 * 3 + 3) + (3 * 7) * 3 + 9 * 2)
(5 * 3 * 7 * 2 + 6) * (2 * 6) + 6
7 * 4 + (6 + 2 * 3 * 9 + (8 + 9) * (5 + 2 + 2))
2 + 2 + ((9 * 2 * 4 + 8 + 3 * 7) * 4 + (2 + 6 + 5 * 5 * 2 + 8) + 5 * 2 * 8) + 4
((2 + 3 + 2 * 8 * 4 * 6) * 4 * 2 * 8 * 5 + 5) + 8 * 9
5 + 4 * (5 + (4 * 4 * 8) * 3 + 3)
4 * 3 + (7 * 4) + 8
4 + (9 * 8 * 5 * 4) + 2 + 4
4 + (7 + 9 * 6) * 5 + 4 + 4 + 6
(2 + 9 * 3) * 2 * (7 + (5 + 8 * 7) + 8)
4 * 3 + 6 + 3 + ((5 + 6 + 7) * 5 * 2)
9 + 8 * (4 + 3) * 4 + 6 * 9
4 * (6 + 5 * 5 * 8 * 6 * 6)
(8 + 7 * 4 + 4 + (2 + 8 * 2 + 4 + 6 + 2)) + 3
2 + (9 * 4) * 7
4 + 7
(9 * (2 * 2 + 4) + 3 * 3 + (7 * 4 * 6 * 4 + 5 + 4) * (8 * 8 * 7 + 8)) * 6 + 3 + 7 + (6 * 5 * 8 * 6) + 2
2 + 6 + (4 + 2 * 7 + 3 * 9 * 9) * ((8 + 2 * 4 * 4 * 2 + 3) + (3 + 9 + 4) * 3 * 4) + 8
8 + (6 * 5 + 2 * 6 * (2 * 3 + 2 * 9 * 7 * 2) + 6) * 2 * 8 + 7 + (6 * 5 * 9 * 6)
6 + (3 + 9 + 3 * 6)
(2 + 5 * 9 * 2) * 2 * 5 + ((4 * 2 * 7 + 8 + 7) + 7 + 6 * 4 * 3 * 6)
(7 + 3 * 6 * (2 * 7) + 7 * 8) * 7 * 2 + 4 + 5 * 2
(6 * 2 * 5) + (7 + 7) * 9 * 5 * 2 * 5
3 * (8 + 3 * 3 * 2) + 5 + (9 * (5 + 8 * 8 * 5 + 8) * 4 + 7 * 8 + 3)
"""