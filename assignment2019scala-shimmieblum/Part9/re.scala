// Core Part about Regular Expression Matching
//=============================================

object CW9c {

// Regular Expressions
abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp   // alternative 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp   // sequence
case class STAR(r: Rexp) extends Rexp             // star


// some convenience for typing regular expressions

import scala.language.implicitConversions    
import scala.language.reflectiveCalls 

def charlist2rexp(s: List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}
implicit def string2rexp(s: String): Rexp = charlist2rexp(s.toList)

implicit def RexpOps (r: Rexp) = new {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

implicit def stringOps (s: String) = new {
  def | (r: Rexp) = ALT(s, r)
  def | (r: String) = ALT(s, r)
  def % = STAR(s)
  def ~ (r: Rexp) = SEQ(s, r)
  def ~ (r: String) = SEQ(s, r)
}

// (5) Complete the function nullable according to
// the definition given in the coursework; this 
// function checks whether a regular expression
// can match the empty string and Returns a boolean
// accordingly.

def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case CHAR(c) => false
  case ONE => true
  case STAR(r) => true
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
}

// nullable(ZERO) //== false
// nullable(ONE) //== true
// nullable(CHAR('a')) //== false
// nullable(ZERO | ONE) //== true
// nullable(ZERO | CHAR('a')) //== false
// nullable(ONE ~ ONE) //== true
// nullable(ONE ~ CHAR('a')) //== false
// nullable(STAR(ZERO)) //== true


// (6) Complete the function der according to
// the definition given in the coursework; this
// function calculates the derivative of a 
// regular expression w.r.t. a character.

def der (c: Char, r: Rexp) : Rexp = r match {
  case ZERO | ONE => ZERO
  case CHAR(d) => if(c == d) ONE else ZERO
  case ALT(r1, r2) => der(c, r1) | der(c, r2)
  case SEQ(r1, r2) => if(nullable(r1)) (der(c ,r1) ~ r2)|der(c, r2) else der(c, r1) ~ r2
  case STAR(r) => der(c,r) ~ r.%
}


// (7) Complete the simp function according to
// the specification given in the coursework; this
// function simplifies a regular expression from
// the inside out, like you would simplify arithmetic 
// expressions; however it does not simplify inside 
// STAR-regular expressions.


def simp(r: Rexp) : Rexp = r match{
  case CHAR(c) => r
  case ONE => r
  case ZERO => r
  case STAR(p) => r
  case SEQ(r1, r2) => (simp(r1), simp(r2)) match{
    case (ZERO, p) => ZERO
    case (p, ZERO) => ZERO
    case (ONE, p) => p
    case (p, ONE) => p
    case(p,q) => p ~ q
  }
  case ALT(r1, r2) => (simp(r1), simp(r2)) match {
    case(ZERO, p) => p
    case (p, ZERO) => p
    case (p,q) if(p==q) => p
    case (p,q) => p|q
  }
}


// simp(ZERO | ONE) //== ONE
// simp(STAR(ZERO | ONE)) //== STAR(ZERO | ONE)
// simp(ONE ~ (ONE ~ (ONE ~ CHAR('a')))) //== CHAR('a')
// simp(ONE ~ (ONE ~ (ONE ~ ZERO))) //== ZERO
// simp(ALT(ONE ~ (ONE ~ (ONE ~ ZERO)), CHAR('a'))) //== CHAR('a')


// simp(CHAR('a') | CHAR('a'))// == CHAR('a')
// simp(ONE | CHAR('a')) //== (ONE | CHAR('a'))
// simp(ALT((CHAR('a') | ZERO) ~ ONE,
// ((ONE | CHAR('b')) | CHAR('c')) ~ (CHAR('d') ~ ZERO))) //== CHAR('a')
// simp((ZERO | ((ZERO | ZERO) | (ZERO | ZERO))) ~ ((ONE | ZERO) | ONE ) ~ (CHAR('a'))) //== ZERO
// simp(ALT(ONE | ONE, ONE | ONE))// == ONE
// simp(ALT(ZERO | CHAR('a'), CHAR('a') | ZERO)) //== CHAR('a')
// simp(ALT(ONE | CHAR('a'), CHAR('a') | ONE)) //== ALT(ONE | CHAR('a'), CHAR('a') | ONE)


// (8) Complete the two functions below; the first 
// calculates the derivative w.r.t. a string; the second
// is the regular expression matcher taking a regular
// expression and a string and checks whether the
// string matches the regular expression

def ders (s: List[Char], r: Rexp) : Rexp = s match{
  case Nil => r
  case c::cs => ders(cs, simp(der(c,r))) 
}

def matcher(r: Rexp, s: String): Boolean = {
  nullable(ders(s.toList, r))
}


// (9) Complete the size function for regular
// expressions according to the specification 
// given in the coursework.

def size(r: Rexp): Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(c) => 1
  case ALT(r1, r2) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2) => 1 + size(r1) + size(r2)
  case STAR(r1) => 1 + size(r1)
}


// some testing data


matcher(("a" ~ "b") ~ "c", "abc")  // => true
matcher(("a" ~ "b") ~ "c", "ab")   // => false

// // the supposedly 'evil' regular expression (a*)* b
// val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

// matcher(EVIL, "a" * 1000 ++ "b")   // => true
// matcher(EVIL, "a" * 1000)          // => false

// // size without simplifications
// size(der('a', der('a', EVIL)))             // => 28
// size(der('a', der('a', der('a', EVIL))))   // => 58

// // size with simplification
// size(simp(der('a', der('a', EVIL))))           // => 8
// size(simp(der('a', der('a', der('a', EVIL))))) // => 8

// Python needs around 30 seconds for matching 28 a's with EVIL. 
// Java 9 and later increase this to an "astonishing" 40000 a's in
// 30 seconds.
//
// Lets see how long it really takes to match strings with 
// 5 Million a's...it should be in the range of a couple
// of seconds.

// def time_needed[T](i: Int, code: => T) = {
//   val start = System.nanoTime()
//   for (j <- 1 to i) code
//   val end = System.nanoTime()
//   (end - start)/(i * 1.0e9)
// }

// for (i <- 0 to 5000000 by 500000) {
//   println(i + " " + "%.5f".format(time_needed(2, matcher(EVIL, "a" * i))))
// }

// another "power" test case 
// simp(Iterator.iterate(ONE:Rexp)(r => SEQ(r, ONE | ONE)).drop(50).next) == ONE

// the Iterator produces the rexp
//
//      SEQ(SEQ(SEQ(..., ONE | ONE) , ONE | ONE), ONE | ONE)
//
//    where SEQ is nested 50 times.



}
