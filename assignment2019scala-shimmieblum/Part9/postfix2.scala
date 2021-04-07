// Shunting Yard Algorithm 
// including Associativity for Operators 
// =====================================

object CW9b {


// type of tokens
type Toks = List[String]

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// left- and right-associativity
abstract class Assoc
case object LA extends Assoc
case object RA extends Assoc



// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}


// the precedences of the operators
val precs = Map("+" -> 1,
  		"-" -> 1,
		"*" -> 2,
		"/" -> 2,
                "^" -> 4)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")

// (3) Implement the extended version of the shunting yard algorithm.
// This version should properly account for the fact that the power 
// operation is right-associative. Apart from the extension to include
// the power operation, you can make the same assumptions as in 
// basic version.

def is_op(str: String):Boolean = {
  ops.contains(str)
}

def equal_prec(op1: String, op2: String): Boolean = {
  precs(op1) == precs(op2)
}

def higher_prec(op1: String, op2: String) : Boolean = {
  precs(op1) > precs(op2) 
}

abstract class Token
case object Operator extends Token
case object Number extends Token
case object LeftParam extends Token
case object RightParam extends Token

def get_token(str: String):Token = {
  if(is_op(str)) Operator
  else if(str == "(") LeftParam
  else if(str == ")") RightParam
  else Number
}

def sort_operators(operator: String, st: Toks, out: Toks):(Toks, Toks) = {
  if(st.isEmpty || st.head == "(" ) (operator::st, out) 
  else if(higher_prec(st.head, operator)) sort_operators(operator,st.tail, out:+st.head)
  else if(equal_prec(st.head, operator) && assoc(st.head) == LA) sort_operators(operator, st.tail, out:+st.head)
  else (operator::st, out)
}

def pop_parenthesis(st: Toks, out: Toks) : (Toks,Toks) ={
  if(st.head == "(") (st.tail, out)
  else pop_parenthesis(st.tail, out:+st.head)
}

def sort_stack(st: Toks, out: Toks) : Toks ={
  if(st.isEmpty) out
  else sort_stack(st.tail, out:+st.head)
}
 
def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = {
  if(toks.isEmpty) sort_stack(st, out)
  else{
    val output = get_token(toks.head) match{
      case Operator => {
        val result = sort_operators(toks.head,st,out)
        syard(toks.tail, result._1, result._2)
      }
      
      case LeftParam => syard(toks.tail, toks.head::st, out)
      case RightParam => {
        val results = pop_parenthesis(st,out)
        syard(toks.tail, results._1, results._2)
      }
      case Number => syard(toks.tail, st, out:+toks.head)
    }
    output
  }
}


// test cases
// syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +


// (4) Implement a compute function that produces a Long(!) for an
// input list of tokens in postfix notation.

def power_function(base: Long, expo: Long) : Long = {
  if(expo == 0) 1
  else if(expo == 1) base
  else base * power_function(base, expo - 1)
}

def compute(toks: Toks, st: List[Long] = Nil) : Long = {
  if(toks.isEmpty) st.head
  else{
    val value = toks.head match{
      case "+" => compute(toks.tail, (st.head + st.tail.head)::st.tail.tail)
      case "-" => compute(toks.tail, (st.tail.head - st.head )::st.tail.tail)
      case "*" => compute(toks.tail, (st.head * st.tail.head)::st.tail.tail)
      case "/" => compute(toks.tail, (st.tail.head / st.head)::st.tail.tail)
      case "^" => compute(toks.tail, power_function(st.tail.head,st.head)::st.tail.tail)
      case _ => (compute(toks.tail,  toks.head.toLong::st))
    }
    value
  }
}


// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))   // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
// compute(syard(split("4 ^ 3 ^ 2")))      // 262144
// compute(syard(split("4 ^ ( 3 ^ 2 )")))  // 262144
// compute(syard(split("( 4 ^ 3 ) ^ 2")))  // 4096
// compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))   // 65536

}
