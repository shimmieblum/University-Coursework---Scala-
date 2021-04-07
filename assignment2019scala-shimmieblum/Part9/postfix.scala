object CW9a {

// type of tokens
type Toks = List[String]

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/")

// the precedences of the operators
val precs = Map("+" -> 1,
		"-" -> 1,
		"*" -> 2,
		"/" -> 2)

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList


// (1) Implement below the shunting yard algorithm. The most
// convenient way to this in Scala is to implement a recursive 
// function and to heavily use pattern matching. The function syard 
// takes some input tokens as first argument. The second and third 
// arguments represent the stack and the output of the shunting yard 
// algorithm.

// In the marking, you can assume the function is called only with 
// an empty stack and an empty output list. You can also assume the
// input os  only properly formatted (infix) arithmetic expressions
// (all parentheses will be well-nested, the input only contains 
// operators and numbers).

// You can implement any additional helper function you need. I found 
// it helpful to implement two auxiliary functions for the pattern matching:  

def is_op(op: String) : Boolean = {
	ops.contains(op)
}

def higher_prec(op1: String, op2: String) : Boolean = {
	precs(op1) >= precs(op2)
}

abstract class Token
case object Operator extends Token
case object RightParen extends Token
case object LeftParen extends Token
case object Number extends Token


def get_token(str: String):Token = {
	if(is_op(str)) Operator
	else if(str == "(") LeftParen
	else if(str == ")") RightParen
	else Number
  }

def pop_parenthesis(st: Toks, out: Toks) : (Toks,Toks) = {
	if(st.head == "(") (st.tail, out)
	else pop_parenthesis(st.tail,out:+st.head)
}

def sort_stack(st: Toks, out: Toks): Toks = {
	if(st.isEmpty) out
	else sort_stack(st.tail, out:+st.head)
}


def sort_operators(operator: String, st: Toks, out: Toks):(Toks, Toks) = {
	if(st.isEmpty || st.head == "(" ) (operator::st, out)
	else if(higher_prec(st.head, operator)) sort_operators(operator,st.tail, out:+st.head)
	else (operator::st, out)
}


def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = {
	if(toks.isEmpty) sort_stack(st,out)
	else {
		val output = get_token(toks.head) match{
			case Operator => {
				val result = sort_operators(toks.head, st, out)
				syard(toks.tail, result._1, result._2)
			}
			case LeftParen => syard(toks.tail,toks.head::st, out)
			case RightParen => {
				val result = pop_parenthesis(st,out)
				syard(toks.tail, result._1,result._2)
			}
			case Number => syard(toks.tail, st, out:+toks.head)
		}
		output
	}
}


// test cases
// syard(split("3 + 4 * ( 2 - 1 )"))  // 3 4 2 1 - * +
// syard(split("10 + 12 * 33"))       // 10 12 33 * +
// syard(split("( 5 + 7 ) * 2"))      // 5 7 + 2 *
// syard(split("5 + 7 / 2"))          // 5 7 2 / +
// syard(split("5 * 7 / 2"))          // 5 7 * 2 /
// syard(split("9 + 24 / ( 7 - 3 )")) // 9 24 7 3 - / +

// syard(split("3 + 4 + 5"))           // 3 4 + 5 +
// syard(split("( ( 3 + 4 ) + 5 )"))    // 3 4 + 5 +
// syard(split("( 3 + ( 4 + 5 ) )"))    // 3 4 5 + +
// syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) // 3 4 5 + +

 
// (2) Implement a compute function that evaluates an input list
// in postfix notation. This function takes a list of tokens
// and a stack as argumenta. The function should produce the 
// result as an integer using the stack. You can assume 
// this function will be only called with proper postfix 
// expressions.    

def compute(toks: Toks, st: List[Int] = Nil) : Int =  {
	if(toks.isEmpty) st.head
	else{
		val value = toks.head match{
			case "+" => compute(toks.tail, (st.head + st.tail.head)::st.tail.tail)
			case "-" => compute(toks.tail, (st.tail.head - st.head )::st.tail.tail)
			case "*" => compute(toks.tail, (st.head * st.tail.head)::st.tail.tail)
			case "/" => compute(toks.tail, (st.tail.head / st.head)::st.tail.tail)
			case _ => (compute(toks.tail, toks.head.toInt::st))
		}
		value
	}
}


// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))  // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15

// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================

}


