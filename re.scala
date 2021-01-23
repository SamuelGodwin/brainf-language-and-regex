// Part 1 about Regular Expression Matching
//==========================================


object CW8a {

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp   // alternative 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp   // sequence
case class STAR(r: Rexp) extends Rexp             // star


// some convenience for typing in regular expressions

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

// (1a) the function nullable; this 
// function checks whether a regular expression
// can match the empty string and Returns a boolean
// accordingly.

def nullable (r: Rexp) : Boolean = r match {

	case ZERO						=> false
	case ONE						=> true
	case CHAR(c)					=> false
	case ALT(r1, r2)				=> nullable(r1) || nullable(r2)
	case SEQ(r1, r2)				=> nullable(r1) && nullable(r2)
	case STAR(r)					=> true
	
}

// (1b) the function der; this
// function calculates the derivative of a 
// regular expression w.r.t. a character.

def der (c: Char, r: Rexp) : Rexp = r match {

	case ZERO => ZERO
	case ONE => ZERO
	case CHAR(d) => if (c == d) ONE
					else ZERO
		
	case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
	
	case SEQ(r1, r2)	=> if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
								else SEQ(der(c, r1), r2)
		
	case STAR(r) => SEQ(der(c, r), STAR(r))

}

// (1c) the simp function; this
// function simplifies a regular expression from
// the inside out, like you would simplify arithmetic 
// expressions; however it does not simplify inside 
// STAR-regular expressions.

def simp(r: Rexp) : Rexp = r match { 

	case SEQ(r1, r2)	=> simplify(SEQ(simp(r1), simp(r2)))
	case ALT(r1, r2)	=> simplify(ALT(simp(r1), simp(r2)))
	case _ 				=> simplify(r)
		
	//else Rexp //if it can't be simplified

	//IT DOES NOT SIMPLIFY INSIDE STAR EXPRESSIONS
	
}

def simplify(r: Rexp) : Rexp = r match {

	case SEQ(r, ZERO) => ZERO
	case SEQ(ZERO, r) => ZERO
	case SEQ(r, ONE) => r
	case SEQ(ONE, r) => r
	case ALT(r, ZERO) => r
	case ALT(ZERO, r) => r
	case ALT(r1, r2) => {
		if (r1==r2) r1
		else r
	}
	case _ => r
	
}

// (1d) two functions below; the first 
// calculates the derivative w.r.t. a string; the second
// is the regular expression matcher taking a regular
// expression and a string and checks whether the
// string matches the regular expression

def ders (s: List[Char], r: Rexp) : Rexp = s match {

	case Nil => r
	
	case c::cs => ders(cs, simp(der(c, r)))

}

def matcher(r: Rexp, s: String): Boolean = {

	val dersval = ders(s.toList, r)

	nullable(dersval)

}

// (1e) the size function for regular
// expressions.

def size(r: Rexp): Int = r match {

	case ZERO			=> 1
	case ONE			=> 1
	case CHAR(c)		=> 1
	case ALT(r1, r2)	=> 1 + size(r1) + size(r2)
	case SEQ(r1, r2)	=> 1 + size(r1) + size(r2)
	case STAR(r)		=> 1 + size(r)

}

// some testing data

/*
matcher(("a" ~ "b") ~ "c", "abc")  // => true
matcher(("a" ~ "b") ~ "c", "ab")   // => false
// the supposedly 'evil' regular expression (a*)* b
val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))
matcher(EVIL, "a" * 1000 ++ "b")   // => true
matcher(EVIL, "a" * 1000)          // => false
// size without simplifications
size(der('a', der('a', EVIL)))             // => 28
size(der('a', der('a', der('a', EVIL))))   // => 58
// size with simplification
size(simp(der('a', der('a', EVIL))))           // => 8
size(simp(der('a', der('a', der('a', EVIL))))) // => 8
// Java needs around 30 seconds for matching 28 a's with EVIL. 
//
// Lets see how long it takes to match strings with 
// 0.5 Million a's...it should be in the range of some
// seconds.
def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start)/(i * 1.0e9)
}
for (i <- 0 to 5000000 by 500000) {
  println(i + " " + "%.5f".format(time_needed(2, matcher(EVIL, "a" * i))))
}
*/


}