// Part 2 about an Interpreter for the Brainf*** language
//========================================================

object CW8b {

type Mem = Map[Int, Int]

// (2a) functions for safely reading  
// and writing brainf*** memory. Safely read should
// Return the value stored in the Map for a given memory
// pointer, if it exists; otherwise it Returns 0. The
// writing function generates a new Map with the
// same data, except at the given memory pointer the
// value v is stored.


def sread(mem: Mem, mp: Int) : Int = {

	//If the Map is not defined at the memory pointer, sread returns 0.

	mem.getOrElse(mp, 0)

}
	

def write(mem: Mem, mp: Int, v: Int) : Mem = {

	//needs an if check here!!
	//if (mem contains mp) //if map already defined at the position they're trying to add to ?
	
	val newMem = mem + (mp -> v)
		
	newMem

}


// (2b) implementation of the two jumping instructions in the 
// brainf*** language. In jumpRight, given a program and 
// a program counter move the counter to the right 
// until the command after the *matching* ]-command. Similarly, 
// jumpLeft implements the move to the left to just after
// the *matching* [-command. The levels are used to find the
// *matching* bracket.

def jumpRight(prog: String, pc: Int, level: Int) : Int = {

	//length of prog. create a list of 0 to this. also makes prog into a list. map list1 to list2 for mapping.
	val keyList = (0 to prog.length-1).toList
	val valList = prog.toList
	
	val progMap = (keyList zip valList) toMap
	
	if (progMap(pc) == '[')
	
		//val newLevel = level + 1
		//recursive call but increase pc
		// if defined, for next key after... call recursively ... until closing is found...
		if (progMap.isDefinedAt(pc+1))
			jumpRight(prog, pc+1, level+1)
		else {
			//not found
			pc+1 // STAND IN VALUE
		}
		
	else if (progMap(pc) == ']')
		if (level == 0) {
			val newPc = pc + 1
			//it's done HERE!
			newPc // STAND IN VALUE
		}
		else {
			//val newLevel = level - 1
			//recursive call but increase pc
			// if defined, for next key after... call recursively ... until closing is found...
			if (progMap.isDefinedAt(pc+1))
				jumpRight(prog, pc+1, level-1)
			else {
				//not found
				pc+1 // STAND IN VALUE

			}
		}
		
	else {
		//recursive call but increase pc
		// if defined, for next key after... call recursively ... until closing is found...
		if (progMap.isDefinedAt(pc+1)) {
			jumpRight(prog, pc+1, level)
		}
		else {
			//not found
			pc+1 // STAND IN VALUE

		}
	}
}

def jumpLeft(prog: String, pc: Int, level: Int) : Int = {

	//length of prog. create a list of 0 to this. also makes prog into a list. map list1 to list2 for mapping.
	val keyList = (0 to prog.length-1).toList
	val valList = prog.toList
	
	val progMap = (keyList zip valList) toMap
	
	if (progMap(pc) == ']')
	
		//val newLevel = level + 1
		//recursive call but increase pc
		// if defined, for next key after... call recursively ... until closing is found...
		if (progMap.isDefinedAt(pc-1))
			jumpLeft(prog, pc-1, level+1)
		else {
			//not found
			pc-1 // STAND IN VALUE
		}
		
	else if (progMap(pc) == '[')
		if (level == 0) {
			val newPc = pc + 1
			//it's done HERE!
			newPc // STAND IN VALUE
		}
		else {
			//val newLevel = level - 1
			//recursive call but increase pc
			// if defined, for next key after... call recursively ... until closing is found...
			if (progMap.isDefinedAt(pc-1))
				jumpLeft(prog, pc-1, level-1)
			else {
				//not found
				pc-1 // STAND IN VALUE

			}
		}
		
	else {
		//recursive call but increase pc
		// if defined, for next key after... call recursively ... until closing is found...
		if (progMap.isDefinedAt(pc-1)) {
			jumpLeft(prog, pc-1, level)
		}
		else {
			//not found
			pc-1 // STAND IN VALUE

		}
	}
}

//run("[->+<]", 0, 0, Map(0 -> 10))

// (2c) the run function that interprets (runs) a brainf***
// program: the arguments are a program, a program counter,
// a memory counter and a brainf*** memory. It Returns the
// memory at the stage when the execution of the brainf*** program
// finishes. The interpretation finishes once the program counter
// pc is pointing to something outside the program string.
// If the pc points to a character inside the program, the pc, 
// memory pointer and memory need to be updated according to 
// rules of the brainf*** language. Then, recursively, the run 
// function continues with the command at the new program
// counter. 
//
// Implementation of the start function that calls run with the program
// counter and memory counter set to 0.

def run(prog: String, pc: Int, mp: Int, mem: Mem) : Mem = {

	if (pc > prog.length-1 || pc < 0) 
	{
		//println("out of range")
		mem
	}
	else {
	
		val keyList = (0 to prog.length-1).toList
		
		val valList = prog.toList
		
		val progMap = (keyList zip valList) toMap	
		
		if (progMap(pc) == '>') {
			val pcNew = pc + 1
			val mpNew = mp + 1
			//mem unchanged
			
			run(prog, pcNew, mpNew, mem)
		}
		
		else if (progMap(pc) == '<') {
			val pcNew = pc + 1
			val mpNew = mp - 1
			//mem unchanged
			
			run(prog, pcNew, mpNew, mem)
		}
		
		else if (progMap(pc) == '+') {

			val pcNew = pc + 1

			//mp unchanged
			
			//mem updated with mp -> mem(mp) + 1	 
			
			val newMem = write(mem, mp, (sread(mem, mp) + 1))
			
			run(prog, pcNew, mp, newMem)
		}
		
		else if (progMap(pc) == '-') {
			val pcNew = pc + 1
			//mp unchanged
			//mem updated with mp -> mem(mp) - 1		
			
			val newMem = write(mem, mp, (sread(mem, mp) - 1))	
			
			run(prog, pcNew, mp, newMem)
		}
		
		else if (progMap(pc) == '.') {
			val pcNew = pc + 1
			//mp and mem unchanged
			
			run(prog, pcNew, mp, mem)
		}
		
		else if (progMap(pc) == ',') {
			val pcNew = pc + 1
			//mp unchanged
			
			//mem updated with mp -> input	
			val input = Console.in.read().toByte
			val newMem = write(mem, mp, input)
			//the input is given by Console.in.read().toByte
			
			run(prog, pcNew, mp, newMem)
		}
		
		else if (progMap(pc) == '[') {
			if (mem(mp) == 0) {
				val pcNew = jumpRight(prog, pc + 1, 0)
				//mp and mem unchanged
				
				run(prog, pcNew, mp, mem)
			}
			//else if (mem(mp) != 0) {
			else {
				val pcNew = pc + 1
				//mp and mem unchanged
				
				run(prog, pcNew, mp, mem)
			}
		}
		
		else if (progMap(pc) == ']') {
			if (mem(mp) != 0) {
				val pcNew = jumpLeft(prog, pc - 1, 0) // gets 1
				//mp and mem unchanged
				
				run(prog, pcNew, mp, mem)
			}
			//else if (mem(mp) == 0) {
			else {
				val pcNew = pc + 1
				//mp and mem unchanged
				
				run(prog, pcNew, mp, mem)
			}
			
		}
		
		else {
			val pcNew = pc + 1
			//mp and mem unchanged
			
			run(prog, pcNew, mp, mem)
		}
		
	
	}

}




def start(prog: String, mem: Mem) = {

	run(prog, 0, 0, mem)

}






// some sample bf programs collected from the Internet
//==================================================


/*
// first some contrived (small) programs
// clears the 0-cell
start("[-]", Map(0 -> 100)) 
// copies content of the 0-cell to 1-cell
start("[->+<]", Map(0 -> 10))
// copies content of the 0-cell to 2-cell and 4-cell
start("[>>+>>+<<<<-]", Map(0 -> 42))
start("+++[>+++++<-]", Map(0 -> 10))
// prints out numbers 0 to 9
start("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""", Map())
// some more "useful" programs
// hello world program 1
start("""++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++
       ..+++.>>.<-.<.+++.------.--------.>>+.>++.""", Map())
// hello world program 2
start("""++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>+
      +.<<+++++++++++++++.>.+++.------.--------.>+.>.""", Map())
// draws the Sierpinski triangle
start("""++++++++[>+>++++<<-]>++>>+<[-[>>+<<-]+>>]>+[-<<<[
      ->[+[-]+>++>>>-<<]<[<]>>++++++[<<+++++>>-]+<<++.[-]<<
      ]>.>+[>>]>+]""", Map())
//Fibonacci numbers below 100
start("""+++++++++++
      >+>>>>++++++++++++++++++++++++++++++++++++++++++++
      >++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>
      +<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[-
      <-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<
      -]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]
      >[<<+>>[-]]<<<<<<<]>>>>>[+++++++++++++++++++++++++
      +++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++
      ++++++++++++++++++++++++++++++++++++++++++++.[-]<<
      <<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<
      [-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]""", Map())
//outputs the square numbers up to 10000
start("""++++[>+++++<-]>[<+++++>-]+<+[
    >[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+
    >>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]
    <<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]""", Map())
//Collatz numbers (need to be typed in)
start(""">,[[----------[
      >>>[>>>>]+[[-]+<[->>>>++>>>>+[>>>>]++[->+<<<<<]]<<<]
      ++++++[>------<-]>--[>>[->>>>]+>+[<<<<]>-],<]>]>>>++>+>>[
      <<[>>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<<]]<[>+<-]>]
      >[>[>>>>]+[[-]<[+[->>>>]>+<]>[<+>[<<<<]]+<<<<]>>>[->>>>]+>+[<<<<]]
      >[[>+>>[<<<<+>>>>-]>]<<<<[-]>[-<<<<]]>>>>>>>
      ]>>+[[-]++++++>>>>]<<<<[[<++++++++>-]<.[-]<[-]<[-]<]<,]""", Map())
// infinite Collatz (never stops)
start(""">>+>+<[[->>[>>]>>>[>>]+[<<]<<<[<<]>[>[>>]>>+>[>>]<+<[<<]<<<[<
      <]>-]>[>>]>>[<<<<[<<]>+>[>>]>>-]<<<<[<<]+>>]<<[+++++[>+++++++
      +<-]>.<++++++[>--------<-]+<<]>>[>>]+[>>>>[<<+>+>-]<-[>+<-]+<
      [<<->>-[<<+>>[-]]]>>>[<<<+<<+>>>>>-]<<<[>>>+<<<-]<<[[-]>+>>->
      [<+<[<<+>>-]<[>+<-]<[>+<-]>>>>-]<[>+<-]+<[->[>>]<<[->[<+++>-[
      <+++>-[<+++>-[<[-]++>>[-]+>+<<-[<+++>-[<+++>-[<[-]+>>>+<<-[<+
      ++>-[<+++>-]]]]]]]]]<[>+<-]+<<]>>>+<[->[<+>-[<+>-[<+>-[<+>-[<
      +>-[<+>-[<+>-[<+>-[<+>-[<[-]>>[-]+>+<<-[<+>-]]]]]]]]]]]<[>+<-
      ]+>>]<<[<<]>]<[->>[->+>]<[-[<+>-[<->>+<-[<+>-[<->>+<-[<+>-[<-
      >>+<-[<+>-[<->>+<-[<+>-[<->>+<-[<+>-[<->>+<-[<+>-[<->>+<-[<+>
      -[<->>+<-[<+>-[<->>+<-[<+>-]]]]]]]]]]]]]]]]]]]>[<+>-]<+<[<+++
      +++++++>-]<]>>[<+>->>]<<[>+>+<<-]>[<+>-]+>[<->[-]]<[-<<-]<<[<
      <]]++++++[>+++++++<-]>++.------------.[-]>[>>]<<[+++++[>+++++
      +++<-]>.<++++++[>--------<-]+<<]+<]>[<+>-]<]>>>[>>]<<[>[-]<-<
      <]++++++++++.[-]<<<[<<]>>>+<[->[<+>-[<+>-[<+>-[<+>-[<+>-[<+>-
      [<+>-[<+>-[<+>-[<[-]>>[-]+>+<<-]]]]]]]]]]<[>+<-]+>>]<<[<<]>>]""", Map())
*/ 

}