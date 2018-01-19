import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.io.StdIn.readInt
// A Small Compiler for the WHILE Language
// (it does not use a parser and lexer)

abstract class Parser[I <% Seq[_], T] {
  def parse(ts: I): Set[(T, I)]

  def parse_all(ts: I) : Set[T] =
    for ((head, tail) <- parse(ts); if (tail.isEmpty)) yield head
}

class SeqParser[I <% Seq[_], T, S](p: => Parser[I, T], q: => Parser[I, S]) extends Parser[I, (T, S)] {
  def parse(sb: I) =
    for ((head1, tail1) <- p.parse(sb);
         (head2, tail2) <- q.parse(tail1)) yield ((head1, head2), tail2)
}

class AltParser[I <% Seq[_], T](p: => Parser[I, T], q: => Parser[I, T]) extends Parser[I, T] {
  def parse(sb: I) = p.parse(sb) ++ q.parse(sb)
}

class FunParser[I <% Seq[_], T, S](p: => Parser[I, T], f: T => S) extends Parser[I, S] {
  def parse(sb: I) =
    for ((head, tail) <- p.parse(sb)) yield (f(head), tail)
}

type Token = (String, String)

case class StringParser(s: String) extends Parser[List[Token], String] {
  def parse(sb: List[Token]) = {
    if (sb.nonEmpty && sb.head._2 == s) Set((sb.head._2, sb.tail))
    else Set()
  }
}

case object AllStringsParser extends Parser[List[Token], String] {
  def parse(sb: List[Token]) = {
    if (sb.nonEmpty && sb.head._1 == "str") Set((sb.head._2, sb.tail))
    else Set()
  }
}

case object NumParser extends Parser[List[Token], Int] {
  def parse(sb: List[Token]) = {
    if (sb.nonEmpty && sb.head._1 == "n") Set((sb.head._2.toInt, sb.tail))
    else Set()
  }
}

case object IdParser extends Parser[List[Token], String] {
  def parse(sb: List[Token]) = {
    if (sb.nonEmpty && sb.head._1 == "i") Set((sb.head._2, sb.tail))
    else Set()
  }
}

implicit def string2parser(s : String) = StringParser(s)

implicit def ParserOps[I<% Seq[_], T](p: Parser[I, T]) = new {
  def || (q : => Parser[I, T]) = new AltParser[I, T](p, q)
  def ==>[S] (f: => T => S) = new FunParser[I, T, S](p, f)
  def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
}

implicit def StringOps(s: String) = new {
  def || (q : => Parser[List[Token], String]) = new AltParser[List[Token], String](s, q)
  def || (r: String) = new AltParser[List[Token], String](s, r)
  def ==>[S] (f: => String => S) = new FunParser[List[Token], String, S](s, f)
  def ~[S](q : => Parser[List[Token], S]) =
    new SeqParser[List[Token], String, S](s, q)
  def ~ (r: String) =
    new SeqParser[List[Token], String, String](s, r)
}

// the abstract syntax trees
abstract class Stmt
abstract class AExp
abstract class BExp
type Block = List[Stmt]

// statements
case object Skip extends Stmt
case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
case class While(b: BExp, bl: Block) extends Stmt
case class For(a1: Stmt, a2: AExp, bl: Block) extends Stmt
case class Assign(s: String, a: AExp) extends Stmt
case class Write(s: String) extends Stmt
case class Writes(s: String) extends Stmt
case class Read(s: String) extends Stmt

// arithmetic expressions
case class Var(s: String) extends AExp
case class Num(i: Int) extends AExp
case class Aop(o: String, a1: AExp, a2: AExp) extends AExp

// boolean expressions
case object True extends BExp
case object False extends BExp
case class Bop(o: String, a1: AExp, a2: AExp) extends BExp

lazy val AExp: Parser[List[Token], AExp] =
  ((Te ~ "+" ~ AExp) ==> { case ((x, y), z) => Aop("+", x, z):AExp } ||
   (Te ~ "-" ~ AExp) ==> { case ((x, y), z) => Aop("-", x, z):AExp } || Te)
lazy val Te: Parser[List[Token], AExp] =
  (Fa ~ "*" ~ Te) ==> { case ((x, y), z) => Aop("*", x, z):AExp } || Fa
lazy val Fa: Parser[List[Token], AExp] =
  (("(" ~ AExp ~ ")") ==> { case ((x, y), z) => y } ||
   IdParser ==> Var ||
   NumParser ==> Num)

// boolean expressions
lazy val BExp: Parser[List[Token], BExp] =
  ((AExp ~ "=" ~ AExp) ==> { case ((x, y), z) => Bop("=", x, z):BExp } ||
   (AExp ~ "!=" ~ AExp) ==> { case ((x, y), z) => Bop("!=", x, z):BExp } ||
   (AExp ~ "<" ~ AExp) ==> { case ((x, y), z) => Bop("<", x, z):BExp } ||
   (AExp ~ ">" ~ AExp) ==> { case ((x, y), z) => Bop(">", x, z):BExp } ||
   ("true" ==> ((_) => True:BExp )) ||
   ("false" ==> ((_) => False:BExp )) ||
   ("(" ~ BExp ~ ")") ==> { case ((x, y), z) => y})

// statement
lazy val Stmt: Parser[List[Token], Stmt] =
  (("skip" ==> ((_) => Skip: Stmt)) ||
   (IdParser ~ ":=" ~ AExp) ==> { case ((x, y), z) => Assign(x, z): Stmt } ||
   ("if" ~ BExp ~ "then" ~ Block ~ "else" ~ Block) ==>
    { case (((((x,y),z),u),v),w) => If(y, u, w): Stmt } ||
   ("for" ~ IdParser ~ ":=" ~ AExp ~ "upto" ~ AExp ~ "do" ~ Block) ==> { case (((((((x,y),z),u),v),w),q),r) => For(Assign(y, u), w, r) } ||
   ("while" ~ BExp ~ "do" ~ Block) ==> { case (((x, y), z), w) => While(y, w) } ||
   ("read" ~ IdParser) ==> { case (x,y) => Read(y)} ||
   ("write" ~ IdParser) ==> { case (x,y) => Write(y)} ||
   ("write" ~ AllStringsParser) ==> { case (x,y) => Writes(y)})

// complex statements
lazy val Stmts: Parser[List[Token], Block] =
  (Stmt ~ ";" ~ Stmts) ==> { case ((x, y), z) => x :: z : Block } ||
  (Stmt ==> ((s) => List(s) : Block))

// blocks in curly braces
lazy val Block: Parser[List[Token], Block] =
  (("{" ~ Stmts ~ "}") ==> { case ((x, y), z) => y} ||
   (Stmt ==> ((s) => List(s))))

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp
case class RECD(x: String, r: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp
case class PLUS(r: Rexp) extends Rexp
case class OPT(r: Rexp) extends Rexp
case class CSET(cs: Set[Char]) extends Rexp

abstract class Val
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(v1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class Stars(vs: List[Val]) extends Val
case class Rec(x: String, v: Val) extends Val

// some convenience for typing in regular expressions
def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}
implicit def string2rexp(s : String) : Rexp = charlist2rexp(s.toList)

implicit def RexpOps(r: Rexp) = new {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ? = OPT(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

implicit def stringOps(s: String) = new {
  def | (r: Rexp) = ALT(s, r)
  def | (r: String) = ALT(s, r)
  def % = STAR(s)
  def ? = OPT(s)
  def ~ (r: Rexp) = SEQ(s, r)
  def ~ (r: String) = SEQ(s, r)
  def $ (r: Rexp) = RECD(s, r)
}

// nullable function: tests whether the regular
// expression can recognise the empty string
def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case RECD(_, r1) => nullable(r1)
  case CSET(_) => false
  case NTIMES(r, i) => if (i == 0) true else nullable(r)
  case PLUS(r) => nullable(r)
  case OPT(r) => true
}

// derivative of a regular expression w.r.t. a character
def der (c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) =>
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r) => SEQ(der(c, r), STAR(r))
  case RECD(_, r1) => der(c, r1)
  case CSET(cs) => if (cs.contains(c)) ONE else ZERO
  case NTIMES(r, i) =>
    if (i == 0) ZERO else SEQ(der(c, r), NTIMES(r, i - 1))
  case PLUS(r) => SEQ(der(c, r), STAR(r))
  case OPT(r) => ALT(ZERO, der(c, r))
}

// derivative w.r.t. a string (iterates der)
def ders (s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::s => ders(s, der(c, r))
}

// extracts a string from value
def flatten(v: Val) : String = v match {
  case Empty => ""
  case Chr(c) => c.toString
  case Left(v) => flatten(v)
  case Right(v) => flatten(v)
  case Sequ(v1, v2) => flatten(v1) + flatten(v2)
  case Stars(vs) => vs.map(flatten).mkString
  case Rec(_, v) => flatten(v)
}

// extracts an environment from a value
def env(v: Val) : List[(String, String)] = v match {
  case Empty => Nil
  case Chr(c) => Nil
  case Left(v) => env(v)
  case Right(v) => env(v)
  case Sequ(v1, v2) => env(v1) ::: env(v2)
  case Stars(vs) => vs.flatMap(env)
  case Rec(x, v) => (x, flatten(v))::env(v)
}

// injection part
def mkeps(r: Rexp) : Val = r match {
  case ONE => Empty
  case ALT(r1, r2) =>
    if (nullable(r1)) Left(mkeps(r1)) else Right(mkeps(r2))
  case SEQ(r1, r2) => Sequ(mkeps(r1), mkeps(r2))
  case STAR(r) => Stars(Nil)
  case RECD(x, r) => Rec(x, mkeps(r))
  case NTIMES(r, n) => Stars((for (i <- 0 until n) yield mkeps(r)).toList)
  case PLUS(r) => Stars(List(mkeps(r)))
  case OPT(r) => Left(Empty)
}


def inj(r: Rexp, c: Char, v: Val) : Val = (r, v) match {
  case (STAR(r), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1)::vs)
  case (SEQ(r1, r2), Sequ(v1, v2)) => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Left(Sequ(v1, v2))) => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Right(v2)) => Sequ(mkeps(r1), inj(r2, c, v2))
  case (ALT(r1, r2), Left(v1)) => Left(inj(r1, c, v1))
  case (ALT(r1, r2), Right(v2)) => Right(inj(r2, c, v2))
  case (CHAR(d), Empty) => Chr(c)
  case (RECD(x, r1), _) => Rec(x, inj(r1, c, v))
  case (NTIMES(r, n), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1)::vs)
  case (PLUS(r), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1)::vs)
  case (OPT(r), Left(v1)) => Left(inj(r, c, v1))
  case (OPT(r), Right(v2)) => Right(inj(r, c, v2))
  case (CSET(s), _) => Chr(c)
}

// main lexing function (produces a value)
def lex(r: Rexp, s: List[Char]) : Val = s match {
  case Nil => if (nullable(r)) mkeps(r)
              else throw new Exception("Not matched")
  case c::cs => inj(r, c, lex(der(c, r), cs))
}

def lexing(r: Rexp, s: String) : Val = lex(r, s.toList)


// some "rectification" functions for simplification
def F_ID(v: Val): Val = v
def F_RIGHT(f: Val => Val) = (v:Val) => Right(f(v))
def F_LEFT(f: Val => Val) = (v:Val) => Left(f(v))
def F_ALT(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
  case Right(v) => Right(f2(v))
  case Left(v) => Left(f1(v))
}
def F_SEQ(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
  case Sequ(v1, v2) => Sequ(f1(v1), f2(v2))
}
def F_SEQ_Empty1(f1: Val => Val, f2: Val => Val) =
  (v:Val) => Sequ(f1(Empty), f2(v))
def F_SEQ_Empty2(f1: Val => Val, f2: Val => Val) =
  (v:Val) => Sequ(f1(v), f2(Empty))
def F_RECD(f: Val => Val) = (v:Val) => v match {
  case Rec(x, v) => Rec(x, f(v))
}
def F_ERROR(v: Val): Val = throw new Exception("error")

// simplification of regular expressions returning also an
// rectification function; no simplification under STAR
def simp(r: Rexp): (Rexp, Val => Val) = r match {
  case ALT(r1, r2) => {
    val (r1s, f1s) = simp(r1)
    val (r2s, f2s) = simp(r2)
    (r1s, r2s) match {
      case (ZERO, _) => (r2s, F_RIGHT(f2s))
      case (_, ZERO) => (r1s, F_LEFT(f1s))
      case _ => if (r1s == r2s) (r1s, F_LEFT(f1s))
                else (ALT (r1s, r2s), F_ALT(f1s, f2s))
    }
  }
  case SEQ(r1, r2) => {
    val (r1s, f1s) = simp(r1)
    val (r2s, f2s) = simp(r2)
    (r1s, r2s) match {
      case (ZERO, _) => (ZERO, F_ERROR)
      case (_, ZERO) => (ZERO, F_ERROR)
      case (ONE, _) => (r2s, F_SEQ_Empty1(f1s, f2s))
      case (_, ONE) => (r1s, F_SEQ_Empty2(f1s, f2s))
      case _ => (SEQ(r1s,r2s), F_SEQ(f1s, f2s))
    }
  }
  case RECD(x, r1) => {
    val (r1s, f1s) = simp(r1)
    (RECD(x, r1s), F_RECD(f1s))
  }
  case r => (r, F_ID)
}

def lex_simp(r: Rexp, s: List[Char]) : Val = s match {
  case Nil => if (nullable(r)) mkeps(r) else throw new Exception("Not matched")
  case c::cs => {
    val (r_simp, f_simp) = simp(der(c, r))
    inj(r, c, f_simp(lex_simp(r_simp, cs)))
  }
}

def lexing_simp(r: Rexp, s: String) : Val = lex_simp(r, s.toList)

// Lexing Rules for a Small While Language

val SYM = CSET(('a' to 'z').toSet ++ ('A' to 'Z').toSet)
val DIGIT = CSET(('0' to '9').toSet)
val ID = SYM ~ ("_" | SYM | DIGIT).%
val DIGIT2 = CSET(('1' to '9').toSet)
val NUM = DIGIT | ( DIGIT2 ~ DIGIT.% )
val KEYWORD : Rexp = "while" | "if" | "then" | "else" | "do" | "for" | "to" | "true" | "false" | "read" | "write" | "skip"
val SEMI: Rexp = ";"
val OP: Rexp = ":=" | "==" | "-" | "+" | "*" | "!=" | "<" | ">" | "&&" | "||" | "%" | "/"
val WHITESPACE = (" " | "\n" | "\t") ~ (" " | "\n" | "\t").%
val PAREN: Rexp = ")" | "{" | "}" | "("
val STRING: Rexp = "\"" ~ SYM.% ~ "\""


val WHILE_REGS = (("k" $ KEYWORD) |
                  ("i" $ ID) |
                  ("o" $ OP) |
                  ("n" $ NUM) |
                  ("s" $ SEMI) |
                  ("str" $ STRING) |
                  ("p" $ PAREN) |
                  ("w" $ WHITESPACE)).%

def time_needed[T](code: => T) = {
   val start = System.nanoTime()
   code
   val end = System.nanoTime()
   println("Time taken: " + (end - start)/(1.0e9) + " seconds.")
}

val progFib = """
write "Fib";
read n;
minus1 := 0;
minus2 := 1;
while n > 0 do {
  temp := minus2;
  minus2 := minus1 + minus2;
  minus1 := temp;
  n := n - 1
};
write "Result";
write minus2
"""

val progFact = """
write "Fact";
read n;
a := 1;
while n > 1 do {
  a := a * n;
  n := n - 1
};
write "Result";
write a
"""

val progFor = """
for i := 2 upto 4 do {
write i
}
"""


// compiler headers needed for the JVM
// (contains an init method, as well as methods for read and write)
val beginning = """
.class public XXX.XXX
.super java/lang/Object

.method public <init>()V
   aload_0
   invokenonvirtual java/lang/Object/<init>()V
   return
.end method

.method public static write(I)V
    .limit locals 1
    .limit stack 2
    getstatic java/lang/System/out Ljava/io/PrintStream;
    iload 0
    invokevirtual java/io/PrintStream/println(I)V
    return
.end method

.method public static writes(Ljava/lang/String;)V
    .limit stack 2
    .limit locals 1
    getstatic java/lang/System/out Ljava/io/PrintStream;
    aload 0
    invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
    return
.end method

.method public static read()I
    .limit locals 10
    .limit stack 10

    ldc 0
    istore 1  ; this will hold our final integer
Label1:
    getstatic java/lang/System/in Ljava/io/InputStream;
    invokevirtual java/io/InputStream/read()I
    istore 2
    iload 2
    ldc 10   ; the newline delimiter
    isub
    ifeq Label2
    iload 2
    ldc 32   ; the space delimiter
    isub
    ifeq Label2

    iload 2
    ldc 48   ; we have our digit in ASCII, have to subtract it from 48
    isub
    ldc 10
    iload 1
    imul
    iadd
    istore 1
    goto Label1
Label2:
    ;when we come here we have our integer computed in local variable 1
    iload 1
    ireturn
.end method

.method public static main([Ljava/lang/String;)V
   .limit locals 200
   .limit stack 200

"""

val ending = """

   return

.end method
"""


// for generating new labels
var counter = -1

def Fresh(x: String) = {
  counter += 1
  x ++ "_" ++ counter.toString()
}

// environments and instructions
type Env = Map[String, String]
type Instrs = List[String]

// arithmetic expression compilation
def compile_aexp(a: AExp, env : Env) : Instrs = a match {
  case Num(i) => List("ldc " + i.toString + "\n")
  case Var(s) => List("iload " + env(s) + "\n")
  case Aop("+", a1, a2) =>
    compile_aexp(a1, env) ++
    compile_aexp(a2, env) ++ List("iadd\n")
  case Aop("-", a1, a2) =>
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ List("isub\n")
  case Aop("*", a1, a2) =>
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ List("imul\n")
}

// boolean expression compilation
def compile_bexp(b: BExp, env : Env, jmp: String) : Instrs = b match {
  case True => Nil
  case False => List("goto " + jmp + "\n")
  case Bop("=", a1, a2) =>
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++
    List("if_icmpne " + jmp + "\n")
  case Bop("!=", a1, a2) =>
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++
    List("if_icmpeq " + jmp + "\n")
  case Bop("<", a1, a2) =>
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++
    List("if_icmpge " + jmp + "\n")
  case Bop(">", a1, a2) =>
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++
    List("if_icmple " + jmp + "\n")
  case Bop("<=", a1, a2) =>
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++
    List("if_icmpgt " + jmp + "\n")
}

// statement compilation
def compile_stmt(s: Stmt, env: Env) : (Instrs, Env) = s match {
  case Skip => (Nil, env)
  case Assign(x, a) => {
    val index = if (env.isDefinedAt(x)) env(x) else
                    env.keys.size.toString
    (compile_aexp(a, env) ++
     List("istore " + index + "\n"), env + (x -> index))
  }
  case If(b, bl1, bl2) => {
    val if_else = Fresh("If_else")
    val if_end = Fresh("If_end")
    val (instrs1, env1) = compile_block(bl1, env)
    val (instrs2, env2) = compile_block(bl2, env1)
    (compile_bexp(b, env, if_else) ++
     instrs1 ++
     List("goto " + if_end + "\n") ++
     List("\n" + if_else + ":\n\n") ++
     instrs2 ++
     List("\n" + if_end + ":\n\n"), env2)
  }
  case While(b, bl) => {
    val loop_begin = Fresh("Loop_begin")
    val loop_end = Fresh("Loop_end")
    val (instrs1, env1) = compile_block(bl, env)
    (List("\n" + loop_begin + ":\n\n") ++
     compile_bexp(b, env, loop_end) ++
     instrs1 ++
     List("goto " + loop_begin + "\n") ++
     List("\n" + loop_end + ":\n\n"), env1)
  }
  case For(a1, a2, bl) => {
    val Assign(i, a) = a1
    val (instrs1, env1) =  compile_stmt(a1, env)
    val (instrs2, env2) = compile_stmt(While(Bop("<=", Var(i), a2), bl ++ List(Assign(i, Aop("+", Var(i), Num(1))))), env1)

    (instrs1 ++ instrs2, env2)
  }
  // for variables, calling java's integer write
  case Write(x) =>
    (List("iload " + env(x) + "\n" +
           "invokestatic XXX/XXX/write(I)V\n"), env)
  case Writes(x) =>
  (List("ldc " + x + "\n" +
           "invokestatic XXX/XXX/writes(Ljava/lang/String;)V\n"), env)
  case Read(x) => {
    val index = if (env.isDefinedAt(x)) env(x) else
                    env.keys.size.toString
    (List("invokestatic XXX/XXX/read()I\n" +
          "istore " + index + "\n"), env + (x -> index))
  }
}

// compilation of a block (i.e. list of instructions)
def compile_block(bl: Block, env: Env) : (Instrs, Env) = bl match {
  case Nil => (Nil, env)
  case s::bl => {
    val (instrs1, env1) = compile_stmt(s, env)
    val (instrs2, env2) = compile_block(bl, env1)
    (instrs1 ++ instrs2, env2)
  }
}

// main compilation function for blocks
def compile(bl: Block, class_name: String) : String = {
  val instructions = compile_block(bl, Map.empty)._1
  (beginning ++ instructions.mkString ++ ending).replaceAllLiterally("XXX", class_name)
}

import scala.util._
import scala.sys.process._
import scala.io

def compile_tofile(bl: Block, class_name: String) = {
  val output = compile(bl, class_name)
  val fw = new java.io.FileWriter(class_name + ".j") //creates assembler file for Jasmin
  fw.write(output)
  fw.close()
}

def compile_all(bl: Block, class_name: String) : Unit = {
  compile_tofile(bl, class_name)
  println("compiled ")
  val test = ("java -jar jvm/jasmin-2.4/jasmin.jar " + class_name + ".j").!! //translates assembler file into byte code
  println("assembled ")
}



var tokensList = env(lexing_simp(WHILE_REGS, progFact)).filterNot{ case (k, w) => k == "w"}
compile_all(Stmts.parse_all(tokensList).head, "fact")
