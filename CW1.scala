abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp
case class ONEORMORETIMES(r: Rexp) extends Rexp
case class OPT(r: Rexp) extends Rexp
case class ZEROORMOREBUTNOMORETHANM(r: Rexp, m: Int) extends Rexp
case class ATLEASTN(r: Rexp, n: Int) extends Rexp
case class NMTIMES(r: Rexp, n: Int, m: Int) extends Rexp
case class NOTR(r: Rexp) extends Rexp
case class CFUN(f: Char => Boolean) extends Rexp

def CHAR(c: Char) = CFUN(c1 => c == c1)
def CSET(cs: Set[Char]) = CFUN(c1 => cs.contains(c1))
def ALL() = CFUN(_ => true)

// nullable function: tests whether the regular
// expression can recognise the empty string
def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  //case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case NTIMES(r, i) => if (i == 0) true else nullable(r)
  //case CSET(_) => false
  case ONEORMORETIMES(r) => nullable(r)
  case OPT(r) => nullable(ALT(r , ONE)) //should be just true
  case ATLEASTN(r, n) => if (n == 0) nullable(STAR(r)) else nullable(r) // should be if (n == 0) true else nullable(r)
  case ZEROORMOREBUTNOMORETHANM(r: Rexp, m: Int) => true
  case NMTIMES(r, n, m) => if (m == 0) true else nullable(ALT(NTIMES(r,n), NTIMES(r,m))) //should be if (m == 0) then true else nullable(r)
  case NOTR(r) => !nullable(r)
  case CFUN(_) => false
}

// derivative of a regular expression w.r.t. a character
def der (c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  //case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) =>
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r1) => SEQ(der(c, r1), STAR(r1))
  case NTIMES(r, i) =>
    if (i == 0) ZERO else SEQ(der(c, r), NTIMES(r, i - 1))
  //case CSET(cs) => if (cs.contains(c)) ONE else ZERO
  case ONEORMORETIMES(r) => SEQ(der(c, r), STAR(r)) // should be der (c, SEQ(r, STAR(r)))
  case OPT(r) => ALT(der(c, r), der(c, ONE)) // der c ONE = ZERO so should be ALT(der(c, r), ZERO)
  case ATLEASTN(r, n) => SEQ(der(c, NTIMES(r, n)), STAR(r)) // should be der c(SEQ(r{n}, STAR(r)), as r{n} == SEQ(r{n}, STAR(r))
  case ZEROORMOREBUTNOMORETHANM(r1, i) =>
    if (i == 0) ZERO else SEQ(der(c, r1), ZEROORMOREBUTNOMORETHANM(r1, i - 1))
  case NMTIMES(r, n, m) =>
    if (n == m) der(c, NTIMES(r, n))
    else if (n == 0) der(c, ZEROORMOREBUTNOMORETHANM(r, m))
    else SEQ(der(c, r), NMTIMES(r, n - 1, m - 1))
  case NOTR(r) => NOTR(der(c, r))
  case CFUN(f) => if (f(c)) ONE else ZERO
}

def simp(r: Rexp) : Rexp = r match {
  case ALT(r1, r2) => (simp(r1), simp(r2)) match {
    case (ZERO, r2s) => r2s
    case (r1s, ZERO) => r1s
    case (r1s, r2s) => if (r1s == r2s) r1s else ALT (r1s, r2s)
  }
  case SEQ(r1, r2) =>  (simp(r1), simp(r2)) match {
    case (ZERO, _) => ZERO
    case (_, ZERO) => ZERO
    case (ONE, r2s) => r2s
    case (r1s, ONE) => r1s
    case (r1s, r2s) => SEQ(r1s, r2s)
  }
  case r => r
}


// derivative w.r.t. a string (iterates der)
def ders (s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::s => ders(s, simp(der(c, r)))
}

def matcher(r: Rexp, s: String) : Boolean = nullable(ders(s.toList, r))


// Q4)
// ([a-z0-9_ .−]+) · @ · ([a-z0-9 .−]+) · . · ([a-z .]{2,6})
val alphabetSet = ('a' to 'z').toSet
val digitsSet = ('0' to '9').toSet
val alphabetDigitsSet = alphabetSet ++ digitsSet
val firstRexp = ONEORMORETIMES(CSET(alphabetDigitsSet ++ Set('_', '.', '-')))
val secondRexp = ONEORMORETIMES(CSET(alphabetDigitsSet ++ Set('.', '-')))
val thirdRexp = NMTIMES(CSET(alphabetSet ++ Set('.')), 2, 6)
val emailRexp = SEQ(SEQ(SEQ(SEQ(firstRexp, CHAR('@')), secondRexp), CHAR('.')), thirdRexp)
