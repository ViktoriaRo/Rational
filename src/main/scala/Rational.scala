/**
  * Answer to the questions:
  *
  * Where is the primary constructor of the class?
  * the primary constructor come right after the class name : class  Rational(n: Int, d: Int)
  *
  * Why there is no “default” constructor?
  * there is no “default” constructor because the class's body is the primary constructor
  *
  * What are g, numer and denom for?
  * g is equal to the gcd function, which calculates the greatest common divisor that will help us to
  * simplify the initial values. For example if n=100, d=200 then in our case the rational number will be 1/2,
  * because numer = n/g which means 100/100=1 and denom = d/g which means 200/100=2. So we need g, numer and denom
  * for simplifying our initial number.
  *
  * Why toString method defined with override?
  * because this method exists and we use override it for our specific case
  *
  * Why do we need second versions of +, -,*
  * and /?
  * we need the second version of +, -,* and / because here we calculate with int numbers
  * and they need different implementation
  *
  * When does require(d!=0) gets executed?
  * this require(d!=0) executes when d==0, and it gets executed after class' constructor
  */

class Rational(n: Int, d: Int) {
  require(d != 0)
  private val g = gcd(n.abs, d.abs)
  val numer = n / g
  val denom = d / g

  def this(n: Int) = this(n, 1)

  def + (that: Rational): Rational = new Rational(
    numer*that.denom+that.numer*denom,denom*that.denom)
  def + (i: Int): Rational = new Rational(numer+i*denom,denom)

  def - (that: Rational): Rational = new Rational(
    numer*that.denom-that.numer*denom,denom*that.denom)
  def - (i: Int): Rational = new Rational(numer-i*denom,denom)

  def * (that: Rational): Rational = new Rational(
    numer*that.numer,denom*that.denom)
  def * (i: Int): Rational = new Rational(numer*i,denom )

  def / (that: Rational): Rational = new Rational(
    numer*that.denom,denom*that.numer)
  def / (i: Int): Rational = new Rational(numer, denom*i)

  override def toString: String = numer + "/" + denom
  private def gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b,a%b)
}

  object Main{
  def main(args: Array[String]): Unit = {
    print("2/3 - 1 = ")
    println(new Rational(2,3) - 1)
    print("1/3 + 1/3 = ")
    println(new Rational(1,3) + new Rational(1,3))
    print("100/300 * 1/3 = ")
    println(new Rational(100,300) * new Rational(1,3))
    print("6/3 / 33/55 = ")
    println(new Rational(6,3) / new Rational(33,55))
    val x = new Rational(25,125)
    println("----------------------")
    println("x = 25/125")
    print("x + 5 = ")
    println(x + 5)
    print("x - 3 = ")
    println(x - 3)
    print("x * 3 = ")
    println(x * 3)
    print("x / 25 = ")
    println(x / 25)
  }
}