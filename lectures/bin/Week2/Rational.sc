package Week2

object Rational {
	val x = new Rational(1,3)                 //> x  : Week2.Rational = 1/3
	val y = new Rational(5,7)                 //> y  : Week2.Rational = 5/7
	val z = new Rational(3,2)                 //> z  : Week2.Rational = 3/2
	
	x.sub(y).sub(z)                           //> res0: Week2.Rational = -79/42
	
	

}

class Rational(x: Int, y: Int) {
	def numer = x
	def denom = y

	def add(that: Rational) =
		new Rational(numer * that.denom + that.numer * denom,
								 denom * that.denom)
								 
	override def toString = numer + "/" + denom

	def neg = new Rational(-numer,denom)

	def sub(that: Rational) =	add(that.neg)
	
}