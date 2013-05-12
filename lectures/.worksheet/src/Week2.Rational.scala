package Week2

object Rational {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(59); 
	val x = new Rational(1,3);System.out.println("""x  : Week2.Rational = """ + $show(x ));$skip(27); 
	val y = new Rational(5,7);System.out.println("""y  : Week2.Rational = """ + $show(y ));$skip(27); 
	val z = new Rational(3,2);System.out.println("""z  : Week2.Rational = """ + $show(z ));$skip(19); val res$0 = 
	
	x.sub(y).sub(z);System.out.println("""res0: Week2.Rational = """ + $show(res$0))}
	
	

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
