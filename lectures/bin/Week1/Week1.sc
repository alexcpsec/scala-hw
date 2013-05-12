package Week1

object Week1 {

  def abs(x : Double) = if (x < 0) -x else x      //> abs: (x: Double)Double
  
		
	def sqrt(x: Double) = {
		def sqrtIter(guess: Double, x:Double): Double =
			if (isGoodEnough(guess, x)) guess
			else sqrtIter(improve(guess, x), x)
			
		def isGoodEnough(guess: Double, x: Double) =
			abs(guess * guess - x) < (0.001 * x)
			
		def improve(guess: Double, x: Double) =
			(guess + x / guess) / 2
	
		sqrtIter(1.0, x)
	}                                         //> sqrt: (x: Double)Double
	
	
	sqrt(2)                                   //> res0: Double = 1.4142156862745097
	sqrt(0.001)                               //> res1: Double = 0.03162278245070105
	sqrt(0.1e-20)                             //> res2: Double = 3.1633394544890125E-11
	sqrt(1e20)                                //> res3: Double = 1.0000021484861237E10
	sqrt(1e50)                                //> res4: Double = 1.0000003807575104E25
}