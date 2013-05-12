package Week1

object Week1 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(75); 

  def abs(x : Double) = if (x < 0) -x else x;System.out.println("""abs: (x: Double)Double""");$skip(345); 
  
		
	def sqrt(x: Double) = {
		def sqrtIter(guess: Double, x:Double): Double =
			if (isGoodEnough(guess, x)) guess
			else sqrtIter(improve(guess, x), x)
			
		def isGoodEnough(guess: Double, x: Double) =
			abs(guess * guess - x) < (0.001 * x)
			
		def improve(guess: Double, x: Double) =
			(guess + x / guess) / 2
	
		sqrtIter(1.0, x)
	};System.out.println("""sqrt: (x: Double)Double""");$skip(13); val res$0 = 
	
	
	sqrt(2);System.out.println("""res0: Double = """ + $show(res$0));$skip(13); val res$1 = 
	sqrt(0.001);System.out.println("""res1: Double = """ + $show(res$1));$skip(15); val res$2 = 
	sqrt(0.1e-20);System.out.println("""res2: Double = """ + $show(res$2));$skip(12); val res$3 = 
	sqrt(1e20);System.out.println("""res3: Double = """ + $show(res$3));$skip(12); val res$4 = 
	sqrt(1e50);System.out.println("""res4: Double = """ + $show(res$4))}
}
