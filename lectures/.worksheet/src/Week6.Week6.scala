package Week6



object Week6 {
  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]


  type Occurrences = List[(Char, Int)];import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(231); 


	val xs = Array(1,2,3,44);System.out.println("""xs  : Array[Int] = """ + $show(xs ));$skip(21); val res$0 = 
	xs map (x => x * 2);System.out.println("""res0: Array[Int] = """ + $show(res$0));$skip(25); 
	
	val s = "Hello World";System.out.println("""s  : String = """ + $show(s ));$skip(29); val res$1 = 
	
	s filter (c => c.isUpper);System.out.println("""res1: String = """ + $show(res$1));$skip(29); val res$2 = 
	
	s exists (c => c.isUpper);System.out.println("""res2: Boolean = """ + $show(res$2));$skip(27); val res$3 = 
	s forall (c => c.isUpper);System.out.println("""res3: Boolean = """ + $show(res$3));$skip(32); 

	val pairs = List(1,2,3) zip s;System.out.println("""pairs  : List[(Int, Char)] = """ + $show(pairs ));$skip(13); val res$4 = 
	pairs.unzip;System.out.println("""res4: (List[Int], List[Char]) = """ + $show(res$4));$skip(17); 

	val w = "aabb";System.out.println("""w  : String = """ + $show(w ));$skip(125); 
	
	val mix = for {
		(letter, nLet) <- w.toLowerCase().groupBy((c: Char) => c).toList.sorted
		} yield (letter, nLet.length);System.out.println("""mix  : List[(Char, Int)] = """ + $show(mix ));$skip(179); 
	
    def combChar(occur: Occurrences): List[Occurrences] =
      (for {
    	  (c, count) <- occur
    	  ii <- 0 to count
    	  if (ii > 0)
      } yield List((c, ii))).toList;System.out.println("""combChar: (occur: Week6.Week6.Occurrences)List[Week6.Week6.Occurrences]""");$skip(26); val res$5 = 
      
     combChar(mix);System.out.println("""res5: List[Week6.Week6.Occurrences] = """ + $show(res$5));$skip(514); 
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def combChar(occur: Occurrences): List[Occurrences] =
      (for {
    	  (c, count) <- occur
    	  ii <- 0 to count
    	  if (ii > 0)
      } yield List((c, ii))).toList
      
    if (occurrences == Nil) List(List())
    else { List() ::
      (for {
        split <- 1 to occurrences.length
        comb <- combChar(occurrences take split)
        rest <- combinations(occurrences drop split)
      } yield comb ++ rest).toList
  	}
  };System.out.println("""combinations: (occurrences: Week6.Week6.Occurrences)List[Week6.Week6.Occurrences]""");$skip(23); val res$6 = 
  
  combinations(mix);System.out.println("""res6: List[Week6.Week6.Occurrences] = """ + $show(res$6));$skip(45); 
      
val sentence = List("Linux", "rulez");System.out.println("""sentence  : List[String] = """ + $show(sentence ));$skip(159); 

  def wordOccurrences(w: Word): Occurrences = for {
		(letter, nLet) <- w.toLowerCase().groupBy((c: Char) => c).toList.sorted
		} yield (letter, nLet.length);System.out.println("""wordOccurrences: (w: Week6.Week6.Word)Week6.Week6.Occurrences""");$skip(168); 

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences =
    wordOccurrences( (s foldLeft "") (_ ++ _) );System.out.println("""sentenceOccurrences: (s: Week6.Week6.Sentence)Week6.Week6.Occurrences""");$skip(757); 



      

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    ((y foldLeft x.toMap)
    	((map: Map[Char,Int], occur: (Char, Int)) =>
    	  if (map.apply(occur._1) - occur._2 > 0)
    		  map.updated(occur._1, map.apply(occur._1) - occur._2)
          else
        	  map - occur._1
    	)).toList.sorted
    	
  };System.out.println("""subtract: (x: Week6.Week6.Occurrences, y: Week6.Week6.Occurrences)Week6.Week6.Occurrences""");$skip(40); 

val oo = sentenceOccurrences(sentence);System.out.println("""oo  : Week6.Week6.Occurrences = """ + $show(oo ));$skip(18); val res$7 = 

combinations(oo);System.out.println("""res7: List[Week6.Week6.Occurrences] = """ + $show(res$7))}

}
