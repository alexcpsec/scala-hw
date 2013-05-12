package Week6



object Week6 {
  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]


  type Occurrences = List[(Char, Int)]


	val xs = Array(1,2,3,44)                  //> xs  : Array[Int] = Array(1, 2, 3, 44)
	xs map (x => x * 2)                       //> res0: Array[Int] = Array(2, 4, 6, 88)
	
	val s = "Hello World"                     //> s  : String = Hello World
	
	s filter (c => c.isUpper)                 //> res1: String = HW
	
	s exists (c => c.isUpper)                 //> res2: Boolean = true
	s forall (c => c.isUpper)                 //> res3: Boolean = false

	val pairs = List(1,2,3) zip s             //> pairs  : List[(Int, Char)] = List((1,H), (2,e), (3,l))
	pairs.unzip                               //> res4: (List[Int], List[Char]) = (List(1, 2, 3),List(H, e, l))

	val w = "aabb"                            //> w  : String = aabb
	
	val mix = for {
		(letter, nLet) <- w.toLowerCase().groupBy((c: Char) => c).toList.sorted
		} yield (letter, nLet.length)     //> mix  : List[(Char, Int)] = List((a,2), (b,2))
	
    def combChar(occur: Occurrences): List[Occurrences] =
      (for {
    	  (c, count) <- occur
    	  ii <- 0 to count
    	  if (ii > 0)
      } yield List((c, ii))).toList               //> combChar: (occur: Week6.Week6.Occurrences)List[Week6.Week6.Occurrences]
      
     combChar(mix)                                //> res5: List[Week6.Week6.Occurrences] = List(List((a,1)), List((a,2)), List((b
                                                  //| ,1)), List((b,2)))
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
  }                                               //> combinations: (occurrences: Week6.Week6.Occurrences)List[Week6.Week6.Occurr
                                                  //| ences]
  
  combinations(mix)                               //> res6: List[Week6.Week6.Occurrences] = List(List(), List((a,1)), List((a,1),
                                                  //|  (b,1)), List((a,1), (b,2)), List((a,2)), List((a,2), (b,1)), List((a,2), (
                                                  //| b,2)), List((a,1)), List((a,2)), List((b,1)), List((b,2)))
      
val sentence = List("Linux", "rulez")             //> sentence  : List[String] = List(Linux, rulez)

  def wordOccurrences(w: Word): Occurrences = for {
		(letter, nLet) <- w.toLowerCase().groupBy((c: Char) => c).toList.sorted
		} yield (letter, nLet.length)     //> wordOccurrences: (w: Week6.Week6.Word)Week6.Week6.Occurrences

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences =
    wordOccurrences( (s foldLeft "") (_ ++ _) )   //> sentenceOccurrences: (s: Week6.Week6.Sentence)Week6.Week6.Occurrences



      

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
    	
  }                                               //> subtract: (x: Week6.Week6.Occurrences, y: Week6.Week6.Occurrences)Week6.Wee
                                                  //| k6.Occurrences

val oo = sentenceOccurrences(sentence)            //> oo  : Week6.Week6.Occurrences = List((e,1), (i,1), (l,2), (n,1), (r,1), (u,
                                                  //| 2), (x,1), (z,1))

combinations(oo)                                  //> res7: List[Week6.Week6.Occurrences] = List(List(), List((e,1)), List((e,1),
                                                  //|  (i,1)), List((e,1), (i,1), (l,1)), List((e,1), (i,1), (l,1), (n,1)), List(
                                                  //| (e,1), (i,1), (l,1), (n,1), (r,1)), List((e,1), (i,1), (l,1), (n,1), (r,1),
                                                  //|  (u,1)), List((e,1), (i,1), (l,1), (n,1), (r,1), (u,1), (x,1)), List((e,1),
                                                  //|  (i,1), (l,1), (n,1), (r,1), (u,1), (x,1), (z,1)), List((e,1), (i,1), (l,1)
                                                  //| , (n,1), (r,1), (u,1), (x,1)), List((e,1), (i,1), (l,1), (n,1), (r,1), (u,1
                                                  //| ), (z,1)), List((e,1), (i,1), (l,1), (n,1), (r,1), (u,2)), List((e,1), (i,1
                                                  //| ), (l,1), (n,1), (r,1), (u,2), (x,1)), List((e,1), (i,1), (l,1), (n,1), (r,
                                                  //| 1), (u,2), (x,1), (z,1)), List((e,1), (i,1), (l,1), (n,1), (r,1), (u,2), (x
                                                  //| ,1)), List((e,1), (i,1), (l,1), (n,1), (r,1), (u,2), (z,1)), List((e,1), (i
                                                  //| ,1), (l,1), (n,1), (r,1), (u,1)), List((e,1), (i,1), (l,1), (n,1), (r,1), (
                                                  //| u,1), (z,1)), List((e,1), (i,1), (l,1), (n,1), (r,1), (u,2)), List((e,1), (
                                                  //| i,1), (l,1), (n,1), (r,
                                                  //| Output exceeds cutoff limit.

}