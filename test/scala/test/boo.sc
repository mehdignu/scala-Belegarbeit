package test
import scala.io.Source
import wordcount.Processing
import wordcount.Sentiments

object boo {

	val pro = new Processing()                //> pro  : wordcount.Processing = wordcount.Processing@5f4da5c3
	val sent = new Sentiments("AFINN-111.txt")//> sent  : wordcount.Sentiments = wordcount.Sentiments@4b9e13df

//val data= sentiAnalyse.analyzeSentiments(sentiAnalyse.getDocumentGroupedByCounts("MobyDickC1.txt",200))
  
  val data= sent.getDocumentGroupedByCounts("MobyDickC1.txt",4)
                                                  //> data  : List[(Int, List[String])] = List((1,List(chapter, loomings, call, me
                                                  //| )), (2,List(ishmael, some, years, ago)), (3,List(never, mind, how, long)), (
                                                  //| 4,List(precisely, having, little, or)), (5,List(no, money, in, my)), (6,List
                                                  //| (purse, and, nothing, particular)), (7,List(to, interest, me, on)), (8,List(
                                                  //| shore, i, thought, i)), (9,List(would, sail, about, a)), (10,List(little, an
                                                  //| d, see, the)), (11,List(watery, part, of, the)), (12,List(world, it, is, a))
                                                  //| , (13,List(way, i, have, of)), (14,List(driving, off, the, spleen)), (15,Lis
                                                  //| t(and, regulating, the, circulation)), (16,List(whenever, i, find, myself)),
                                                  //|  (17,List(growing, grim, about, the)), (18,List(mouth, whenever, it, is)), (
                                                  //| 19,List(a, damp, drizzly, november)), (20,List(in, my, soul, whenever)), (21
                                                  //| ,List(i, find, myself, involuntarily)), (22,List(pausing, before, coffin, wa
                                                  //| rehouses)), (23,List(and, bringing, up, the)), (24,List(rear, of, every, fun
                                                  //| eral)), (25,List(i, meet
                                                  //| Output exceeds cutoff limit.
val k = data.map(x => ( x._1

, x._2.map(x => sent.sentiments.getOrElse(x,0) ).reduce(_+_).toDouble

, x._2.map(x => if (sent.sentiments.getOrElse(x,10) != 10 ) 1 else 0 ).reduce(_+_).toDouble )

).map{

	case(a,b,c) if (b/c).isNaN => (a,b,c)
	case(a,b,c) => (a,b/c,c)

}                                                 //> k  : List[(Int, Double, Double)] = List((1,0.0,0.0), (2,0.0,0.0), (3,0.0,0.0
                                                  //| ), (4,0.0,0.0), (5,-1.0,1.0), (6,0.0,0.0), (7,1.0,1.0), (8,0.0,0.0), (9,0.0,
                                                  //| 0.0), (10,0.0,0.0), (11,0.0,0.0), (12,0.0,0.0), (13,0.0,0.0), (14,0.0,0.0), 
                                                  //| (15,0.0,0.0), (16,0.0,0.0), (17,1.0,1.0), (18,0.0,0.0), (19,0.0,0.0), (20,0.
                                                  //| 0,0.0), (21,0.0,0.0), (22,0.0,0.0), (23,0.0,0.0), (24,-1.0,1.0), (25,0.0,0.0
                                                  //| ), (26,0.0,0.0), (27,0.0,0.0), (28,0.0,0.0), (29,2.0,1.0), (30,-1.0,1.0), (3
                                                  //| 1,0.0,0.0), (32,0.0,0.0), (33,0.0,0.0), (34,0.0,0.0), (35,0.0,0.0), (36,0.0,
                                                  //| 0.0), (37,0.0,0.0), (38,0.0,0.0), (39,0.0,0.0), (40,0.0,0.0), (41,0.0,0.0), 
                                                  //| (42,0.0,0.0), (43,0.0,0.0), (44,0.0,0.0), (45,0.0,0.0), (46,0.0,0.0), (47,0.
                                                  //| 0,0.0), (48,0.0,0.0), (49,2.0,1.0), (50,0.0,0.0), (51,0.0,0.0), (52,0.0,0.0)
                                                  //| , (53,0.0,0.0), (54,0.0,0.0), (55,0.0,0.0), (56,0.0,0.0), (57,0.0,0.0), (58,
                                                  //| 0.0,0.0), (59,0.0,0.0), (60,0.0,0.0), (61,0.0,0.0), (62,2.0,1.0), (63,0.0,0.
                                                  //| 0), (64,0.0,0.0), (65,0.
                                                  //| Output exceeds cutoff limit.


        










           
                                                   
   

 }