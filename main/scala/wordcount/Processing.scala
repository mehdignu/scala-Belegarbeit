package wordcount
import common._
import mapreduce.BasicOperations
import scala.language.postfixOps

class Processing {
   
  /**********************************************************************************************
   *
   *                          Aufgabe 1
   *   
   *********************************************************************************************
  */
  def getWords(line:String):List[String]={
    /*
     * Extracts all words in a line
     * 
     * 1. Removes all characters which are not letters (A-Z or a-z)
     * 2. Shifts all words to lower case
     * 3. Extracts all words and put them into a list of strings
     */
    
      line replaceAll("[^A-Za-z]", " ") toLowerCase() trim() replaceAll(" +", " ") split " " toList


  } 
  
  def getAllWords(l:List[(Int,String)]):List[String]={
    
    /*
     * Extracts all Words from a List containing tupels consisting
     * of a line number and a string
     * The Words should be in the same order as they occur in the source document
     * 
     * Hint: Use the flatMap function
     */
    
     l flatMap (X => getWords(X._2)) filter(_ != "")
    
    
    
  }
  
  def countTheWords(l:List[String]):List[(String,Int)]={
 
    /*
     *  Gets a list of words and counts the occurences of the individual words
     */
    
        l groupBy(w => w) map(x => (x._1, x._2.count(t => true))) toList
        
  }
  
  /**********************************************************************************************
   *
   *                          Aufgabe 2
   *   
   *********************************************************************************************
  */
  
  def mapReduce[S,B,R](mapFun:(S=>B), 
      redFun:(R,B)=>R, 
      base:R,   
      l:List[S]):R =

  l.map(mapFun).foldLeft(base)(redFun)

  
  

  def countTheWordsMR(l: List[String]): List[(String, Int)] =
  
  mapReduce[String, (String, Int), Map[String, Int]](
  
    //map function which adds for each word an int of 1 as the initial state to pass it later to the reduce function 
    w => (w, 1),
  
  //t: tupel comes from the map function
  //m is the base map where the words will be counted
    (m, t) => m.get(t._1) match {
      
      case None => m ++ Map(t._1 -> 1)
      case Some(e) => m ++ Map(t._1 -> (e + 1))
  
    },
  
    Map[String, Int](),
  
    l
    
  ).toList                                        
  
  
  
  /**********************************************************************************************
   *
   *                          Aufgabe 3
   *   
   *********************************************************************************************
  */      
  
    def getAllWordsWithIndex(l:List[(Int,String)]):List[(Int,String)]= 
      
      l.flatMap(L =>
        getWords(L._2)
        .filter(_ != "")
        .map(w =>
              (L._1, w)
        )
      )
    
    /*
     * Extracts all Words from a List containing tupels consisting
     * of a line number and a string
     */
    
  
    def createInverseIndex(l: List[(Int, String)]): Map[String, List[Int]] = {
     
       l.foldLeft(Map[String, List[Int]]())( //base value
       
       (m, w) => //reduce function
     
      m.get(w._2) match {
      
        case Some(e) => m ++ Map(w._2 -> (e ++ List(w._1)))
        case None => m ++ Map(w._2 -> List(w._1))
      
      })
  
   }   
  
   def andConjunction(words:List[String], invInd:Map[String,List[Int]]):List[Int]={
     
       exists(words, invInd)
       .flatten
       .groupBy(w => w)
       .map(x => (x._1, x._2.count(t => true)))
       .filter(x => x._2 == words.length)
       .map(x => x._1)
           .toList
		 
   }
   
 
  def exists(words: List[String], l: Map[String, List[Int]]): List[List[Int]] = {
		 
		 	words.foldLeft(List[List[Int]]())((x, w) => l.get(w) match {
		 	
		 	case None => x
			case Some(elem) => l.get(w).head :: x
			
		})
		 	
		 
		  }     	 
	
   
   def orConjunction(words:List[String], invInd:Map[String, List[Int]]):List[Int]={
     
     exists(words, invInd).flatten.distinct
   }
}


object Processing{
  
  def getData(filename:String):List[(Int,String)]={

    val url= getClass.getResource("/"+filename).getPath
    val src = scala.io.Source.fromFile(url)
    val iter = src.getLines()
    var c = -1
    val result= (for (row <- iter) yield {c=c+1;(c,row)}).toList
    src.close()
    result
  }
}