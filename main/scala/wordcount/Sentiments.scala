package wordcount
import org.jfree.data.xy.XYSeries
import org.jfree.data.xy.XYSeriesCollection
import org.jfree.chart.renderer.xy.XYDotRenderer
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.JFreeChart
import org.jfree.ui.ApplicationFrame
import org.jfree.chart.ChartPanel
import org.jfree.chart.renderer.xy.XYSplineRenderer
import scala.io.Source
import wordcount.Processing


/**
 * @author hendrik
 */

class Sentiments (sentiFile:String){
 
  val sentiments:Map[String,Int]= getSentiments(sentiFile)
  
  val pro= new Processing()
  
/**********************************************************************************************
   *
   *                          Aufgabe 5
   *   
   *********************************************************************************************
  */

  def getDocumentGroupedByCounts(filename:String, wordCount:Int): List[(Int,List[String])]= {
 
    val url=getClass.getResource("/"+filename).getPath //get the file
                                            
    val lines = Source.fromFile(url).getLines.toList
     
    val res =   lines flatMap (X => pro.getWords(X)) filter(_ != "")
    
    res.grouped(wordCount).zipWithIndex.map(x => x._2+1 -> x._1).toList
    
  }
  
  
  def analyzeSentiments(l:List[(Int,List[String])]):List[(Int, Double, Double)]= {
    
     l.map(x => 
       
       ( x._1

    , x._2.map(x => sentiments.getOrElse(x,0) ).reduce(_+_).toDouble
    
    , x._2.map(x => if (sentiments.getOrElse(x,10) != 10 ) 1 else 0 ).reduce(_+_).toDouble )
    ).map{

      	case(a,b,c) if (b/c).isNaN => (a,b,c)
      	case(a,b,c) => (a,b/c,c)
      
      }  
  }

  
  /**********************************************************************************************
   *
   *                          Helper Functions
   *   
   *********************************************************************************************
  */
  
  def getSentiments(filename:String):Map[String,Int]={
        
    val url=getClass.getResource("/"+filename).getPath
    val src = scala.io.Source.fromFile(url)
    val iter = src.getLines()
    val result:Map[String,Int]= (for (row <- iter) yield {val seg= row.split("\t"); (seg(0) -> seg(1).toInt)}).toMap
    src.close()
    result
    
  }
  
  
  def createGraph(data:List[(Int,Double,Double)]):Unit={
    
    val series1:XYSeries = new XYSeries("Sentiment-Werte");
    for (el <- data){series1.add(el._1,el._2)}
    val series2:XYSeries  = new XYSeries("Relative Haeufigkeit der erkannten Worte");
    for (el <- data){series2.add(el._1,el._3)}
    
    val dataset1:XYSeriesCollection  = new XYSeriesCollection();
    dataset1.addSeries(series1);
    val dataset2:XYSeriesCollection  = new XYSeriesCollection();
    dataset2.addSeries(series2);
    
    val dot:XYDotRenderer = new XYDotRenderer();
    dot.setDotHeight(5);
    dot.setDotWidth(5);
    
    val spline:XYSplineRenderer = new XYSplineRenderer();
    spline.setPrecision(10);
    
    val x1ax:NumberAxis= new NumberAxis("Abschnitt");
    val y1ax:NumberAxis = new NumberAxis("Sentiment Werte");
    val x2ax:NumberAxis= new NumberAxis("Abschnitt");
    val y2ax:NumberAxis = new NumberAxis("Relative Haeufigfkeit");
    
    val plot1:XYPlot  = new XYPlot(dataset1,x1ax,y1ax, spline);
    val plot2:XYPlot  = new XYPlot(dataset2,x2ax,y2ax, dot);

    val chart1:JFreeChart  = new JFreeChart(plot1);
    val chart2:JFreeChart  = new JFreeChart(plot2);
    val frame1:ApplicationFrame = new ApplicationFrame("Sentiment-Analyse: Sentiment Werte"); 
    val frame2:ApplicationFrame = new ApplicationFrame("Sentiment-Analyse: Relative Häufigkeiten"); 
    val chartPanel1: ChartPanel = new ChartPanel(chart1);
    val chartPanel2: ChartPanel = new ChartPanel(chart2);
    
    frame1.setContentPane(chartPanel1);
    frame1.pack();
    frame1.setVisible(true);
    frame2.setContentPane(chartPanel2);
    frame2.pack();
    frame2.setVisible(true);
  }
}
