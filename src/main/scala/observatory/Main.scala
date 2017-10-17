package observatory

import Extraction._
import Manipulation._

object Main extends App {

  def temperatureColorScales = List(
    (60.0, Color(255, 255, 255)),
    (32.0, Color(255, 0  , 0)),
    (12.0, Color(255, 255,  0)),
    (0.0,  Color(0  , 255,  255)),
    (-15.0, Color(0  , 0, 255)),
    (-27.0, Color(255, 0,   255)),
    (-50.0, Color(33 , 0, 107)),
    (-60.0, Color(0  , 0, 0))
  )

  def deviationColorScales = List(
    (7.0, Color(0, 0, 0)),
    (4.0, Color(255, 0, 0)),
    (2.0, Color(255, 255, 0)),
    (0.0,  Color(255, 255, 255)),
    (-2.0, Color(0, 255, 255)),
    (-7.0, Color(0, 0, 255))
  )

  def generateTiles(fromYear: Int, toYear: Int): Unit = {
    val yearsData = for {
      year <- fromYear to toYear
    } yield locateTemperatures(year, "/stations.csv", f"/$year.csv")
  }

  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    yearlyData.par.foreach(yd => {
      val year = yd._1
      val data = yd._2
      (0 to 3).par.foreach(zoom => {
        for (
             row <- (0 until 1 << zoom);
             col <- (0 until 1 << zoom)
           ) {
             val (x, y) = (col, row)
             generateImage(year, zoom, x, y, data)
             // val filepath = f"target/temperatures/$year/$zoom/$x-$y.png"
             // image.output(new java.io.File(filepath))
           }
      })
    })
    // type Data = Iterable[(Location, Double)] 
  }

  val years = 1975 to 2015

  val records = Extraction.locateTemperatures(2021, "/mystations.csv", "/2021.csv")
  val temperatures = Extraction.locationYearlyAverageRecords(records)
  val image = Interaction.tile(temperatures, temperatureColorScales.toIterable, 8, 10, 10)
  // val grid = makeGrid(temperatures)
  // Visualization2.visualizeGrid(grid, temperatureColorScales, 1, 1, 0)
}
