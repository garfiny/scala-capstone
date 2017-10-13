package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math._
import Visualization._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    // val n = pow(2, zoom)
    // val lon_deg = x / n * 360.0 - 180.0
    // val lat_rad = atan(sinh(Pi * (1 - 2 * y / n)))
    // val lat_deg = lat_rad * 180.0 / Pi
    // Location(lat_deg, lon_deg)
    val latLon = Tile(x, y, zoom).toLatLon
    Location(latLon.lat, latLon.lon)
  }

  def locationToTile(location: Location, zoom: Int): (Int, Int) = {
    // val n = pow(2.0, zoom)
    // val lat_rad = toRadians(location.lat)
    // val xtile = rint(n * ((location.lon + 180.0) / 360.0)).asInstanceOf[Int]
    // val ytile = rint(n * (1.0 - (log(tan(lat_rad) + (1 / cos(lat_rad))) / Pi) / 2.0)).asInstanceOf[Int]
    // (xtile, ytile)
    val tile = LatLonPoint(location.lat, location.lon, zoom).toTile
    (tile.x, tile.y)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val location = tileLocation(zoom, x, y)
    val temperature = predictTemperature(temperatures, location)
    val interpolatedColor = interpolateColor(colors, temperature)
    // ========================
    // println("Tile coordinates (x, y, z): (" + x + "," + y + "," + zoom + ") === Location: " + location)
    // println("temperatures ===============")
    // temperatures.foreach(t => {
    //   println("Location: (" + t._1.lat + ", " + t._1.lon + ") == TEMP: " + t._2 )
    // })
    // println("Color scales: =============")
    // colors.foreach(c => println("Temp: " + c._1 + "  Color: " + c._2))
    // println("predicted =================")
    // println("TEMP: " + temperature + "  === Color: " + interpolatedColor)
    // ========================

    // val temperaturesMap = Map() ++ temperatures.map(t => (locationToTile(t._1, zoom) -> interpolateColor(colors, t._2)))
    val array = Array.ofDim[Pixel](256, 256)
    for (
         i <- 0 until 256; // y == row
         j <- 0 until 256  // x == column
       ) {
         val color = if (x == j && y == i) {
           interpolatedColor
         } else {
           val loc = tileLocation(zoom, j, i)
           val temp = predictTemperature(temperatures, loc)
           interpolateColor(colors, temp)
         }
         val pixel = Pixel(color.red, color.green, color.blue, 127)
         array(i)(j) = pixel
       }
    val image = Image(256, 256, array.flatten)
    image.output(new java.io.File("target/interactive_image.png"))
    image
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
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
}
