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
    val latLon = Tile(x, y, zoom).toLatLon
    Location(latLon.lat, latLon.lon)
  }

  def locationToTile(location: Location, zoom: Int): (Int, Int) = {
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
    // val array = Array.ofDim[Pixel](256, 256)
    // val xpos = (pow(2, (9 - zoom)) * x).toInt
    // val ypos = (pow(2, (9 - zoom)) * y).toInt
    // for (
    //      i <- 0 until 256; // y == row
    //      j <- 0 until 256  // x == column
    //    ) {
    //      val color = if (xpos == j && ypos == i) {
    //        interpolatedColor
    //      } else {
    //        val loc = tileLocation(9, j + xpos, i + ypos)
    //        val temp = predictTemperature(temperatures, loc)
    //        interpolateColor(colors, temp)
    //      }
    //      val pixel = Pixel(color.red, color.green, color.blue, 127)
    //      array(i)(j) = pixel
    //    }
    // val image = Image(256, 256, array.flatten)
    // image.output(new java.io.File("target/interactive_image.png"))
    // image
    generateTile(temperatures, colors, interpolatedColor, zoom, x, y, 256)
  }

  def generateTile(temperatures: Iterable[(Location, Double)], 
    colors: Iterable[(Double, Color)],
    interpolatedColor: Color,
    zoom: Int, x: Int, y: Int, resolution: Int): Image = {
      val imageWidth = resolution
      val imageHeight = resolution

      val pixels = (0 until imageWidth * imageHeight).par.map(pos => {
        val xPos = ((pos % imageWidth).toDouble / imageWidth + x).toInt // column of image as fraction with offset x
        val yPos = ((pos / imageHeight).toDouble / imageHeight + y).toInt // row of image as fraction with offset y
        val color = interpolateColor(colors,
          predictTemperature( temperatures, tileLocation(zoom, xPos, yPos)))
        val pixel = Pixel(color.red, color.green, color.blue, 127)
        pos -> pixel
      }).seq.sortBy(_._1).map(_._2)

      val image = Image(imageWidth, imageHeight, pixels.toArray)
      image.output(new java.io.File("target/interactive_image.png"))
      image
  }

  /**
   *     * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
   *          @param yearlyData Sequence of (year, data), where `data` is some data associated with
   *                                `year`. The type of `data` can be anything.
   *                  @param generateImage Function that generates an image given a year, a zoom level, the x and
   *                                           y coordinates of the tile and the data to build the image from
   *                     
   *                     
   */
  // def generateTiles[Data](
  //   yearlyData: Iterable[(Int, Data)],
  //   generateImage: (Int, Int, Int, Int, Data) => Unit
  // ): Unit = {
  //   yearlyData.par.foreach(yd => {
  //     val year = yd._1
  //     val data = yd._2
  //     (0 to 3).par.foreach(zoom => {
  //       for (
  //            row <- (0 until 1 << zoom);
  //            col <- (0 until 1 << zoom)
  //          ) {
  //            val (x, y) = (col, row)
  //            generateImage(year, zoom, x, y, data)
  //            // val filepath = f"target/temperatures/$year/$zoom/$x-$y.png"
  //            // image.output(new java.io.File(filepath))
  //          }
  //     })
  //   })
  // }

  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    val _ = for {
      (year, data) <- yearlyData
      zoom <- 0 to 3
      x <- 0 until 1 << zoom
      y <- 0 until 1 << zoom
      } {
        generateImage(year, zoom, x, y, data)
      }
  }
}
