package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math.acos
import scala.math.cos
import scala.math.sin
import scala.math.abs
import scala.math.Pi
import scala.math.pow
import scala.math.rint

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    val P = 3

    val temps = temperatures.map(t => {
      val dist = locationDistance(location, t._1)
      val w = 1.0 / pow(dist, P)
      (t._1, t._2, dist, if (dist <= 1.0) t._2 else w)
    }).toList.sortBy(_._3)
    val totalMu = temps.map(t => t._2 * t._4).reduce(_ + _)
    val totalW = temps.map(_._4).reduce(_ + _)
    val mu = totalMu / totalW
    val zeros = temps.filter(t => t._3 <= 1.0).map(t => t._2)
    if (zeros.isEmpty) {
      mu
    } else {
      zeros.head
    }
  }

  def locationDistance(loc1: Location, loc2: Location): Double = {
    val r = 6371 // Mean radius of the Each (km)
    val pi = Pi
    // p == latitude, q == longitude
    val p1 = loc1.lat * pi / 180 // location 1 latitude in radian
    val p2 = loc2.lat * pi / 180 // location 2 latitude in radian
    val q1 = loc1.lon * pi / 180 // location 1 longitude in radian
    val q2 = loc2.lon * pi / 180 // location 2 longitude in radian
    val deltaLat = abs(p1 - p2)
    val deltaLon = abs(q1 - q2)
    val delta = acos(sin(p1) * sin(p2) + cos(p1) * cos(p2) * cos(deltaLon))
    r * delta
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
    def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
      val samePoint = points.find(_._1 == value).map(_._2)
      if (samePoint.isDefined) {
        samePoint.get
      } else {
        val sortedPoints = points.toSeq.sortWith(_._1 < _._1)
        val leftPoints = points.filter(p => p._1 < value).toSeq.sortWith(_._1 < _._1)
        val rightPoints = points.filter(p => p._1 > value).toSeq.sortWith(_._1 < _._1)
        if (leftPoints.isEmpty) {
          sortedPoints.head._2
        } else if (rightPoints.isEmpty) {
          sortedPoints.last._2
        } else {
          interpolate(leftPoints.last, rightPoints.head, value)
        }
      }
    }

  def interpolate(p0: (Double, Color), p1: (Double, Color), x: Double): Color = {
    val x0 = p0._1
    val y0 = p0._2
    val x1 = p1._1
    val y1 = p1._2
    val y0Product = (1 - (x - x0) / (x1 - x0))
    val y0Total = y0.productIterator.map(_.asInstanceOf[Int] * y0Product)
    val y1Product = (1 - (x1 - x) / (x1 - x0))
    val y1Total = y1.productIterator.map(_.asInstanceOf[Int] * y1Product)
    val newColorTuple = y0Total.zip(y1Total).map(e => e._1 + e._2).toList.map(rint)
    Color(newColorTuple(0).asInstanceOf[Int],
      newColorTuple(1).asInstanceOf[Int],
      newColorTuple(2).asInstanceOf[Int])
  }

  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val array = Array.ofDim[Pixel](180, 360)
    val tempMap = Map() ++ temperatures.map(t => (gpsToCoordinate(t._1) -> interpolateColor(colors, t._2)))
    for (
         i <- 0 until 180;
         j <- 0 until 360
       ) {
         val color = if (tempMap.contains((i, j))) {
           tempMap((i, j))
         } else {
           val loc = coordindatesToGps(i, j)
           val predictedTemp = predictTemperature(temperatures, loc)
           interpolateColor(colors, predictedTemp)
         }
         val pixel = Pixel(color.red, color.green, color.blue, 127)
         array(i)(j) = pixel
       }
    val arr = array.flatten
    val myImage = Image(360, 180, arr)
    myImage.output(new java.io.File("target/some-image.png"))
    myImage
  }

  def gpsToCoordinate(location: Location): (Int, Int) = {
    val lon = rint(location.lon).asInstanceOf[Int]
    val lat = rint(location.lat).asInstanceOf[Int]
    (90 - lat, 180 + lon)
  }

  def coordindatesToGps(row: Int, column: Int): Location = {
    Location(90 - row, column - 180)
  }
}

