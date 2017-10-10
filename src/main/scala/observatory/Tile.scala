package observatory

import scala.math._

case class Tile(x: Int, y: Int, z: Int) {
  def toLatLon = new LatLonPoint(toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y.toDouble / (1 << z))))), x.toDouble / (1 << z) * 360.0 - 180.0, z)
}

case class LatLonPoint(lat: Double, lon: Double, z: Int) {
  def toTile = new Tile(
    ((lon + 180.0) / 360.0 * (1 << z)).toInt,
    ((1 - log(tan(toRadians(lat)) + 1 / cos(toRadians(lat))) / Pi) / 2.0 * (1 << z)).toInt,
    z
  )
}
