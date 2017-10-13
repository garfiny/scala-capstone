package observatory

import scala.math.rint
import scala.math.sqrt

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {
    // println("temperatures ===============")
    // temperatures.foreach(t => {
    //   println("Location: (" + t._1.lat + ", " + t._1.lon + ") == TEMP: " + t._2 )
    // })
    val itr = dimIterator
    val array = Array.fill[LatLonGrid](180, 360) { 
      val (lat: Int, lon: Int) = itr.next 
      LatLonGrid(lat, lon, Visualization.predictTemperature(temperatures, Location(lat, lon)))
    }
    temperatures.foreach(t => findGrid(array, t._1.lat, t._1.lon).temperature = t._2)
    (lat: Int, lon: Int) => findGrid(array, lat, lon).temperature
  }

  def findGrid(array: Array[Array[LatLonGrid]], lat: Double, lon: Double): LatLonGrid = {
    val row = rint(90 - lat).toInt
    val column = rint(180 + lon).toInt
    array(row)(column)
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {
    val grids = temperaturess.par.map(makeGrid)
    println("Size: " + grids.size)
    val arrayItr = dimIterator
    val array = Array.fill[LatLonGrid](180, 360) { 
      val (lat: Int, lon: Int) = arrayItr.next 
      LatLonGrid(lat, lon, 0.0)
    }
    val itr = (for {
      i <- 90 until -90 by -1
      j <- -180 until 180
    } yield (i, j)).par.toIterator
    for (i <- itr) {
      val lat = i._1
      val lon = i._2
      val avg = grids.par.map(func => func(lat, lon)).sum / grids.size
      findGrid(array, lat, lon).temperature = avg
    }

    // val array = Array.fill[LatLonGrid](180, 360) { 
    //   val (lat: Int, lon: Int) = itr.next 
    //   val avg = grids.par.map(func => func(lat, lon)).sum / grids.size
    //   LatLonGrid(lat, lon, avg)
    // }
    (lat: Int, lon: Int) => findGrid(array, lat, lon).temperature
  }

  def dimIterator: Iterator[(Int, Int)] = 
    (for {
      i <- 90 until -90 by -1
      j <- -180 until 180
    } yield (i, j)).toIterator


  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {
    val gridFunc = makeGrid(temperatures)
    (lat: Int, lon: Int) => {
      val temperature = gridFunc(lat, lon)
      val mean = normals(lat, lon)
      sqrt(temperature - mean)
    }
  }
}

case class LatLonGrid(lat: Int, lon: Int, var temperature: Double)
