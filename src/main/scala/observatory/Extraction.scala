package observatory

import java.io.{BufferedReader, File, FileReader, InputStreamReader}
import java.time.LocalDate

import scala.annotation.tailrec
import scala.tools.nsc.interpreter.InputStream

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val tempFileIS = getClass.getResourceAsStream(temperaturesFile)
    val stationsFileIS = getClass.getResourceAsStream(stationsFile)
    val stations = Map() ++ readCsv(stationsFileIS).map {
      case Array(f1, f2, f3, f4) => Option((f1, f2, f3, f4))
      case _ => Option.empty
    }.filter(_.isDefined)
      .map(_.get)
      .map(t => ((t._1, t._2) -> t))

    readCsv(tempFileIS).map {
      case Array(f1, f2, f3, f4, f5, _*) => (f1, f2, f3, f4, f5)
    }.filterNot(t => BigDecimal(t._5).equals(9999.9))
      .filter(t => stations.contains((t._1, t._2)))
      .map(t => {
        val localDate = LocalDate.of(year, t._3.toInt, t._4.toInt)
        val station = stations((t._1, t._2))
        val location = Location(station._3.toDouble, station._4.toDouble)
        (localDate, location, convertFahrenheitToDegree(t._5.toDouble))
      })
  }

  def convertFahrenheitToDegree(fah: Double): Double =
    (fah - 32.0) * (5.0/9.0)

  def readCsv(file: InputStream): Seq[Array[String]] = {
    val breader = new BufferedReader(new InputStreamReader(file))
    val lines: List[Option[String]] = readBuffer(breader, List())
    lines.map(_.map(_.split(",").map(_.trim))).map(_.get)
  }

  @tailrec
  def readBuffer(reader: BufferedReader, buffer: List[Option[String]]): List[Option[String]] = {
    val line = Option(reader.readLine())
    if (line.isDefined) {
      readBuffer(reader, line :: buffer)
    } else {
      buffer
    }
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records.groupBy(_._2).mapValues(v => v.map(_._3).reduce(_+_) / v.size)
  }

}
