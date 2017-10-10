package observatory

object Main extends App {

  def colorScales = List(
    (60.0, Color(255, 255, 255)),
    (32.0, Color(255, 0  , 0)),
    (12.0, Color(255, 255,  0)),
    (0.0,  Color(0  , 255,  255)),
    (-15.0, Color(0  , 0, 255)),
    (-27.0, Color(255, 0,   255)),
    (-50.0, Color(33 , 0, 107)),
    (-60.0, Color(0  , 0, 0))
  ).toIterable

  val records = Extraction.locateTemperatures(2021, "/mystations.csv", "/2021.csv")
  val temperatures = Extraction.locationYearlyAverageRecords(records)
  val image = Interaction.tile(temperatures, colorScales, 0, 1, 1)
}
