package othello.elements

sealed trait Color

case object Black extends Color()
case object White extends Color()

case class Cell(color: Option[Color]) {
  def colorChar: Char = color match {
    case Some(Black) => 'B'
    case Some(White) => 'W'
    case None => '.'
  }
}
