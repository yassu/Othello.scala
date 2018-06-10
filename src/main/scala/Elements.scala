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

case class Board (cells: List[List[Cell]])

object Board {
  val SIZE = 8
  def initialBoard: Board = Board(
    (
      for (i <- 0 until SIZE) yield (
        (
          for (j <- 0 until SIZE) yield Cell(
            if (i == SIZE / 2 - 1 && j == SIZE / 2 - 1) Some(White)
            else if (i == SIZE / 2 - 1 && j == SIZE / 2) Some(Black)
            else if (i == SIZE / 2 && j == SIZE / 2 - 1) Some(Black)
            else if (i == SIZE / 2 && j == SIZE / 2) Some(White)
            else None
          )
        ).toList
      )
    ).toList
  )
}
