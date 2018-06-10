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

case class Board (cells: List[List[Cell]]) {
  def apply(x: Int, y: Int): Cell = cells(y)(x)
  def changeCell(cell: Cell, pos: (Int, Int)): Board = Board (
    (
      for (y <- (0 until Board.SIZE)) yield (
        (
          for (x <- (0 until Board.SIZE))
            yield if(x == pos._1 && y == pos._2) cell else cells(y)(x)
        ).toList
      )
    ).toList
  )
}

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
