package othello.elements

sealed trait Color

case object Black extends Color()
case object White extends Color()

case class Cell(color: Option[Color]) {
  def isDefined: Boolean = color match {
    case Some(e) => true
    case None => false
  }

  def otherCell: Cell = color match {
    case Some(White) => Cell(Some(Black))
    case Some(Black) => Cell(Some(White))
    case None => Cell(None)
  }

  def colorChar: Char = color match {
    case Some(Black) => 'B'
    case Some(White) => 'W'
    case None => '.'
  }
}

object Cell {
  val BlackCell = Cell(Some(Black))
  val WhiteCell = Cell(Some(White))
  val NoneCell = Cell(None)
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

  def puttable(pos: (Int, Int), cell: Cell): Boolean =
    this.changedCellPositionsByVertical1(pos, cell).nonEmpty ||
    this.changedCellPositionsByVertical2(pos, cell).nonEmpty ||
    this.changedCellPositionsByHeight1(pos, cell).nonEmpty ||
    this.changedCellPositionsByHeight2(pos, cell).nonEmpty

  def changedCellPositions(pos: (Int, Int), cell: Cell): Set[(Int, Int)] =
    this.changedCellPositionsByVertical1(pos, cell) ++
    this.changedCellPositionsByVertical2(pos, cell) ++
    this.changedCellPositionsByHeight1(pos, cell) ++
    this.changedCellPositionsByHeight2(pos, cell)

  def numberOfCells(cell: Cell): Int =
    (
      for (x <- (0 until Board.SIZE); y <- (0 until Board.SIZE)) yield (x, y)
    ).filter(t => this(t._1, t._2) == cell).size

  private[othello] def changedCellPositionsByVertical1(pos: (Int, Int), cell: Cell): Set[(Int, Int)] = {
    val rowCols = (pos._2 + 1 until Board.SIZE).
      takeWhile(this(pos._1, _).otherCell == cell)
    if (rowCols.isEmpty) Set()
    else if (this(pos._1, pos._2).isDefined) Set()
    else if (! Board.ensurePosition(pos._1, rowCols.last + 1)) Set()
    else if (this(pos._1, rowCols.last + 1) == cell) rowCols.map(h => (pos._1, h)).toSet
    else Set()
  }

  private[othello] def changedCellPositionsByVertical2(pos: (Int, Int), cell: Cell): Set[(Int, Int)] = {
    val rowCols = (0 until pos._2).reverse.
      takeWhile(this(pos._1, _).otherCell == cell)
    if (rowCols.isEmpty) Set()
    else if (this(pos._1, pos._2).isDefined) Set()
    else if (! Board.ensurePosition(pos._1, rowCols.last - 1)) Set()
    else if (this(pos._1, rowCols.last - 1) == cell) rowCols.map(h => (pos._1, h)).toSet
    else Set()
  }

  private[othello] def changedCellPositionsByHeight1(pos: (Int, Int), cell: Cell): Set[(Int, Int)] = {
    val rowCols = (pos._1 + 1 until Board.SIZE).
      takeWhile(this(_, pos._2).otherCell == cell)
    if (rowCols.isEmpty) Set()
    else if (this(pos._1, pos._2).isDefined) Set()
    else if (! Board.ensurePosition(pos._1 + 1, rowCols.last)) Set()
    else if (this(rowCols.last + 1, pos._2) == cell) rowCols.map(h => (h, pos._2)).toSet
    else Set()
  }

  private[othello] def changedCellPositionsByHeight2(pos: (Int, Int), cell: Cell): Set[(Int, Int)] = {
    val rowCols = (0 until pos._1).reverse.
      takeWhile(this(_, pos._2).otherCell == cell)
    if (rowCols.isEmpty) Set()
    else if (this(pos._1, pos._2).isDefined) Set()
    else if (! Board.ensurePosition(rowCols.last - 1, pos._2)) Set()
    else if (this(rowCols.last - 1, pos._2) == cell) rowCols.map(h => (h, pos._2)).toSet
    else Set()
  }

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

  private[othello] def ensurePosition(x: Int, y: Int): Boolean =
    x >= 0 && x < Board.SIZE &&
    y >= 0 && y < Board.SIZE
}
