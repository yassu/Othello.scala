package othello

import io._

case class User(cell: Cell) {
  def put(board: Board, pos: (Int, Int)): Board = {
    val changedPositions = board.changedCellPositions(pos, cell) + pos
    Board(
      (
        for (y <- 0 until Board.SIZE) yield (
          (
            for (x <- 0 until Board.SIZE)
              yield if (changedPositions.contains((x, y))) cell else board(x, y)
          ).toList
        )
      ).toList
    )
  }

  def puttable(board: Board, pos: (Int, Int)): Boolean = board.puttable(pos, cell)

  def getPosition(board: Board): (Int, Int) = {
    try {
      val Array(x, y) = io.StdIn.readLine.split(' ').map(_.toInt)
      println(s"(x, y) = ($x, $y)")
      if (! this.puttable(board, (x, y)))
        throw new IllegalArgumentException("xyz")
      return (x, y)
    }
    catch {
      case e: Exception => {
        println("Occur number error or can't put such number.")
        print("Please input other number. >>")
        println(e)
        return this.getPosition(board)
      }
    }
  }
}
