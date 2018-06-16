package othello

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
}
