package othello

case class User(board: Board, cell: Cell) {
  def put(pos: (Int, Int)): User = {
    val changedPositions = board.changedCellPositions(pos, cell) + pos
    println(changedPositions)
    User (
      Board(
        (
          for (y <- 0 until Board.SIZE) yield (
            (
              for (x <- 0 until Board.SIZE)
                yield if (changedPositions.contains((x, y))) cell else board(x, y)
            ).toList
          )
        ).toList
      ),
      cell
    )
  }
}
