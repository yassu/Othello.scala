package othello

import org.scalatest.FunSpec

class UserSpec extends FunSpec {
  it ("User instance is initialized") {
    User(Cell.BlackCell)
  }

  it ("put") {
    val user = User(Cell.BlackCell)
    val resBoard = Board.initialBoard.
      changeCell(Cell.BlackCell, (2, 3)).
      changeCell(Cell.BlackCell, (3, 3))
    assert(user.put(Board.initialBoard, (2, 3)) == resBoard)
  }

  it ("puttable") {
    val user = User(Cell.BlackCell)
    assert(user.puttable(Board.initialBoard, (3, 2)))
    assert(! user.puttable(Board.initialBoard, (4, 2)))
  }
}
