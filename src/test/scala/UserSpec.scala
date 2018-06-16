package othello

import org.scalatest.FunSpec

class UserSpec extends FunSpec {
  it ("User instance is initialized") {
    User(Board.initialBoard, Cell.BlackCell)
  }

  it ("put") {
    val user = User(Board.initialBoard, Cell.BlackCell)
    val resBoard = Board.initialBoard.
      changeCell(Cell.BlackCell, (2, 3)).
      changeCell(Cell.BlackCell, (3, 3))
    val resUser = User(resBoard, Cell.BlackCell)
    println(user.put(2, 3).board(3))
    assert(user.put((2, 3)) == resUser)
  }
}
