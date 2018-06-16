package othello

import scala.util.control.Breaks

object Main {
  def main(args: Array[String]) {
    var board = Board.initialBoard
    var blackUser = User(Cell.BlackCell)
    var whiteUser = User(Cell.WhiteCell)

    println("Black Turn")
    println(board.toPrettyString)
    println("Please input number >> ")
    val blackPos = blackUser.getPosition(board)
    board = blackUser.put(board, blackPos)

    println(board.toPrettyString)

    println("White Turn")
    println(board.toPrettyString)
    println("Please input number >> ")
    val whitePos = whiteUser.getPosition(board)
    board = whiteUser.put(board, whitePos)

    println(board.toPrettyString)
  }
}
