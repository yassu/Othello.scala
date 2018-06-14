package othello.elements

import org.scalatest.FunSpec

class ColorSpec extends FunSpec {
  describe("Color object should be initialized.") {
    it("Black color object should be initialized.") {
      Black
    }
    it("White color object should be initialized.") {
      White
    }
  }
}

class CellSpec extends FunSpec {
  it("Cell instance whose color is black should be initialized.") {
    Cell(Option(Black))
  }

  describe ("isDefined") {
    it ("Black Cell is defined") {
      assert(Cell(Some(Black)).isDefined)
    }

    it ("White Cell is defined.") {
      assert(Cell(Some(White)).isDefined)
    }

    it ("None Cell is not defined.") {
      assert(! Cell(None).isDefined)
    }
  }

  describe("colorChar") {
    it ("Cell(Black).colorChar") {
      Cell(Option(Black)).colorChar == 'B'
    }
    it ("Cell(White).colorChar") {
      Cell(Option(White)).colorChar == 'W'
    }
    it ("Cell(None).colorChar") {
      Cell(None).colorChar == '.'
    }
  }
}

class BoardSpec extends FunSpec {
  it("initialBoard") {
    val board = Board(List(
      List(Cell(None), Cell(None), Cell(None), Cell(None),
        Cell(None), Cell(None), Cell(None), Cell(None)),
      List(Cell(None), Cell(None), Cell(None), Cell(None),
        Cell(None), Cell(None), Cell(None), Cell(None)),
      List(Cell(None), Cell(None), Cell(None), Cell(None),
        Cell(None), Cell(None), Cell(None), Cell(None)),
      List(Cell(None), Cell(None), Cell(None), Cell(Some(White)),
        Cell(Some(Black)), Cell(None), Cell(None), Cell(None)),
      List(Cell(None), Cell(None), Cell(None), Cell(Some(Black)),
        Cell(Some(White)), Cell(None), Cell(None), Cell(None)),
      List(Cell(None), Cell(None), Cell(None), Cell(None),
        Cell(None), Cell(None), Cell(None), Cell(None)),
      List(Cell(None), Cell(None), Cell(None), Cell(None),
        Cell(None), Cell(None), Cell(None), Cell(None)),
      List(Cell(None), Cell(None), Cell(None), Cell(None),
        Cell(None), Cell(None), Cell(None), Cell(None)),
    ))
    assert(Board.initialBoard == board)
  }

  describe ("ensurePosition") {
    val board = Board.initialBoard
    it ("(n, m) direction") {
      assert(board.ensurePosition(3, 3))
    }

    it ("(n, m) direction2") {
      assert(board.ensurePosition(0, 0))
    }

    it ("(-n, m) direction2") {
      assert(! board.ensurePosition(-1, 0))
    }

    it ("(n, -m) direction2") {
      assert(! board.ensurePosition(0, -1))
    }

    it ("(n, m) direction3") {
      assert(board.ensurePosition(7, 0))
    }

    it ("(high, m) direction3") {
      assert(! board.ensurePosition(8, 0))
    }

    it ("(n, m) direction4") {
      assert(board.ensurePosition(0, 7))
    }

    it ("(n, high) direction4") {
      assert(! board.ensurePosition(0, 8))
    }

    it ("(n, m) direction5") {
      assert(board.ensurePosition(7, 7))
    }
  }

  describe ("changedCellPositionsByVertical1") {
    it ("initBoard") {
      val board = Board.initialBoard
      assert(board.changedCellPositionsByVertical1((3, 2), Cell(Some(Black))) == List((3, 3)))
    }

    it ("initBoard2") {
      val board = Board.initialBoard
      assert(board.changedCellPositionsByVertical1((3, 2), Cell(Some(White))) == List())
    }

    it ("initBoard3") {
      val board = Board.initialBoard.changeCell(Cell(Some(White)), (0, 7))
      assert(board.changedCellPositionsByVertical1((0, 6), Cell(Some(Black))) == List())
    }

    it ("initBoard4") {
      val board = Board.initialBoard.
        changeCell(Cell(Some(Black)), (0, 7)).
        changeCell(Cell(Some(White)), (0, 6)).
        changeCell(Cell(Some(White)), (0, 5))
      assert(board.changedCellPositionsByVertical1((0, 4), Cell(Some(Black))) == List((0, 5), (0, 6))
      )
    }
  }

  it ("changeCell") {
    val board = Board.initialBoard.changeCell(Cell(Option(Black)), (3, 2))
    assert(board(3, 2) == Cell(Option(Black)))
  }

}
