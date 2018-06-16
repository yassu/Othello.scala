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

  describe ("otherCell") {
    it ("Other cell is Black color cell") {
      assert(Cell(Some(White)).otherCell == Cell(Some(Black)))
    }

    it ("Black cell is White color cell") {
      assert(Cell(Some(Black)).otherCell == Cell(Some(White)))
    }

    it ("None cell is White color cell") {
      assert(Cell(None).otherCell == Cell(None))
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

  it ("Cell.BlackCell can be initialized") {
    Cell.BlackCell
  }

  it ("Cell.WhiteCell can be initialized") {
    Cell.WhiteCell
  }

  it ("Cell.NoneCell can be initialized") {
    Cell.NoneCell
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
    it ("(n, m) direction1") {
      assert(Board.ensurePosition(3, 3))
    }

    it ("(n, m) direction2") {
      assert(Board.ensurePosition(0, 0))
    }

    it ("(-n, m) direction2") {
      assert(! Board.ensurePosition(-1, 0))
    }

    it ("(n, -m) direction2") {
      assert(! Board.ensurePosition(0, -1))
    }

    it ("(n, m) direction3") {
      assert(Board.ensurePosition(7, 0))
    }

    it ("(high, m) direction3") {
      assert(! Board.ensurePosition(8, 0))
    }

    it ("(n, m) direction4") {
      assert(Board.ensurePosition(0, 7))
    }

    it ("(n, high) direction4") {
      assert(! Board.ensurePosition(0, 8))
    }

    it ("(n, m) direction5") {
      assert(Board.ensurePosition(7, 7))
    }
  }

  describe ("changedCellPositionsByVertical1") {
    it ("initBoard") {
      val board = Board.initialBoard
      assert(board.changedCellPositionsByVertical1((3, 2), Cell(Some(Black))) == Set((3, 3)))
    }

    it ("initBoard2") {
      val board = Board.initialBoard
      assert(board.changedCellPositionsByVertical1((3, 2), Cell(Some(White))) == Set())
    }

    it ("initBoard3") {
      val board = Board.initialBoard.changeCell(Cell(Some(White)), (0, 7))
      assert(board.changedCellPositionsByVertical1((0, 6), Cell(Some(Black))) == Set())
    }

    it ("initBoard4") {
      val board = Board.initialBoard.
        changeCell(Cell(Some(Black)), (0, 7)).
        changeCell(Cell(Some(White)), (0, 6)).
        changeCell(Cell(Some(White)), (0, 5))
        assert(board.changedCellPositionsByVertical1((0, 4), Cell(Some(Black))) == Set((0, 5), (0, 6))
      )
    }
  }

  describe ("changedCellPositionsByVertical2") {
    it ("initialBoard1") {
      val board = Board.initialBoard
      assert(board.changedCellPositionsByVertical2((4, 5), Cell.BlackCell) == Set((4, 4)))
    }

    it ("initialBoard2") {
      val board = Board.initialBoard
      assert(board.changedCellPositionsByVertical2((3, 2), Cell.BlackCell) == Set())
    }

    it ("initialBoard3") {
      val board = Board.initialBoard.
        changeCell(Cell.WhiteCell, (0, 0))
      assert(board.changedCellPositionsByVertical2((0, 1), Cell.BlackCell) == Set())
    }

    it ("initialBoard4") {
      val board = Board.initialBoard.
        changeCell(Cell.BlackCell, (0, 0)).
        changeCell(Cell.WhiteCell, (0, 1)).
        changeCell(Cell.WhiteCell, (0, 2))
        assert(board.changedCellPositionsByVertical2((0, 3), Cell.BlackCell) == Set((0, 2), (0, 1)))
    }
  }

  describe ("changedCellPositionsByHeight1") {
    it ("initialBoard1") {
      val board = Board.initialBoard
      assert(board.changedCellPositionsByHeight1((2, 3), Cell.BlackCell) == Set((3, 3)))
    }

    it ("initialBoard2") {
      val board = Board.initialBoard
      assert(board.changedCellPositionsByHeight1((2, 4), Cell.BlackCell) == Set())
    }

    it ("initialBoard3") {
      val board = Board.initialBoard.changeCell(Cell.BlackCell, (7, 0))
      assert(board.changedCellPositionsByHeight1((6, 0), Cell.BlackCell) == Set())
    }

    it ("initialBoard4") {
      val board = Board.initialBoard.changeCell(Cell.BlackCell, (7, 0))
      assert(board.changedCellPositionsByHeight1((6, 0), Cell.BlackCell) == Set())
    }
  }

  it ("changeCell") {
    val board = Board.initialBoard.changeCell(Cell(Option(Black)), (3, 2))
    assert(board(3, 2) == Cell(Option(Black)))
  }

}
