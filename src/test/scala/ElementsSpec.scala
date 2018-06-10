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
    var board = Board(List(
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
}
