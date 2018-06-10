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
