package xiangqi

class FenTest extends munit.FunSuite:

  test("initial FEN writes correctly"):
    val fen = Fen.writeBoard(Board.standard)
    assertEquals(fen, "rnbakabnr/9/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/1C5C1/9/RNBAKABNR")

  test("FEN read/write roundtrip for standard"):
    val fen = Fen.initial
    Fen.read(fen) match
      case Left(err) => fail(s"Failed to read FEN: $err")
      case Right(pos) =>
        val fen2 = Fen.write(pos)
        val fen2Board = fen2.split(' ')(0)
        val fenBoard = fen.split(' ')(0)
        assertEquals(fen2Board, fenBoard)

  test("FEN read turn - red"):
    Fen.read("rnbakabnr/9/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/1C5C1/9/RNBAKABNR w - - 0 1") match
      case Right(pos) => assertEquals(pos.turn, Color.Red)
      case Left(e)    => fail(e)

  test("FEN read turn - black"):
    Fen.read("rnbakabnr/9/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/1C5C1/9/RNBAKABNR b - - 0 1") match
      case Right(pos) => assertEquals(pos.turn, Color.Black)
      case Left(e)    => fail(e)

  test("FEN read invalid rank count"):
    assert(Fen.read("rnbakabnr/9/1c5c1 w - - 0 1").isLeft)

  test("FEN isInitial"):
    val pos = Position.standard
    assert(Fen.isInitial(pos))

  test("FEN after move is not initial"):
    val game = Game.standard
    game.playUci("e3e4") match
      case Right(g) => assert(!Fen.isInitial(g.position))
      case Left(e)  => fail(e)
