package xiangqi

class BoardTest extends munit.FunSuite:

  test("standard board has 32 pieces"):
    assertEquals(Board.standard.pieces.size, 32)

  test("standard board red general at e1 (file=4, rank=0)"):
    val sq = Square(File.unsafe(4), Rank.unsafe(0))
    assertEquals(Board.standard(sq), Some(Piece(Color.Red, Role.General)))

  test("standard board black general at e10 (file=4, rank=9)"):
    val sq = Square(File.unsafe(4), Rank.unsafe(9))
    assertEquals(Board.standard(sq), Some(Piece(Color.Black, Role.General)))

  test("standard board has 5 red soldiers at rank 3"):
    val soldiers = Board.standard.piecesOf(Color.Red).filter(_._2.role == Role.Soldier)
    assertEquals(soldiers.size, 5)
    assert(soldiers.keys.forall(_.rank.value == 3))

  test("FEN initial write and read roundtrip"):
    val pos = Position.standard
    val fen = Fen.write(pos)
    val pos2 = Fen.read(fen)
    assert(pos2.isRight)
    assertEquals(Fen.write(pos2.toOption.get), fen)

  test("FEN initial position matches known FEN"):
    val fen = Fen.writeBoard(Board.standard)
    assertEquals(fen, "rnbakabnr/9/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/1C5C1/9/RNBAKABNR")
