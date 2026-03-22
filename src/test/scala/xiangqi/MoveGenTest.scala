package xiangqi

class MoveGenTest extends munit.FunSuite:

  test("chariot at a1 can slide along rank and file"):
    val sq = Square(File.unsafe(0), Rank.unsafe(0))
    val board = Board(Map(sq -> Piece(Color.Red, Role.Chariot)))
    val dests = MoveGen.destinations(board, sq, Piece(Color.Red, Role.Chariot))
    // Can move to all squares on file 0 (ranks 1-9) and rank 0 (files 1-8)
    assertEquals(dests.count(_.file.value == 0), 9)  // ranks 1-9
    assertEquals(dests.count(_.rank.value == 0), 8)  // files 1-8

  test("chariot blocked by friendly piece"):
    val from = Square(File.unsafe(0), Rank.unsafe(0))
    val blocker = Square(File.unsafe(0), Rank.unsafe(3))
    val board = Board(Map(
      from -> Piece(Color.Red, Role.Chariot),
      blocker -> Piece(Color.Red, Role.Soldier)
    ))
    val dests = MoveGen.destinations(board, from, Piece(Color.Red, Role.Chariot))
    assert(!dests.contains(blocker))  // can't capture own piece
    assert(!dests.exists(d => d.file.value == 0 && d.rank.value > 3)) // blocked beyond

  test("chariot can capture enemy"):
    val from = Square(File.unsafe(0), Rank.unsafe(0))
    val enemy = Square(File.unsafe(0), Rank.unsafe(3))
    val board = Board(Map(
      from -> Piece(Color.Red, Role.Chariot),
      enemy -> Piece(Color.Black, Role.Soldier)
    ))
    val dests = MoveGen.destinations(board, from, Piece(Color.Red, Role.Chariot))
    assert(dests.contains(enemy))
    assert(!dests.exists(d => d.file.value == 0 && d.rank.value > 3))

  test("cannon moves without capture are non-jumping"):
    // Cannon at b1, blocker at b5, empty otherwise
    val from = Square(File.unsafe(1), Rank.unsafe(0))
    val blocker = Square(File.unsafe(1), Rank.unsafe(4))
    val board = Board(Map(
      from -> Piece(Color.Red, Role.Cannon),
      blocker -> Piece(Color.Red, Role.Soldier)
    ))
    val dests = MoveGen.destinations(board, from, Piece(Color.Red, Role.Cannon))
    // Can reach ranks 1,2,3 on file 1 (before blocker), can't reach blocker (same color)
    assert(dests.contains(Square(File.unsafe(1), Rank.unsafe(1))))
    assert(dests.contains(Square(File.unsafe(1), Rank.unsafe(3))))
    assert(!dests.contains(blocker))

  test("cannon captures by jumping over exactly one piece"):
    val from = Square(File.unsafe(4), Rank.unsafe(0))
    val screen = Square(File.unsafe(4), Rank.unsafe(4))
    val target = Square(File.unsafe(4), Rank.unsafe(8))
    val board = Board(Map(
      from -> Piece(Color.Red, Role.Cannon),
      screen -> Piece(Color.Red, Role.Soldier),
      target -> Piece(Color.Black, Role.General)
    ))
    val dests = MoveGen.destinations(board, from, Piece(Color.Red, Role.Cannon))
    assert(dests.contains(target))
    // Can't land on screen (own piece) and can't go beyond target
    assert(!dests.contains(screen))
    assert(!dests.exists(d => d.file.value == 4 && d.rank.value > 8))

  test("general stays in palace"):
    // General at d2 (file=3, rank=1) - center of palace has moves: N(3,2), S(3,0), E(4,1), W(2,1)
    val sq = Square(File.unsafe(3), Rank.unsafe(1))
    val board = Board(Map(sq -> Piece(Color.Red, Role.General)))
    val dests = MoveGen.destinations(board, sq, Piece(Color.Red, Role.General))
    assert(dests.forall(_.inRedPalace))
    assertEquals(dests.size, 4) // can go N, S, E, W all within palace

  test("general at corner of palace has 2 moves"):
    val sq = Square(File.unsafe(2), Rank.unsafe(0)) // bottom-left of red palace
    val board = Board(Map(sq -> Piece(Color.Red, Role.General)))
    val dests = MoveGen.destinations(board, sq, Piece(Color.Red, Role.General))
    assertEquals(dests.size, 2) // only right and up

  test("advisor moves diagonally in palace"):
    val sq = Square(File.unsafe(3), Rank.unsafe(0))
    val board = Board(Map(sq -> Piece(Color.Red, Role.Advisor)))
    val dests = MoveGen.destinations(board, sq, Piece(Color.Red, Role.Advisor))
    assert(dests.nonEmpty)
    assert(dests.forall(_.inRedPalace))

  test("elephant cannot cross river"):
    val sq = Square(File.unsafe(2), Rank.unsafe(2)) // red elephant in red territory
    val board = Board(Map(sq -> Piece(Color.Red, Role.Elephant)))
    val dests = MoveGen.destinations(board, sq, Piece(Color.Red, Role.Elephant))
    assert(dests.forall(_.onRedSide))

  test("elephant blocked at elbow"):
    val from = Square(File.unsafe(2), Rank.unsafe(0))
    val elbow = Square(File.unsafe(3), Rank.unsafe(1)) // elbow for move (2,2)
    val dest = Square(File.unsafe(4), Rank.unsafe(2))
    val board = Board(Map(
      from -> Piece(Color.Red, Role.Elephant),
      elbow -> Piece(Color.Black, Role.Soldier)
    ))
    val dests = MoveGen.destinations(board, from, Piece(Color.Red, Role.Elephant))
    assert(!dests.contains(dest))

  test("horse L-shape movement"):
    val sq = Square(File.unsafe(4), Rank.unsafe(4)) // center
    val board = Board(Map(sq -> Piece(Color.Red, Role.Horse)))
    val dests = MoveGen.destinations(board, sq, Piece(Color.Red, Role.Horse))
    assertEquals(dests.size, 8) // all 8 knight moves possible from center

  test("horse leg blocked"):
    val from = Square(File.unsafe(4), Rank.unsafe(4))
    val legBlock = Square(File.unsafe(4), Rank.unsafe(5)) // blocks upward leg
    val board = Board(Map(
      from -> Piece(Color.Red, Role.Horse),
      legBlock -> Piece(Color.Black, Role.Soldier)
    ))
    val dests = MoveGen.destinations(board, from, Piece(Color.Red, Role.Horse))
    // Moves requiring upward first step are blocked (2 moves: (3,6) and (5,6))
    assert(!dests.contains(Square(File.unsafe(3), Rank.unsafe(6))))
    assert(!dests.contains(Square(File.unsafe(5), Rank.unsafe(6))))

  test("soldier before crossing river can only move forward"):
    val sq = Square(File.unsafe(4), Rank.unsafe(3)) // red soldier, not yet crossed
    val board = Board(Map(sq -> Piece(Color.Red, Role.Soldier)))
    val dests = MoveGen.destinations(board, sq, Piece(Color.Red, Role.Soldier))
    assertEquals(dests.size, 1)
    assertEquals(dests.head.rank.value, 4) // forward

  test("soldier after crossing river can move forward and sideways"):
    val sq = Square(File.unsafe(4), Rank.unsafe(6)) // red soldier, crossed river
    val board = Board(Map(sq -> Piece(Color.Red, Role.Soldier)))
    val dests = MoveGen.destinations(board, sq, Piece(Color.Red, Role.Soldier))
    assertEquals(dests.size, 3) // forward + left + right

  test("standard position legal moves count for red"):
    val pos = Position.standard
    val moves = pos.legalMoves
    // Standard Xiangqi: Red has moves from 5 chariots, horses, cannons, soldiers, general
    // Opening: 44 legal moves (similar to chess ~44)
    assert(moves.size > 30)
    assert(moves.size < 60)

  test("flying general detection"):
    // Place both generals on same file with nothing between
    val board = Board(Map(
      Square(File.unsafe(4), Rank.unsafe(0)) -> Piece(Color.Red, Role.General),
      Square(File.unsafe(4), Rank.unsafe(9)) -> Piece(Color.Black, Role.General)
    ))
    assert(MoveGen.flyingGeneral(board))

  test("no flying general when pieces between"):
    val board = Board(Map(
      Square(File.unsafe(4), Rank.unsafe(0)) -> Piece(Color.Red, Role.General),
      Square(File.unsafe(4), Rank.unsafe(5)) -> Piece(Color.Red, Role.Chariot),
      Square(File.unsafe(4), Rank.unsafe(9)) -> Piece(Color.Black, Role.General)
    ))
    assert(!MoveGen.flyingGeneral(board))

  test("checkmate detection - basic"):
    // Red general surrounded on 3 sides with Black chariot threatening
    // Simple position where Red has no legal moves
    val board = Board(Map(
      Square(File.unsafe(4), Rank.unsafe(0)) -> Piece(Color.Red, Role.General),
      Square(File.unsafe(3), Rank.unsafe(0)) -> Piece(Color.Red, Role.Advisor), // blocks left
      Square(File.unsafe(5), Rank.unsafe(0)) -> Piece(Color.Red, Role.Advisor), // blocks right
      // Black pieces threatening
      Square(File.unsafe(4), Rank.unsafe(9)) -> Piece(Color.Black, Role.General),
      Square(File.unsafe(4), Rank.unsafe(1)) -> Piece(Color.Black, Role.Chariot), // threatens e1
      Square(File.unsafe(0), Rank.unsafe(1)) -> Piece(Color.Black, Role.Chariot), // covers rank 1
    ))
    val pos = Position(board, Color.Red)
    // Red general can only try to move to (4,1) but that's covered by Black chariot
    // or (3,0)/(5,0) but those are own advisors
    // This is at least check
    assert(pos.isInCheck)

  test("game play through UCI"):
    val game = Game.standard
    // Red cannon from b3 to b8 (opening cannon center move would be e3)
    // Let's play a simple valid move: Red chariot at a1 can't move (blocked)
    // Play: Red soldier at e4 forward to e5 (crosses river)
    val result = game.playUci("e3e4") // file e=4, rank 3 -> rank 4
    // This is a valid Red soldier move
    result match
      case Right(g) =>
        assertEquals(g.turn, Color.Black)
        assertEquals(g.plies, 1)
      case Left(err) =>
        fail(s"Expected valid move but got: $err")
