package xiangqi

/**
 * FEN-like notation for Xiangqi (WXF/UCCI format).
 *
 * Format: <board> <turn> <move-counter>
 * Board: ranks 9 to 0, separated by '/', pieces by forsyth char (upper=Red, lower=Black), digits=empty.
 *
 * Example starting position:
 * rnbakabnr/9/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/1C5C1/9/RNBAKABNR w - - 0 1
 * (using UCCI notation where Red=uppercase)
 */
object Fen:

  val initial: String = "rnbakabnr/9/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/1C5C1/9/RNBAKABNR w - - 0 1"

  def write(pos: Position): String =
    val boardStr = writeBoard(pos.board)
    val turnStr  = if pos.turn == Color.Red then "w" else "b"
    s"$boardStr $turnStr - - 0 ${pos.plies / 2 + 1}"

  def writeBoard(board: Board): String =
    (9 to 0 by -1).map: r =>
      writeRank(board, r)
    .mkString("/")

  private def writeRank(board: Board, r: Int): String =
    val sb = new StringBuilder
    var empty = 0
    for f <- 0 to 8 do
      Square.fromInts(f, r).flatMap(board(_)) match
        case None =>
          empty += 1
        case Some(piece) =>
          if empty > 0 then
            sb.append(empty.toString)
            empty = 0
          sb.append(piece.forsyth)
    if empty > 0 then sb.append(empty.toString)
    sb.toString

  def read(fen: String): Either[String, Position] =
    val parts = fen.trim.split(' ')
    if parts.length < 2 then return Left(s"Invalid FEN: $fen")
    for
      board <- readBoard(parts(0))
      turn  <- readTurn(parts(1))
    yield
      val plies = if parts.length >= 6 then
        (parts(5).toIntOption.getOrElse(1) - 1) * 2
      else 0
      Position(board, turn, plies)

  private def readTurn(s: String): Either[String, Color] = s match
    case "w" | "r" => Right(Color.Red)
    case "b"       => Right(Color.Black)
    case other     => Left(s"Invalid turn: $other")

  private def readBoard(s: String): Either[String, Board] =
    import scala.util.boundary, boundary.break
    val ranks = s.split('/')
    if ranks.length != 10 then Left(s"Expected 10 ranks, got ${ranks.length}")
    else boundary:
      var pieces = Map.empty[Square, Piece]
      for (rankStr, rankIdx) <- ranks.zipWithIndex do
        val r = 9 - rankIdx // rank 9 comes first in FEN
        var f = 0
        for c <- rankStr do
          if c.isDigit then
            f += c.asDigit
          else
            Piece.fromForsyth(c) match
              case None => break(Left(s"Unknown piece char: $c"))
              case Some(piece) =>
                Square.fromInts(f, r) match
                  case None => break(Left(s"Invalid square: file=$f rank=$r"))
                  case Some(sq) => pieces = pieces + (sq -> piece)
                f += 1
      Right(Board(pieces))

  def isInitial(pos: Position): Boolean =
    writeBoard(pos.board) == writeBoard(Position.standard.board)
