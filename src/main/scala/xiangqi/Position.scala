package xiangqi

/**
 * A Xiangqi position: board + turn + move number.
 */
case class Position(
    board: Board,
    turn: Color,
    plies: Int = 0,          // half-moves played
    lastMove: Option[Move] = None
):
  def ply: Int = plies

  /** Apply a move without legality checking. */
  def playUnchecked(move: Move): Position =
    copy(
      board = MoveGen.applyMove(board, move),
      turn = turn.opposite,
      plies = plies + 1,
      lastMove = Some(move)
    )

  /** Apply a move with full legality checking. */
  def play(move: Move): Either[String, Position] =
    val legal = MoveGen.legalMoves(board, turn)
    if legal.exists(m => m.from == move.from && m.to == move.to) then
      Right(playUnchecked(move))
    else
      Left(s"Illegal move: ${move.uci}")

  /** Play by UCI string (e.g. "a0a1"). */
  def playUci(uci: String): Either[String, Position] =
    if uci.length != 4 then Left(s"Invalid UCI: $uci")
    else
      val fromKey = uci.take(2)
      val toKey   = uci.drop(2)
      for
        from <- Square.fromKey(fromKey).toRight(s"Invalid square: $fromKey")
        to   <- Square.fromKey(toKey).toRight(s"Invalid square: $toKey")
        piece <- board(from).toRight(s"No piece at $fromKey")
        _     <- Either.cond(piece.color == turn, (), s"Not your turn")
        move   = Move(piece, from, to, board(to))
        next  <- play(move)
      yield next

  def isInCheck: Boolean = MoveGen.isInCheck(board, turn)

  def legalMoves: List[Move] = MoveGen.legalMoves(board, turn)

  def legalDestinations: Map[String, List[String]] = MoveGen.legalDestinations(board, turn)

  /** Is the current player checkmated? */
  def isCheckmate: Boolean = legalMoves.isEmpty && isInCheck

  /**
   * Is the current player stalemated?
   * In Xiangqi, stalemate means the side with no legal moves loses.
   */
  def isStalemate: Boolean = legalMoves.isEmpty && !isInCheck

  /** Game over = no legal moves (checkmate or stalemate) */
  def isGameOver: Boolean = legalMoves.isEmpty

  def winner: Option[Color] =
    if isGameOver then Some(turn.opposite) else None

  def status: Status =
    if isCheckmate then Status.VariantEnd
    else if isStalemate then Status.Stalemate
    else Status.Started

object Position:
  val standard: Position = Position(Board.standard, Color.Red)

  def fromFen(fen: String): Either[String, Position] =
    Fen.read(fen)
