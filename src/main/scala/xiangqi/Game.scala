package xiangqi

/**
 * A Xiangqi game with full history.
 */
case class Game(
    position: Position,
    history: List[Move] = Nil,
    status: Status = Status.Started,
    winner: Option[Color] = None
):
  def board: Board = position.board
  def turn: Color  = position.turn
  def plies: Int   = position.plies

  def isOver: Boolean = status.finished

  /** Play a move by UCI string. Returns updated Game or error. */
  def playUci(uci: String): Either[String, Game] =
    if isOver then Left("Game is already over")
    else
      position.playUci(uci).map: next =>
        val newStatus = next.status
        val newWinner = next.winner
        copy(
          position = next,
          history = history :+ next.lastMove.get,
          status = if newStatus == Status.Started && next.isGameOver then
                     Status.VariantEnd
                   else newStatus,
          winner = newWinner
        )

  /** All legal moves for the current player. */
  def legalMoves: List[Move] = position.legalMoves

  /** Legal destination map for UI. */
  def legalDestinations: Map[String, List[String]] = position.legalDestinations

  def fen: String = Fen.write(position)

object Game:
  val standard: Game = Game(Position.standard)

  def fromFen(fen: String): Either[String, Game] =
    Position.fromFen(fen).map(pos => Game(pos))
