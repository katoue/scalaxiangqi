package xiangqi

/**
 * Xiangqi move generation. Computes pseudo-legal and legal moves.
 */
object MoveGen:

  /** Generate all pseudo-legal destination squares for a piece. */
  def destinations(board: Board, from: Square, piece: Piece): List[Square] =
    piece.role match
      case Role.General  => generalMoves(board, from, piece.color)
      case Role.Advisor  => advisorMoves(board, from, piece.color)
      case Role.Elephant => elephantMoves(board, from, piece.color)
      case Role.Horse    => horseMoves(board, from)
      case Role.Chariot  => chariotMoves(board, from)
      case Role.Cannon   => cannonMoves(board, from)
      case Role.Soldier  => soldierMoves(board, from, piece.color)

  /** General moves one step orthogonally, stays in palace. */
  private def generalMoves(board: Board, from: Square, color: Color): List[Square] =
    val dests = from.neighbors(List((0, 1), (0, -1), (1, 0), (-1, 0)))
    dests.filter: sq =>
      sq.inPalace(color) && board(sq).forall(_.color != color)

  /** Advisor moves one step diagonally, stays in palace. */
  private def advisorMoves(board: Board, from: Square, color: Color): List[Square] =
    val dests = from.neighbors(List((1, 1), (1, -1), (-1, 1), (-1, -1)))
    dests.filter: sq =>
      sq.inPalace(color) && board(sq).forall(_.color != color)

  /**
   * Elephant moves exactly 2 steps diagonally, cannot cross the river,
   * and is blocked if the "elbow" square is occupied.
   */
  private def elephantMoves(board: Board, from: Square, color: Color): List[Square] =
    val steps = List((2, 2), (2, -2), (-2, 2), (-2, -2))
    steps.flatMap: (df, dr) =>
      val elbow = from.+(df / 2, dr / 2)
      val dest = from.+(df, dr)
      for
        el  <- elbow
        dst <- dest
        if board.isEmpty(el)
        if board(dst).forall(_.color != color)
        if color == Color.Red && dst.onRedSide || color == Color.Black && dst.onBlackSide
      yield dst

  /**
   * Horse moves in an "L" shape: one step orthogonal then one step diagonal.
   * Blocked if the first step square is occupied (leg block rule).
   */
  private def horseMoves(board: Board, from: Square): List[Square] =
    val pieceColor = board(from).map(_.color)
    // (legStep, destinations)
    val legs = List(
      (0, 1)  -> List((1, 2), (-1, 2)),
      (0, -1) -> List((1, -2), (-1, -2)),
      (1, 0)  -> List((2, 1), (2, -1)),
      (-1, 0) -> List((-2, 1), (-2, -1))
    )
    legs.flatMap: (leg, dests) =>
      val legSq = from.+(leg._1, leg._2)
      legSq match
        case None => Nil
        case Some(l) if board.isOccupied(l) => Nil
        case _ =>
          dests.flatMap: (df, dr) =>
            from.+(df, dr).filter: dst =>
              board(dst).forall(p => pieceColor.forall(c => p.color != c))

  /** Chariot moves any number of steps orthogonally, blocked by pieces. */
  private def chariotMoves(board: Board, from: Square): List[Square] =
    val piece = board(from).get
    orthogonalSlide(board, from, piece.color)

  /**
   * Cannon moves like the Chariot but must jump over exactly one piece to capture.
   * Without capture: moves orthogonally to empty squares (no jumping).
   */
  private def cannonMoves(board: Board, from: Square): List[Square] =
    val piece = board(from).get
    val dirs = List((1, 0), (-1, 0), (0, 1), (0, -1))
    dirs.flatMap: (df, dr) =>
      cannonLine(board, from, df, dr, piece.color)

  private def cannonLine(board: Board, from: Square, df: Int, dr: Int, color: Color): List[Square] =
    var cur = from.+(df, dr)
    var result = List.empty[Square]
    var jumpedOver = false
    while cur.isDefined do
      val sq = cur.get
      board(sq) match
        case None =>
          if !jumpedOver then result = sq :: result
          cur = sq.+(df, dr)
        case Some(p) =>
          if !jumpedOver then
            jumpedOver = true
            cur = sq.+(df, dr)
          else
            if p.color != color then result = sq :: result
            cur = None // can only capture once
    result

  /** Soldier moves forward one step; after crossing river, can also move sideways. */
  private def soldierMoves(board: Board, from: Square, color: Color): List[Square] =
    val forward = if color == Color.Red then (0, 1) else (0, -1)
    val dests = if from.soldierHasCrossed(color) then
      List(forward, (1, 0), (-1, 0))
    else
      List(forward)
    from.neighbors(dests).filter: sq =>
      board(sq).forall(_.color != color)

  /** Slide orthogonally until blocked. */
  private def orthogonalSlide(board: Board, from: Square, color: Color): List[Square] =
    val dirs = List((1, 0), (-1, 0), (0, 1), (0, -1))
    dirs.flatMap: (df, dr) =>
      raySlide(board, from, df, dr, color)

  private def raySlide(board: Board, from: Square, df: Int, dr: Int, color: Color): List[Square] =
    var cur = from.+(df, dr)
    var result = List.empty[Square]
    var blocked = false
    while cur.isDefined && !blocked do
      val sq = cur.get
      board(sq) match
        case None =>
          result = sq :: result
          cur = sq.+(df, dr)
        case Some(p) =>
          if p.color != color then result = sq :: result
          blocked = true
    result

  /** Generate all pseudo-legal moves for a color. */
  def pseudoLegal(board: Board, color: Color): List[Move] =
    board.piecesOf(color).toList.flatMap: (from, piece) =>
      destinations(board, from, piece).map: to =>
        Move(piece, from, to, board(to))

  /** Apply a move to the board. */
  def applyMove(board: Board, move: Move): Board =
    board.move(move.from, move.to)

  /** Is the given color's General in check? */
  def isInCheck(board: Board, color: Color): Boolean =
    board.kingSquare(color) match
      case None    => false // no king (shouldn't happen in valid game)
      case Some(kSq) =>
        // Is there an enemy piece attacking kSq?
        val enemy = color.opposite
        board.piecesOf(enemy).exists: (from, piece) =>
          destinations(board, from, piece).contains(kSq)

  /**
   * Flying General rule: the two Generals cannot face each other with no pieces between them.
   * Returns true if the position violates this rule.
   */
  def flyingGeneral(board: Board): Boolean =
    (board.kingSquare(Color.Red), board.kingSquare(Color.Black)) match
      case (Some(rk), Some(bk)) if rk.file.value == bk.file.value =>
        board.piecesBetween(rk, bk) == 0
      case _ => false

  /** A position is illegal if the side that just moved left their own king in check,
   *  or the flying general rule is violated. */
  def isLegalPosition(board: Board, movedColor: Color): Boolean =
    !isInCheck(board, movedColor) && !flyingGeneral(board)

  /** Generate all legal moves for a color. */
  def legalMoves(board: Board, color: Color): List[Move] =
    pseudoLegal(board, color).filter: move =>
      val after = applyMove(board, move)
      isLegalPosition(after, color)

  /** Map from square key to list of legal destination square keys (for UI). */
  def legalDestinations(board: Board, color: Color): Map[String, List[String]] =
    legalMoves(board, color)
      .groupBy(_.from.key)
      .map((from, moves) => from -> moves.map(_.to.key))

