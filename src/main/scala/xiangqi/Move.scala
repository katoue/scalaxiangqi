package xiangqi

case class Move(
    piece: Piece,
    from: Square,
    to: Square,
    capture: Option[Piece] = None
):
  def uci: String = s"${from.key}${to.key}"
  override def toString: String = uci
