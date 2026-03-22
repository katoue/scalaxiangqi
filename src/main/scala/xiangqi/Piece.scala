package xiangqi

case class Piece(color: Color, role: Role):
  def is(c: Color): Boolean = color == c
  def is(r: Role): Boolean = role == r
  def isNot(c: Color): Boolean = color != c

  /** Forsyth char: uppercase for Red, lowercase for Black */
  def forsyth: Char =
    val c = role.forsyth
    if color == Color.Red then c.toUpper else c

  override def toString: String = s"${color.toString.head}${role.forsyth.toUpper}"

object Piece:
  def fromForsyth(c: Char): Option[Piece] =
    Role.forsyth(c).map: role =>
      val color = if c.isUpper then Color.Red else Color.Black
      Piece(color, role)
