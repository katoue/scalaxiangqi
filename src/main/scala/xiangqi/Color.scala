package xiangqi

enum Color:
  case Red, Black

  def opposite: Color = this match
    case Red   => Black
    case Black => Red

  def fold[A](red: => A, black: => A): A = this match
    case Red   => red
    case Black => black

  override def toString: String = this match
    case Red   => "red"
    case Black => "black"

object Color:
  val all: List[Color] = List(Red, Black)
  def fromBoolean(b: Boolean): Color = if b then Red else Black
