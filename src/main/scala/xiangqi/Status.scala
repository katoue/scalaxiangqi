package xiangqi

enum Status(val id: Int):
  case Created    extends Status(10)
  case Started    extends Status(20)
  case Aborted    extends Status(25)
  case Resign     extends Status(31)
  case Stalemate  extends Status(32) // In Xiangqi, stalemate = loss for the stalemated side
  case Timeout    extends Status(33)
  case Draw       extends Status(34)
  case Outoftime  extends Status(35)
  case Cheat      extends Status(36)
  case NoStart    extends Status(37)
  case UnknownFinish extends Status(38)
  case VariantEnd extends Status(60)

  def finished: Boolean = id >= 25
  def playing: Boolean  = !finished

object Status:
  val byId: Map[Int, Status] = values.toList.map(s => s.id -> s).toMap
  def apply(id: Int): Option[Status] = byId.get(id)
