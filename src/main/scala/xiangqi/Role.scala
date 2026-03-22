package xiangqi

/** Xiangqi piece roles */
enum Role(val forsyth: Char, val forsythFull: String):
  case General  extends Role('k', "general")   // 将/帅
  case Advisor  extends Role('a', "advisor")   // 士/仕
  case Elephant extends Role('b', "elephant")  // 象/相 (bishop initial)
  case Horse    extends Role('n', "horse")     // 马/馬 (knight initial)
  case Chariot  extends Role('r', "chariot")   // 车/車 (rook initial)
  case Cannon   extends Role('c', "cannon")    // 炮/砲
  case Soldier  extends Role('p', "soldier")   // 卒/兵 (pawn initial)

object Role:
  val all: List[Role] = Role.values.toList
  val byForsyth: Map[Char, Role] = all.map(r => r.forsyth -> r).toMap
  val byFull: Map[String, Role] = all.map(r => r.forsythFull -> r).toMap

  def forsyth(c: Char): Option[Role] = byForsyth.get(c.toLower)
