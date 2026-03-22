package xiangqi

/**
 * Xiangqi board: 9 files (a-i) x 10 ranks (0-9, 0=Red's back rank).
 * Red plays from ranks 0-4 (bottom), Black from ranks 5-9 (top).
 * Palace: files c-e (2-4), ranks 0-2 (Red) and 7-9 (Black).
 */
opaque type File = Int // 0-8 (a-i)
opaque type Rank = Int // 0-9

object File:
  def apply(i: Int): Option[File] = Option.when(i >= 0 && i <= 8)(i)
  def unsafe(i: Int): File = i
  extension (f: File)
    def value: Int = f
    def char: Char = ('a' + f).toChar
    def +(n: Int): Option[File] = File(f + n)
    def -(n: Int): Option[File] = File(f - n)
  val all: List[File] = (0 to 8).map(File.unsafe).toList

object Rank:
  def apply(i: Int): Option[Rank] = Option.when(i >= 0 && i <= 9)(i)
  def unsafe(i: Int): Rank = i
  extension (r: Rank)
    def value: Int = r
    def char: Char = ('0' + r).toChar
    def +(n: Int): Option[Rank] = Rank(r + n)
    def -(n: Int): Option[Rank] = Rank(r - n)
  val all: List[Rank] = (0 to 9).map(Rank.unsafe).toList

case class Square(file: File, rank: Rank):
  def key: String = s"${file.char}${rank.char}"

  /** Is this square in Red's palace? (files c-e, ranks 0-2) */
  def inRedPalace: Boolean =
    file.value >= 2 && file.value <= 4 && rank.value >= 0 && rank.value <= 2

  /** Is this square in Black's palace? (files c-e, ranks 7-9) */
  def inBlackPalace: Boolean =
    file.value >= 2 && file.value <= 4 && rank.value >= 7 && rank.value <= 9

  def inPalace(color: Color): Boolean = color match
    case Color.Red   => inRedPalace
    case Color.Black => inBlackPalace

  /** Is this square on Red's side? (ranks 0-4) */
  def onRedSide: Boolean = rank.value <= 4

  /** Is this square on Black's side? (ranks 5-9) */
  def onBlackSide: Boolean = rank.value >= 5

  /** Has this soldier crossed the river? */
  def soldierHasCrossed(color: Color): Boolean = color match
    case Color.Red   => rank.value >= 5 // Red soldiers advance toward rank 9
    case Color.Black => rank.value <= 4 // Black soldiers advance toward rank 0

  def +(df: Int, dr: Int): Option[Square] =
    for
      f <- File(file.value + df)
      r <- Rank(rank.value + dr)
    yield Square(f, r)

  def neighbors(dfs: List[(Int, Int)]): List[Square] =
    dfs.flatMap((df, dr) => this.+(df, dr))

object Square:
  val all: List[Square] =
    for
      r <- Rank.all
      f <- File.all
    yield Square(f, r)

  val allByKey: Map[String, Square] = all.map(s => s.key -> s).toMap

  def fromKey(key: String): Option[Square] = allByKey.get(key)

  def fromInts(file: Int, rank: Int): Option[Square] =
    for
      f <- File(file)
      r <- Rank(rank)
    yield Square(f, r)
