package xiangqi

/**
 * The Xiangqi board: 9x10 grid.
 */
case class Board(pieces: Map[Square, Piece]):

  def apply(sq: Square): Option[Piece] = pieces.get(sq)
  def apply(file: Int, rank: Int): Option[Piece] =
    Square.fromInts(file, rank).flatMap(pieces.get)

  def place(piece: Piece, sq: Square): Board = copy(pieces = pieces + (sq -> piece))
  def remove(sq: Square): Board = copy(pieces = pieces - sq)
  def move(from: Square, to: Square): Board =
    pieces.get(from).fold(this)(p => copy(pieces = pieces - from - to + (to -> p)))

  def isEmpty(sq: Square): Boolean = !pieces.contains(sq)
  def isOccupied(sq: Square): Boolean = pieces.contains(sq)

  def pieceAt(sq: Square): Option[Piece] = pieces.get(sq)

  def piecesOf(color: Color): Map[Square, Piece] = pieces.filter(_._2.color == color)

  def kingSquare(color: Color): Option[Square] =
    pieces.find { case (_, p) => p.color == color && p.role == Role.General }.map(_._1)

  /** All squares between from and to on the same file or rank (exclusive). */
  def squaresBetween(from: Square, to: Square): List[Square] =
    val df = to.file.value - from.file.value
    val dr = to.rank.value - from.rank.value
    if df != 0 && dr != 0 then Nil
    else
      val stepF = df.sign
      val stepR = dr.sign
      val steps = math.max(math.abs(df), math.abs(dr)) - 1
      (1 to steps).flatMap: i =>
        Square.fromInts(from.file.value + stepF * i, from.rank.value + stepR * i)
      .toList

  /** Count pieces between two squares on the same file/rank. */
  def piecesBetween(from: Square, to: Square): Int =
    squaresBetween(from, to).count(isOccupied)

  override def toString: String =
    (9 to 0 by -1).map: r =>
      (0 to 8).map: f =>
        Square.fromInts(f, r).flatMap(pieces.get).map(_.forsyth).getOrElse('.')
      .mkString
    .mkString("\n")

object Board:
  val empty: Board = Board(Map.empty)

  /** Standard Xiangqi starting position.
   *  Red at bottom (ranks 0-4), Black at top (ranks 5-9).
   *  Rank 0 = Red's back rank, Rank 9 = Black's back rank.
   */
  val standard: Board =
    val pieces = Map(
      // Red back rank (rank 0)
      Square(File.unsafe(0), Rank.unsafe(0)) -> Piece(Color.Red, Role.Chariot),
      Square(File.unsafe(1), Rank.unsafe(0)) -> Piece(Color.Red, Role.Horse),
      Square(File.unsafe(2), Rank.unsafe(0)) -> Piece(Color.Red, Role.Elephant),
      Square(File.unsafe(3), Rank.unsafe(0)) -> Piece(Color.Red, Role.Advisor),
      Square(File.unsafe(4), Rank.unsafe(0)) -> Piece(Color.Red, Role.General),
      Square(File.unsafe(5), Rank.unsafe(0)) -> Piece(Color.Red, Role.Advisor),
      Square(File.unsafe(6), Rank.unsafe(0)) -> Piece(Color.Red, Role.Elephant),
      Square(File.unsafe(7), Rank.unsafe(0)) -> Piece(Color.Red, Role.Horse),
      Square(File.unsafe(8), Rank.unsafe(0)) -> Piece(Color.Red, Role.Chariot),
      // Red cannons (rank 2)
      Square(File.unsafe(1), Rank.unsafe(2)) -> Piece(Color.Red, Role.Cannon),
      Square(File.unsafe(7), Rank.unsafe(2)) -> Piece(Color.Red, Role.Cannon),
      // Red soldiers (rank 3)
      Square(File.unsafe(0), Rank.unsafe(3)) -> Piece(Color.Red, Role.Soldier),
      Square(File.unsafe(2), Rank.unsafe(3)) -> Piece(Color.Red, Role.Soldier),
      Square(File.unsafe(4), Rank.unsafe(3)) -> Piece(Color.Red, Role.Soldier),
      Square(File.unsafe(6), Rank.unsafe(3)) -> Piece(Color.Red, Role.Soldier),
      Square(File.unsafe(8), Rank.unsafe(3)) -> Piece(Color.Red, Role.Soldier),
      // Black soldiers (rank 6)
      Square(File.unsafe(0), Rank.unsafe(6)) -> Piece(Color.Black, Role.Soldier),
      Square(File.unsafe(2), Rank.unsafe(6)) -> Piece(Color.Black, Role.Soldier),
      Square(File.unsafe(4), Rank.unsafe(6)) -> Piece(Color.Black, Role.Soldier),
      Square(File.unsafe(6), Rank.unsafe(6)) -> Piece(Color.Black, Role.Soldier),
      Square(File.unsafe(8), Rank.unsafe(6)) -> Piece(Color.Black, Role.Soldier),
      // Black cannons (rank 7)
      Square(File.unsafe(1), Rank.unsafe(7)) -> Piece(Color.Black, Role.Cannon),
      Square(File.unsafe(7), Rank.unsafe(7)) -> Piece(Color.Black, Role.Cannon),
      // Black back rank (rank 9)
      Square(File.unsafe(0), Rank.unsafe(9)) -> Piece(Color.Black, Role.Chariot),
      Square(File.unsafe(1), Rank.unsafe(9)) -> Piece(Color.Black, Role.Horse),
      Square(File.unsafe(2), Rank.unsafe(9)) -> Piece(Color.Black, Role.Elephant),
      Square(File.unsafe(3), Rank.unsafe(9)) -> Piece(Color.Black, Role.Advisor),
      Square(File.unsafe(4), Rank.unsafe(9)) -> Piece(Color.Black, Role.General),
      Square(File.unsafe(5), Rank.unsafe(9)) -> Piece(Color.Black, Role.Advisor),
      Square(File.unsafe(6), Rank.unsafe(9)) -> Piece(Color.Black, Role.Elephant),
      Square(File.unsafe(7), Rank.unsafe(9)) -> Piece(Color.Black, Role.Horse),
      Square(File.unsafe(8), Rank.unsafe(9)) -> Piece(Color.Black, Role.Chariot),
    )
    Board(pieces)
