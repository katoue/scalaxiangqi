# scalaxiangqi

[![](https://jitpack.io/v/katoue/scalaxiangqi.svg)](https://jitpack.io/#katoue/scalaxiangqi)

A Scala 3 library implementing Chinese Chess (Xiangqi) game logic.

## Features

- Complete Xiangqi game rules
- 9×10 board (files a-i, ranks 0-9)
- All 7 piece types: General, Advisor, Elephant, Horse, Chariot, Cannon, Soldier
- Full legal move generation with:
  - Palace restriction (General, Advisor)
  - River restriction (Elephant)
  - Horse leg-blocking rule
  - Cannon jump-capture rule
  - Flying General rule
  - Check detection
- FEN notation support (UCCI format)
- UCI move format

## Quick Start

```scala
import xiangqi.*

// Start a new game
val game = Game.standard

// Get legal moves
val dests = game.legalDestinations
// Map("a0" -> List("a1", "a2", ...), ...)

// Play a move (UCI format: from+to)
val result = game.playUci("e3e4") // Move soldier forward
result match
  case Right(g) => println(s"Turn: ${g.turn}, FEN: ${g.fen}")
  case Left(err) => println(s"Illegal: $err")

// Load from FEN
val pos = Position.fromFen("rnbakabnr/9/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/1C5C1/9/RNBAKABNR w - - 0 1")
```

## Board Layout

```
Rank 9: r n b a k a b n r  (Black back rank)
Rank 8: . . . . . . . . .
Rank 7: . c . . . . . c .  (Black cannons)
Rank 6: p . p . p . p . p  (Black soldiers)
Rank 5: . . . . . . . . .  (River)
Rank 4: . . . . . . . . .
Rank 3: P . P . P . P . P  (Red soldiers)
Rank 2: . C . . . . . C .  (Red cannons)
Rank 1: . . . . . . . . .
Rank 0: R N B A K A B N R  (Red back rank)
        a b c d e f g h i
```

- Red (uppercase) plays from bottom (ranks 0-4)
- Black (lowercase) plays from top (ranks 5-9)

## Piece Forsyth Characters

| Role | Red | Black | Name |
|------|-----|-------|------|
| General | K | k | 将/帅 |
| Advisor | A | a | 士/仕 |
| Elephant | B | b | 象/相 |
| Horse | N | n | 马/馬 |
| Chariot | R | r | 车/車 |
| Cannon | C | c | 炮/砲 |
| Soldier | P | p | 卒/兵 |

## Building

```bash
sbt compile
sbt test
```

Requires Java 11+, SBT 1.9+, Scala 3.4+.

## License

MIT
