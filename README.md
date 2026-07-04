# sml-chess

[![CI](https://github.com/sjqtentacles/sml-chess/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-chess/actions/workflows/ci.yml)

A pure Standard ML chess engine core: **fully-legal move generation**, **perft**,
**FEN** parsing/printing, **UCI** move (de)serialization, and a small
material-based **negamax** search.

No dependencies, no FFI, no threads, no clock, no randomness: the same position
always yields the same moves, perft counts, and search result under **MLton**
and **Poly/ML**. Positions are immutable values — `makeMove` returns a fresh
position and never mutates its argument.

Move generation is *fully legal*: pawn pushes/captures, double pushes,
en passant, and promotions; castling with all legality conditions; check
detection; and pinned pieces (a move that would leave one's own king in check
is never produced). Correctness is pinned down by **perft** reference vectors.

## Board representation

The board is a **0x88** array: a square is `rank * 16 + file`, with file and
rank in `0..7` (file 0 = `a`, rank 0 = rank `1`). A square `s` is on the board
iff `s` is in `[0, 127]` and `s mod 16 <= 7`. The off-board "gap" (files 8..15)
makes ray/step generation reject wrap-around automatically.

```sml
datatype color = White | Black
datatype ptype = Pawn | Knight | Bishop | Rook | Queen | King
type piece = color * ptype
type move  = { from : int, to : int, promo : ptype option }
```

Castling is encoded as the king's two-square move (e.g. `e1g1`); en passant as
the capturing pawn's diagonal move. Promotions set the `promo` field.

## API

```sml
structure Chess : sig
  datatype color = White | Black
  datatype ptype = Pawn | Knight | Bishop | Rook | Queen | King
  type piece = color * ptype
  type position
  type move = { from : int, to : int, promo : ptype option }

  val square : int -> int -> int          (* file, rank -> 0x88 square *)
  val fileOf : int -> int
  val rankOf : int -> int
  val onBoard : int -> bool
  val squareToString   : int -> string    (* 4 -> "e1" *)
  val squareFromString : string -> int option

  val startPos   : position
  val pieceAt    : position -> int -> piece option
  val sideToMove : position -> color
  val parseFen   : string -> position option   (* NONE on malformed input *)
  val toFen      : position -> string

  val legalMoves  : position -> move list  (* fully legal *)
  val makeMove    : position -> move -> position
  val inCheck     : position -> bool       (* is the side to move in check? *)
  val isCheckmate : position -> bool
  val isStalemate : position -> bool
  val perft       : position -> int -> int (* count leaf nodes at depth *)

  val moveToUci   : move -> string
  val moveFromUci : string -> move option
  val parseMove   : position -> string -> move option  (* validates legality *)

  val bestMove : position -> int -> move option  (* negamax to fixed depth *)
  val evaluate : position -> int                 (* material, side-to-move POV *)
end
```

`parseFen`, `moveFromUci`, and `parseMove` return `option` instead of raising.
`toFen o (valOf o parseFen)` round-trips any well-formed FEN. A halfmove/fullmove
counter outside the fixed 32-bit `int` range yields `NONE` (never `Overflow`),
so `parseFen` behaves identically under MLton (32-bit `int`) and Poly/ML
(63-bit `int`).

## Reference vectors (perft)

Perft (leaf-node counts of the move tree) validates *full* legality. These all
pass under both compilers:

| Position | depth 1 | depth 2 | depth 3 | depth 4 |
|----------|--------:|--------:|--------:|--------:|
| start `rnbqkbnr/…/RNBQKBNR w KQkq - 0 1` | 20 | 400 | 8902 | 197281 |
| Kiwipete `r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1` | 48 | 2039 | 97862 | — |

The full perft(4) from the start position runs under both MLton and Poly/ML in
seconds (no compiler-specific split was needed).

## Example

```sml
val pos  = Chess.startPos
val 20   = Chess.perft pos 1
val 197281 = Chess.perft pos 4

val e4   = valOf (Chess.parseMove pos "e2e4")
val pos' = Chess.makeMove pos e4
(* "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1" *)
```

Running [`examples/demo.sml`](examples/demo.sml) with `make example` prints:

```
Start position:
  FEN: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
  side to move: white
  legal moves: 20

perft from the start position:
  perft(1) = 20
  perft(2) = 400
  perft(3) = 8902
  perft(4) = 197281

perft from Kiwipete:
  perft(1) = 48
  perft(2) = 2039
  perft(3) = 97862

After 1. e4 e5 2. Nf3:
  FEN: rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2
  best reply (depth 2): a7a6
```

## Build & test

Requires [MLton](http://mlton.org/) and/or [Poly/ML](https://polyml.org/).

```sh
make test        # build + run the suite under MLton
make test-poly   # run the suite under Poly/ML
make all-tests   # both
make example     # build + run the demo
make clean
```

## Installing with smlpkg

```sh
smlpkg add github.com/sjqtentacles/sml-chess
smlpkg sync
```

Reference `lib/github.com/sjqtentacles/sml-chess/chess.mlb` from your own `.mlb`
(MLton / MLKit), or feed `sources.mlb` to `tools/polybuild` (Poly/ML).

## Layout

```
sml.pkg                                      smlpkg manifest
Makefile                                     MLton + Poly/ML targets
.github/workflows/ci.yml                     CI: MLton + Poly/ML
lib/github.com/sjqtentacles/sml-chess/
  chess.sig    CHESS signature
  chess.sml    0x88 movegen + FEN/UCI + perft + search
  sources.mlb / chess.mlb
examples/
  demo.sml     start position, perft tables, a short game
test/
  harness.sml  shared assertion harness
  test.sml     perft vectors + FEN/UCI/movegen/search (61 checks)
  entry.sml / main.sml
tools/polybuild  Poly/ML build wrapper
```

## Tests

61 deterministic checks. The core reference vectors are perft counts from the
start position (`perft(4) = 197281`) and Kiwipete (`perft(2) = 2039`,
`perft(3) = 97862`), which exercise every legality rule. Plus: FEN round-trips
and rejection of malformed input (including move counters outside the 32-bit
`int` range, which yield `NONE` rather than raising `Overflow`), en-passant
generation and capture,
under-/over-promotion (four moves, UCI `a7a8q`/`a7a8n`), castling (move +
rook relocation), fool's-mate checkmate detection, a stalemate position, a
pinned-piece test (a pinned bishop yields no legal move), UCI (de)serialization,
and a negamax mate-in-1 search. Run `make all-tests` to verify identical output
under both compilers.

## License

MIT. See [LICENSE](LICENSE).
