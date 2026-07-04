(* chess.sig

   A pure Standard ML chess move generator: full legal move generation
   (pawn pushes/captures/double/en-passant/promotion, castling with legality,
   check detection, pinned pieces), FEN parsing/printing, UCI move
   (de)serialization, perft, and a small negamax search with a material
   evaluation.

   The board is a 0x88 array: a square is `rank * 16 + file` with file and rank
   in 0..7 (file 0 = 'a', rank 0 = rank '1'). A square `s` is on the board iff
   `Int.andb (s, 0x88) = 0`. Positions are immutable values; `makeMove` returns
   a fresh position and never mutates its argument.

   Nothing here uses FFI, threads, the clock, or randomness: the same position
   always yields the same moves, perft counts, and search result under MLton
   and Poly/ML. Decoders (`parseFen`, `moveFromUci`, `parseMove`) return `option`
   rather than raising. *)

signature CHESS =
sig
  datatype color = White | Black
  datatype ptype = Pawn | Knight | Bishop | Rook | Queen | King
  type piece = color * ptype

  type position

  (* A move in coordinate form: source/destination 0x88 squares plus an
     optional promotion piece. Castling is encoded as the king's two-square
     move; en passant as the capturing pawn's diagonal move. *)
  type move = { from : int, to : int, promo : ptype option }

  (* ---- squares ---- *)
  val square        : int -> int -> int      (* file, rank -> 0x88 square *)
  val fileOf        : int -> int
  val rankOf        : int -> int
  val onBoard       : int -> bool
  val squareToString : int -> string         (* 36 -> "e1" ... *)
  val squareFromString : string -> int option (* "e4" -> SOME 68 *)

  (* ---- positions ---- *)
  val startPos   : position
  val pieceAt    : position -> int -> piece option
  val sideToMove : position -> color

  (* FEN: `parseFen` returns NONE on malformed input -- including a
     halfmove/fullmove counter outside the fixed 32-bit `int` range, so it is
     total and identical under MLton and Poly/ML (never raising `Overflow`).
     `toFen o parseFen` round-trips any well-formed FEN. *)
  val parseFen : string -> position option
  val toFen    : position -> string

  (* ---- move generation ---- *)
  val legalMoves  : position -> move list     (* fully legal moves *)
  val makeMove    : position -> move -> position
  val inCheck     : position -> bool          (* is the side to move in check? *)
  val isCheckmate : position -> bool
  val isStalemate : position -> bool

  (* count leaf nodes of the move tree at the given depth *)
  val perft : position -> int -> int

  (* ---- UCI move (de)serialization ---- *)
  val moveToUci   : move -> string                  (* {e2,e4,NONE} -> "e2e4" *)
  val moveFromUci : string -> move option           (* "e7e8q" -> SOME {..} *)
  (* parse a UCI move and validate it against a position's legal moves *)
  val parseMove   : position -> string -> move option

  (* ---- simple search (bonus) ---- *)
  (* negamax with material evaluation to the given fixed depth; NONE when
     there are no legal moves. Deterministic tie-breaking by move order. *)
  val bestMove : position -> int -> move option
  (* material balance from the side-to-move's perspective, in centipawns *)
  val evaluate : position -> int
end
