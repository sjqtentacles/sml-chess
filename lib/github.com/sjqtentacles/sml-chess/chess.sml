(* chess.sml - 0x88 board, fully legal move generation, FEN/UCI, perft, and a
   small negamax search.

   Squares are 0x88: square = rank*16 + file (file 0='a', rank 0='1'); a square
   is on the board iff it is in [0,127] and its file (sq mod 16) is <= 7. The
   off-board "gap" (files 8..15) makes ray/step generation reject wrap-around
   automatically. Positions are immutable; makeMove copies the 128-cell board. *)

structure Chess :> CHESS =
struct

  datatype color = White | Black
  datatype ptype = Pawn | Knight | Bishop | Rook | Queen | King
  type piece = color * ptype

  type castling = { wk : bool, wq : bool, bk : bool, bq : bool }

  type position =
    { board : piece option array,
      turn : color,
      castle : castling,
      ep : int option,
      half : int,
      full : int }

  type move = { from : int, to : int, promo : ptype option }

  fun opposite White = Black
    | opposite Black = White

  (* ---------------- squares ---------------- *)

  fun square f r = r * 16 + f
  fun fileOf sq = sq mod 16
  fun rankOf sq = sq div 16
  fun onBoard sq = sq >= 0 andalso sq < 128 andalso fileOf sq <= 7

  fun squareToString sq =
    String.str (Char.chr (Char.ord #"a" + fileOf sq))
    ^ Int.toString (rankOf sq + 1)

  fun squareFromString s =
    if String.size s <> 2 then NONE
    else
      let val f = Char.ord (String.sub (s, 0)) - Char.ord #"a"
          val r = Char.ord (String.sub (s, 1)) - Char.ord #"1"
      in if f >= 0 andalso f <= 7 andalso r >= 0 andalso r <= 7
         then SOME (square f r) else NONE
      end

  (* ---------------- board helpers ---------------- *)

  fun copyBoard b = Array.tabulate (128, fn i => Array.sub (b, i))

  fun pieceOn (board : piece option array) s =
    if onBoard s then Array.sub (board, s) else NONE

  val knightD = [33, 31, 18, 14, ~33, ~31, ~18, ~14]
  val kingD   = [1, ~1, 16, ~16, 15, ~15, 17, ~17]
  val bishopD = [15, 17, ~15, ~17]
  val rookD   = [1, ~1, 16, ~16]

  (* is `sq` attacked by any piece of color `by`? *)
  fun isAttacked board sq by =
    let
      fun has s pt =
        case pieceOn board s of
          SOME (c, t) => c = by andalso t = pt
        | NONE => false
      val knight = List.exists (fn d => has (sq + d) Knight) knightD
      val king   = List.exists (fn d => has (sq + d) King) kingD
      val pawn =
        if by = White then has (sq - 15) Pawn orelse has (sq - 17) Pawn
        else has (sq + 15) Pawn orelse has (sq + 17) Pawn
      fun ray dirs pts =
        List.exists
          (fn d =>
             let fun go s =
                   if not (onBoard s) then false
                   else case Array.sub (board, s) of
                          NONE => go (s + d)
                        | SOME (c, t) =>
                            c = by andalso List.exists (fn p => p = t) pts
             in go (sq + d) end)
          dirs
    in
      knight orelse king orelse pawn
      orelse ray bishopD [Bishop, Queen]
      orelse ray rookD [Rook, Queen]
    end

  fun findKing board c =
    let fun go s =
          if s >= 128 then ~1
          else case Array.sub (board, s) of
                 SOME (c', King) => if c' = c then s else go (s + 1)
               | _ => go (s + 1)
    in go 0 end

  (* ---------------- make move ---------------- *)

  fun makeMove (pos : position) (m : move) : position =
    let
      val { board, turn, castle, ep, half, full } = pos
      val b = copyBoard board
      val from = #from m
      val to = #to m
      val (pcol, pt) = valOf (Array.sub (board, from))
      val isPawn = pt = Pawn
      val isKing = pt = King
      val epCapture =
        isPawn andalso (SOME to = ep) andalso fileOf from <> fileOf to
      val captured =
        (case Array.sub (board, to) of SOME _ => true | NONE => false)
        orelse epCapture

      val () = Array.update (b, from, NONE)
      val placed = case #promo m of SOME prom => (pcol, prom) | NONE => (pcol, pt)
      val () = Array.update (b, to, SOME placed)
      val () =
        if epCapture then
          Array.update (b, (if pcol = White then to - 16 else to + 16), NONE)
        else ()
      (* castling: move the rook *)
      val () =
        if isKing andalso to - from = 2 then
          (Array.update (b, from + 1, Array.sub (b, from + 3));
           Array.update (b, from + 3, NONE))
        else if isKing andalso from - to = 2 then
          (Array.update (b, from - 1, Array.sub (b, from - 4));
           Array.update (b, from - 4, NONE))
        else ()

      (* castling-rights update *)
      val c0 = castle
      val c1 =
        if isKing then
          (if pcol = White
           then { wk = false, wq = false, bk = #bk c0, bq = #bq c0 }
           else { wk = #wk c0, wq = #wq c0, bk = false, bq = false })
        else c0
      fun clearSq sq (c : castling) =
        if sq = 0 then { wk = #wk c, wq = false, bk = #bk c, bq = #bq c }
        else if sq = 7 then { wk = false, wq = #wq c, bk = #bk c, bq = #bq c }
        else if sq = 112 then { wk = #wk c, wq = #wq c, bk = #bk c, bq = false }
        else if sq = 119 then { wk = #wk c, wq = #wq c, bk = false, bq = #bq c }
        else c
      val c2 = clearSq from (clearSq to c1)

      val ep' =
        if isPawn andalso Int.abs (to - from) = 32
        then SOME ((from + to) div 2) else NONE
      val half' = if isPawn orelse captured then 0 else half + 1
      val full' = if turn = Black then full + 1 else full
    in
      { board = b, turn = opposite turn, castle = c2,
        ep = ep', half = half', full = full' }
    end

  (* ---------------- pseudo-legal generation ---------------- *)

  fun pseudoMoves (pos : position) : move list =
    let
      val { board, turn, castle, ep, ... } = pos
      val acc = ref ([] : move list)
      fun emit f t p = acc := { from = f, to = t, promo = p } :: !acc
      fun add f t = emit f t NONE
      fun addPromos f t =
        (emit f t (SOME Queen); emit f t (SOME Rook);
         emit f t (SOME Bishop); emit f t (SOME Knight))
      fun emptyAt t = onBoard t andalso pieceOn board t = NONE

      val (forward, startRank, promoRank, capDirs) =
        if turn = White then (16, 1, 7, [15, 17]) else (~16, 6, 0, [~15, ~17])

      fun genPawn s =
        let val one = s + forward in
          (if emptyAt one then
             (if rankOf one = promoRank then addPromos s one else add s one;
              if rankOf s = startRank andalso emptyAt (s + 2 * forward)
              then add s (s + 2 * forward) else ())
           else ());
          List.app
            (fn d =>
               let val t = s + d in
                 if onBoard t then
                   (case pieceOn board t of
                      SOME (c, _) =>
                        if c <> turn then
                          (if rankOf t = promoRank then addPromos s t else add s t)
                        else ()
                    | NONE => if SOME t = ep then add s t else ())
                 else ()
               end)
            capDirs
        end

      fun genStep s dirs =
        List.app
          (fn d =>
             let val t = s + d in
               if onBoard t then
                 (case pieceOn board t of
                    NONE => add s t
                  | SOME (c, _) => if c <> turn then add s t else ())
               else ()
             end)
          dirs

      fun genSlide s dirs =
        List.app
          (fn d =>
             let fun go t =
                   if not (onBoard t) then ()
                   else case Array.sub (board, t) of
                          NONE => (add s t; go (t + d))
                        | SOME (c, _) => if c <> turn then add s t else ()
             in go (s + d) end)
          dirs

      fun genPiece s (c, pt) =
        if c <> turn then ()
        else (case pt of
                Pawn => genPawn s
              | Knight => genStep s knightD
              | King => genStep s kingD
              | Bishop => genSlide s bishopD
              | Rook => genSlide s rookD
              | Queen => genSlide s (bishopD @ rookD))

      fun scan s =
        if s >= 128 then ()
        else (if onBoard s then
                (case Array.sub (board, s) of SOME p => genPiece s p | NONE => ())
              else ();
              scan (s + 1))

      fun genCastling () =
        let
          val opp = opposite turn
          fun safe sq = not (isAttacked board sq opp)
          fun isRook sq c =
            case pieceOn board sq of SOME (c', Rook) => c' = c | _ => false
          fun isKingAt sq c =
            case pieceOn board sq of SOME (c', King) => c' = c | _ => false
        in
          if turn = White then
            (if #wk castle andalso isKingAt 4 White andalso isRook 7 White
                andalso emptyAt 5 andalso emptyAt 6
                andalso safe 4 andalso safe 5 andalso safe 6
             then add 4 6 else ();
             if #wq castle andalso isKingAt 4 White andalso isRook 0 White
                andalso emptyAt 1 andalso emptyAt 2 andalso emptyAt 3
                andalso safe 4 andalso safe 3 andalso safe 2
             then add 4 2 else ())
          else
            (if #bk castle andalso isKingAt 116 Black andalso isRook 119 Black
                andalso emptyAt 117 andalso emptyAt 118
                andalso safe 116 andalso safe 117 andalso safe 118
             then add 116 118 else ();
             if #bq castle andalso isKingAt 116 Black andalso isRook 112 Black
                andalso emptyAt 113 andalso emptyAt 114 andalso emptyAt 115
                andalso safe 116 andalso safe 115 andalso safe 114
             then add 116 114 else ())
        end
    in
      scan 0; genCastling (); List.rev (!acc)
    end

  (* ---------------- legality / queries ---------------- *)

  fun inCheckColor (pos : position) c =
    isAttacked (#board pos) (findKing (#board pos) c) (opposite c)

  fun inCheck (pos : position) = inCheckColor pos (#turn pos)

  fun legalMoves (pos : position) : move list =
    let val mover = #turn pos
    in
      List.filter
        (fn m =>
           let val pos' = makeMove pos m
           in not (isAttacked (#board pos') (findKing (#board pos') mover)
                              (opposite mover))
           end)
        (pseudoMoves pos)
    end

  fun isCheckmate pos = inCheck pos andalso null (legalMoves pos)
  fun isStalemate pos = not (inCheck pos) andalso null (legalMoves pos)

  fun perft pos 0 = 1
    | perft pos 1 = List.length (legalMoves pos)
    | perft pos d =
        List.foldl (fn (m, acc) => acc + perft (makeMove pos m) (d - 1))
                   0 (legalMoves pos)

  (* ---------------- UCI ---------------- *)

  fun promoChar Queen = "q"
    | promoChar Rook = "r"
    | promoChar Bishop = "b"
    | promoChar Knight = "n"
    | promoChar _ = ""

  fun moveToUci (m : move) =
    squareToString (#from m) ^ squareToString (#to m)
    ^ (case #promo m of SOME p => promoChar p | NONE => "")

  fun charToPromo #"q" = SOME Queen
    | charToPromo #"r" = SOME Rook
    | charToPromo #"b" = SOME Bishop
    | charToPromo #"n" = SOME Knight
    | charToPromo _ = NONE

  fun moveFromUci s =
    let val n = String.size s in
      if n <> 4 andalso n <> 5 then NONE
      else
        case (squareFromString (String.substring (s, 0, 2)),
              squareFromString (String.substring (s, 2, 2))) of
          (SOME f, SOME t) =>
            if n = 4 then SOME { from = f, to = t, promo = NONE }
            else (case charToPromo (String.sub (s, 4)) of
                    SOME p => SOME { from = f, to = t, promo = SOME p }
                  | NONE => NONE)
        | _ => NONE
    end

  fun parseMove pos s =
    case moveFromUci s of
      NONE => NONE
    | SOME m =>
        List.find
          (fn lm => #from lm = #from m andalso #to lm = #to m
                    andalso #promo lm = #promo m)
          (legalMoves pos)

  (* ---------------- FEN ---------------- *)

  fun pieceToChar (White, Pawn) = #"P" | pieceToChar (White, Knight) = #"N"
    | pieceToChar (White, Bishop) = #"B" | pieceToChar (White, Rook) = #"R"
    | pieceToChar (White, Queen) = #"Q" | pieceToChar (White, King) = #"K"
    | pieceToChar (Black, Pawn) = #"p" | pieceToChar (Black, Knight) = #"n"
    | pieceToChar (Black, Bishop) = #"b" | pieceToChar (Black, Rook) = #"r"
    | pieceToChar (Black, Queen) = #"q" | pieceToChar (Black, King) = #"k"

  exception BadFen

  fun charToPiece c =
    case c of
      #"P" => (White, Pawn) | #"N" => (White, Knight) | #"B" => (White, Bishop)
    | #"R" => (White, Rook) | #"Q" => (White, Queen) | #"K" => (White, King)
    | #"p" => (Black, Pawn) | #"n" => (Black, Knight) | #"b" => (Black, Bishop)
    | #"r" => (Black, Rook) | #"q" => (Black, Queen) | #"k" => (Black, King)
    | _ => raise BadFen

  fun parseFen s =
    (let
       val fields = String.tokens (fn c => c = #" ") s
       val (boardF, turnF, castF, epF, rest) =
         case fields of
           (a :: b :: c :: d :: r) => (a, b, c, d, r)
         | _ => raise BadFen
       val board = Array.array (128, NONE)
       val ranks = String.fields (fn c => c = #"/") boardF
       val () = if List.length ranks <> 8 then raise BadFen else ()
       fun fillRank (rankStr, rankIdx) =
         let
           fun loop (i, file) =
             if i >= String.size rankStr then
               (if file <> 8 then raise BadFen else ())
             else
               let val ch = String.sub (rankStr, i) in
                 if Char.isDigit ch then
                   loop (i + 1, file + (Char.ord ch - Char.ord #"0"))
                 else
                   (if file > 7 then raise BadFen else ();
                    Array.update (board, square file rankIdx, SOME (charToPiece ch));
                    loop (i + 1, file + 1))
               end
         in loop (0, 0) end
       val _ = List.foldl
                 (fn (rs, i) => (fillRank (rs, 7 - i); i + 1)) 0 ranks
       val turn = case turnF of "w" => White | "b" => Black | _ => raise BadFen
       val castle =
         if castF = "-" then { wk = false, wq = false, bk = false, bq = false }
         else
           let
             fun has c = CharVector.exists (fn x => x = c) castF
             val () = CharVector.app
                        (fn c => if c = #"K" orelse c = #"Q" orelse c = #"k"
                                    orelse c = #"q"
                                 then () else raise BadFen) castF
           in { wk = has #"K", wq = has #"Q", bk = has #"k", bq = has #"q" } end
       val ep = if epF = "-" then NONE
                else (case squareFromString epF of
                        SOME sq => SOME sq | NONE => raise BadFen)
       (* Parse a FEN move counter via `IntInf`, bounds-checked into the fixed
          32-bit range.  `Int.fromString` raises `Overflow` past 2^31 on MLton
          (32-bit int) but not on Poly/ML (63-bit int); routing through `IntInf`
          and raising `BadFen` on out-of-range counters (which `parseFen`
          surfaces as `NONE`) keeps decoding total and identical on both. *)
       fun counter s =
         case IntInf.fromString s of
           SOME n =>
             if n >= ~2147483648 andalso n <= 2147483647
             then IntInf.toInt n
             else raise BadFen
         | NONE => raise BadFen
       val (half, full) =
         case rest of
           [] => (0, 1)
         | [h] => (counter h, 1)
         | (h :: f :: _) => (counter h, counter f)
     in
       SOME { board = board, turn = turn, castle = castle,
              ep = ep, half = half, full = full }
     end)
    handle _ => NONE

  fun toFen (pos : position) =
    let
      val { board, turn, castle, ep, half, full } = pos
      fun rankStr r =
        let
          fun loop (file, run, acc) =
            if file > 7 then
              acc ^ (if run > 0 then Int.toString run else "")
            else
              case pieceOn board (square file r) of
                NONE => loop (file + 1, run + 1, acc)
              | SOME p =>
                  loop (file + 1, 0,
                        acc ^ (if run > 0 then Int.toString run else "")
                        ^ String.str (pieceToChar p))
        in loop (0, 0, "") end
      val boardF =
        String.concatWith "/"
          (List.tabulate (8, fn i => rankStr (7 - i)))
      val turnF = if turn = White then "w" else "b"
      val castF =
        let
          val s = (if #wk castle then "K" else "")
                  ^ (if #wq castle then "Q" else "")
                  ^ (if #bk castle then "k" else "")
                  ^ (if #bq castle then "q" else "")
        in if s = "" then "-" else s end
      val epF = case ep of NONE => "-" | SOME sq => squareToString sq
    in
      String.concatWith " "
        [boardF, turnF, castF, epF, Int.toString half, Int.toString full]
    end

  val startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  val startPos = valOf (parseFen startFen)

  fun pieceAt (pos : position) sq = pieceOn (#board pos) sq
  fun sideToMove (pos : position) = #turn pos

  (* ---------------- search (bonus) ---------------- *)

  fun pieceValue Pawn = 100 | pieceValue Knight = 320
    | pieceValue Bishop = 330 | pieceValue Rook = 500
    | pieceValue Queen = 900 | pieceValue King = 0

  fun evaluate (pos : position) =
    let
      val { board, turn, ... } = pos
      fun loop (s, acc) =
        if s >= 128 then acc
        else
          let val acc' =
                if onBoard s then
                  (case Array.sub (board, s) of
                     NONE => acc
                   | SOME (c, pt) =>
                       acc + (if c = White then pieceValue pt
                              else ~ (pieceValue pt)))
                else acc
          in loop (s + 1, acc') end
      val material = loop (0, 0)
    in if turn = White then material else ~material end

  val mateScore = 1000000

  fun negamax pos depth =
    if depth = 0 then evaluate pos
    else
      case legalMoves pos of
        [] => if inCheck pos then ~ (mateScore + depth) else 0
      | ms =>
          List.foldl
            (fn (m, best) =>
               let val v = ~ (negamax (makeMove pos m) (depth - 1))
               in if v > best then v else best end)
            (~ (mateScore * 2)) ms

  fun bestMove pos depth =
    case legalMoves pos of
      [] => NONE
    | ms =>
        let
          val scored =
            List.map (fn m => (m, ~ (negamax (makeMove pos m) (depth - 1)))) ms
          val best =
            List.foldl
              (fn ((m, s), acc) =>
                 case acc of
                   NONE => SOME (m, s)
                 | SOME (_, bs) => if s > bs then SOME (m, s) else acc)
              NONE scored
        in case best of SOME (m, _) => SOME m | NONE => NONE end
end
