(* Tests for sml-chess: 0x88 board, fully-legal move generation, FEN/UCI,
   perft, and search. The critical reference vectors are perft counts from the
   standard start position and the "Kiwipete" position (these validate full
   legality: castling, en passant, promotion, pins, and check evasion). *)

structure Tests =
struct
  open Harness
  structure C = Chess

  fun hasUci ms u = List.exists (fn m => C.moveToUci m = u) ms

  val startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  val kiwiFen  =
      "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
  val foolsMate = "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3"
  val stalemate = "7k/5Q2/6K1/8/8/8/8/8 b - - 0 1"

  fun fen s = valOf (C.parseFen s)

  fun runAll () =
    let
      val () = section "squares"
      val () = checkInt "square e1" (4, C.square 4 0)
      val () = checkString "squareToString e1" ("e1", C.squareToString (C.square 4 0))
      val () = checkString "squareToString h8" ("h8", C.squareToString (C.square 7 7))
      val () = checkBool "squareFromString e4" (true, C.squareFromString "e4" = SOME (C.square 4 3))
      val () = checkBool "squareFromString a1" (true, C.squareFromString "a1" = SOME 0)
      val () = checkBool "squareFromString junk" (true, C.squareFromString "z9" = NONE)
      val () = checkBool "onBoard 0" (true, C.onBoard 0)
      val () = checkBool "onBoard 119 (h8)" (true, C.onBoard 119)
      val () = checkBool "off-board 8" (false, C.onBoard 8)

      val () = section "FEN round-trips"
      val () = checkString "start toFen" (startFen, C.toFen C.startPos)
      val () = checkString "start parse/print" (startFen, C.toFen (fen startFen))
      val () = checkString "kiwipete round-trip" (kiwiFen, C.toFen (fen kiwiFen))
      val () = checkString "fool's-mate round-trip" (foolsMate, C.toFen (fen foolsMate))
      val () = checkString "stalemate round-trip" (stalemate, C.toFen (fen stalemate))
      val () = checkBool "parseFen empty -> NONE" (false, isSome (C.parseFen ""))
      val () = checkBool "parseFen garbage -> NONE" (false, isSome (C.parseFen "not a fen"))
      val () = checkBool "parseFen bad ranks -> NONE"
                 (false, isSome (C.parseFen "rnbqkbnr/8 w - - 0 1"))
      (* A FEN move counter outside the fixed 32-bit `int` range must parse to
         `NONE` on every compiler -- never raise `Overflow`.  MLton's 32-bit
         `Int.fromString` raises past 2^31 (caught as `BadFen`) while Poly/ML's
         63-bit int silently accepts it, so without a bounds check the two
         compilers disagree.  Out-of-range counters now `raise BadFen`, which
         `parseFen` surfaces as its documented `NONE`. *)
      val () = checkBool "parseFen huge halfmove -> NONE"
                 (false, isSome
                    (C.parseFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 999999999999 1"))
      val () = checkBool "parseFen huge fullmove -> NONE"
                 (false, isSome
                    (C.parseFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 999999999999"))
      val () = checkBool "parseFen halfmove 2147483648 -> NONE"
                 (false, isSome
                    (C.parseFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 2147483648 1"))
      val () = checkString "parseFen in-range counters round-trip"
                 ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 100 2147483647",
                  C.toFen (fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 100 2147483647"))

      val () = section "perft: start position (reference vectors)"
      val () = checkInt "perft(1) = 20" (20, C.perft C.startPos 1)
      val () = checkInt "perft(2) = 400" (400, C.perft C.startPos 2)
      val () = checkInt "perft(3) = 8902" (8902, C.perft C.startPos 3)
      val () = checkInt "perft(4) = 197281" (197281, C.perft C.startPos 4)

      val () = section "perft: Kiwipete (reference vectors)"
      val kiwi = fen kiwiFen
      val () = checkInt "kiwi perft(1) = 48" (48, C.perft kiwi 1)
      val () = checkInt "kiwi perft(2) = 2039" (2039, C.perft kiwi 2)
      val () = checkInt "kiwi perft(3) = 97862" (97862, C.perft kiwi 3)

      val () = section "move counts"
      val () = checkInt "legalMoves start = 20" (20, List.length (C.legalMoves C.startPos))
      val afterE4 = C.makeMove C.startPos (valOf (C.moveFromUci "e2e4"))
      val () = checkInt "legalMoves after 1.e4 = 20" (20, List.length (C.legalMoves afterE4))

      val () = section "makeMove / FEN effect"
      val () = checkString "1.e4 sets ep square"
                 ("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1",
                  C.toFen afterE4)

      val () = section "en passant"
      val epPos = fen "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3"
      val () = checkBool "ep capture generated (e5d6)"
                 (true, hasUci (C.legalMoves epPos) "e5d6")
      val epAfter = C.makeMove epPos (valOf (C.parseMove epPos "e5d6"))
      val () = checkBool "ep: capturing pawn now on d6"
                 (true, C.pieceAt epAfter (C.square 3 5) = SOME (C.White, C.Pawn))
      val () = checkBool "ep: captured pawn removed from d5"
                 (true, C.pieceAt epAfter (C.square 3 4) = NONE)

      val () = section "promotion"
      val promoPos = fen "8/P7/8/8/8/8/8/k6K w - - 0 1"
      val fromA7 = List.filter (fn m => #from m = C.square 0 6) (C.legalMoves promoPos)
      val () = checkInt "four promotions from a7" (4, List.length fromA7)
      val () = checkBool "queen promotion uci a7a8q"
                 (true, hasUci fromA7 "a7a8q")
      val () = checkBool "knight promotion uci a7a8n"
                 (true, hasUci fromA7 "a7a8n")
      val promoAfter = C.makeMove promoPos (valOf (C.parseMove promoPos "a7a8q"))
      val () = checkBool "promoted to queen on a8"
                 (true, C.pieceAt promoAfter (C.square 0 7) = SOME (C.White, C.Queen))

      val () = section "castling"
      val castPos = fen "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1"
      val () = checkBool "kingside castle generated (e1g1)"
                 (true, hasUci (C.legalMoves castPos) "e1g1")
      val () = checkBool "queenside castle generated (e1c1)"
                 (true, hasUci (C.legalMoves castPos) "e1c1")
      val castled = C.makeMove castPos (valOf (C.parseMove castPos "e1g1"))
      val () = checkBool "O-O: king on g1"
                 (true, C.pieceAt castled (C.square 6 0) = SOME (C.White, C.King))
      val () = checkBool "O-O: rook on f1"
                 (true, C.pieceAt castled (C.square 5 0) = SOME (C.White, C.Rook))

      val () = section "check / checkmate / stalemate"
      val () = checkBool "start not in check" (false, C.inCheck C.startPos)
      val fm = fen foolsMate
      val () = checkBool "fool's mate: in check" (true, C.inCheck fm)
      val () = checkBool "fool's mate: checkmate" (true, C.isCheckmate fm)
      val () = checkBool "fool's mate: not stalemate" (false, C.isStalemate fm)
      val () = checkInt "fool's mate: no legal moves" (0, List.length (C.legalMoves fm))
      val sm = fen stalemate
      val () = checkBool "stalemate: stalemate" (true, C.isStalemate sm)
      val () = checkBool "stalemate: not checkmate" (false, C.isCheckmate sm)
      val () = checkBool "stalemate: not in check" (false, C.inCheck sm)

      val () = section "pins (full legality)"
      val pinPos = fen "4r3/8/8/8/8/8/4B3/4K3 w - - 0 1"
      val () = checkBool "pinned bishop has no legal move"
                 (false, List.exists (fn m => #from m = C.square 4 1) (C.legalMoves pinPos))

      val () = section "UCI (de)serialization"
      val () = checkString "moveToUci e2e4"
                 ("e2e4", C.moveToUci { from = C.square 4 1, to = C.square 4 3, promo = NONE })
      val () = checkString "moveToUci promo e7e8q"
                 ("e7e8q",
                  C.moveToUci { from = C.square 4 6, to = C.square 4 7, promo = SOME C.Queen })
      val () = checkBool "moveFromUci e2e4"
                 (true, C.moveFromUci "e2e4"
                        = SOME { from = C.square 4 1, to = C.square 4 3, promo = NONE })
      val () = checkBool "moveFromUci e7e8q"
                 (true, C.moveFromUci "e7e8q"
                        = SOME { from = C.square 4 6, to = C.square 4 7, promo = SOME C.Queen })
      val () = checkBool "moveFromUci bad -> NONE" (true, C.moveFromUci "xx" = NONE)
      val () = checkBool "parseMove legal e2e4"
                 (true, isSome (C.parseMove C.startPos "e2e4"))
      val () = checkBool "parseMove illegal e2e5 -> NONE"
                 (true, C.parseMove C.startPos "e2e5" = NONE)

      val () = section "search (negamax + material eval)"
      val () = checkInt "start material is balanced" (0, C.evaluate C.startPos)
      val () = checkBool "bestMove start is some move"
                 (true, isSome (C.bestMove C.startPos 1))
      val matePos = fen "6k1/5ppp/8/8/8/8/8/R6K w - - 0 1"
      val () = checkString "finds mate in 1 (Ra8#)"
                 ("a1a8", C.moveToUci (valOf (C.bestMove matePos 2)))
    in
      Harness.run ()
    end

  val run = runAll
end
