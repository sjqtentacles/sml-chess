(* demo.sml - print the standard start position, a perft table, and a couple of
   legal-move queries. Deterministic: identical output on every run and under
   both compilers. *)

structure C = Chess

fun line s = print (s ^ "\n")
fun pad w s =
  if String.size s >= w then s
  else s ^ String.implode (List.tabulate (w - String.size s, fn _ => #" "))

val () = line "Start position:"
val () = line ("  FEN: " ^ C.toFen C.startPos)
val () = line ("  side to move: "
               ^ (case C.sideToMove C.startPos of C.White => "white" | C.Black => "black"))
val () = line ("  legal moves: " ^ Int.toString (List.length (C.legalMoves C.startPos)))

val () = line ""
val () = line "perft from the start position:"
val () = List.app
  (fn d => line ("  perft(" ^ Int.toString d ^ ") = " ^ Int.toString (C.perft C.startPos d)))
  [1, 2, 3, 4]

val () = line ""
val () = line "perft from Kiwipete:"
val kiwi = valOf (C.parseFen
  "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
val () = List.app
  (fn d => line ("  perft(" ^ Int.toString d ^ ") = " ^ Int.toString (C.perft kiwi d)))
  [1, 2, 3]

val () = line ""
val () = line "After 1. e4 e5 2. Nf3:"
fun play pos uci = C.makeMove pos (valOf (C.parseMove pos uci))
val p = play (play (play C.startPos "e2e4") "e7e5") "g1f3"
val () = line ("  FEN: " ^ C.toFen p)
val () = line ("  best reply (depth 2): "
               ^ (case C.bestMove p 2 of SOME m => C.moveToUci m | NONE => "(none)"))
