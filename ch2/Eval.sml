structure Eval = 
struct

fun Hd nil = TM.B
  | Hd (h :: _) = h
fun Tl nil = nil
  | Tl (_ :: tl) = tl
fun Cons (TM.B, nil) = nil
  | Cons (h, t) = h::t

fun moveL (LList, h , RList)
    = (Tl LList, Hd LList, Cons (h, RList))
fun moveR (LList, h , RList)
    = (Cons(h, LList), Hd RList, Tl RList)

fun move TM.L tape = moveL tape
  | move TM.R tape = moveR tape

fun exec delta (q, tape as (LList, h, RList)) = 
    case List.find (fn (x,y) => x = (q, h)) delta of
        NONE => (LList, h , RList)
      | SOME (x, (q', s, d)) =>
            exec delta (q', move d (LList, s, RList))

fun eval (state, delta) tape = exec delta (state, tape)

end

