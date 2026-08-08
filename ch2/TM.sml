structure TM = 
struct
datatype D = R | L
datatype S = B | I | O
datatype Q = M | H | MR | ST
type delta = ((Q * S) * (Q * S * D)) list
type program = Q * delta
type tape = S list * S * S list
val P = (M, [((M, I), (M, O , L)),
             ((M, O), (H, I , L)),
             ((M, B), (H, I , L))])

(* Exercise 2.2 *)
val P2 = (ST, [((ST, B), (MR, B, R)),
               ((MR, I), (MR, I, R)),
               ((MR, O), (MR, O, R)),
               ((MR, B), (M, B, L)),
               ((M, I), (M, O , L)),
               ((M, O), (H, I , L)),
               ((M, B), (H, I , L)),
               ((H, I), (H, I , L)),
               ((H, O), (H, O , L))])
end

