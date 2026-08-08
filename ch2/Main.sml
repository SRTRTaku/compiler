open TM
val T = ([I, O, I], I, nil);
val r = Eval.eval P T
val _ = Dynamic.pp {T = T, r = r};

(* practice 2.2 *)
val T2 = (nil, B, [I, O, I, I]);
val r2 = Eval.eval P2 T2
val _ = Dynamic.pp {T2 = T2, r2 = r2};

