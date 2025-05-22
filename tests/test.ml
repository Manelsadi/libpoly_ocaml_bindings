open Libpoly

let db = Variable.Db.create ()
let order = Variable.Order.create ()
let x = Variable.Db.new_variable db "x"
let () = Variable.Order.push order x
let y = Variable.Db.new_variable db "y"
let () = Variable.Order.push order y
let ctx = Polynomial.Context.create db order
let p1 = Polynomial.create_simple ~ctx (Integer.of_z (Z.of_int 2)) x 1
let () = print_endline (Polynomial.to_string p1)
let p2 = Polynomial.create_simple ~ctx (Integer.of_z (Z.of_int 7)) y 4
let () = print_endline (Polynomial.to_string p2)
let p3 = Polynomial.add ~ctx p1 p2
let () = print_endline (Polynomial.to_string p3)
let p4 = Polynomial.mul ~ctx p3 p2
let () = print_endline (Polynomial.to_string p4)

let pconst n =
  Polynomial.of_list ~ctx
    [
      Polynomial.Monomial.create ~ctx (Integer.of_z n) [];
      Polynomial.Monomial.create ~ctx (Integer.of_z (Z.of_int 14)) [ (x, 3) ];
      Polynomial.Monomial.create ~ctx (Integer.of_z (Z.of_int 14)) [ (y, 2) ];
    ]

let p5 = pconst (Z.of_int 14)
let () = print_endline (Polynomial.to_string p5)
let p6 = Polynomial.resultant ~ctx p4 p5
let () = print_endline (Polynomial.to_string p6)

let m =
  let m = Assignment.create db in
  Assignment.add m y (Value.of_z (Z.of_int 19));
  m

let rts = Polynomial.roots_isolate p6 m

let () =
  Array.iter (fun v -> Format.eprintf "root: %s@." (Value.to_string v)) rts
