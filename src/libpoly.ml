open Ctypes
module CI = Cstubs_internals

(* open Ctypes_zarith *)
open Unsigned

module Ring = struct
  include Ctypes_type_description.Ring
  include Ctypes_bindings.Function.Ring

  let lp_Z = !@lp_Z
end

module UPolynomial = struct
  include Types_generated.UPolynomial
  include Ctypes_bindings.Function.UPolynomial

  let degree p = degree p |> Unsigned.Size_t.to_int

  let construct_from_int ring degree coeffs =
    construct_from_int ring (Size_t.of_int degree) coeffs

  let construct_power ring degree coeff =
    construct_power ring (Size_t.of_int degree) coeff
end

module AlgebraicNumber = struct
  include Types_generated.AlgebraicNumber
  include Ctypes_bindings.Function.AlgebraicNumber
end

module Integer = struct
  open Ctypes_bindings

  type t = Type.lp_integer_t ptr

  external construct_from_z : _ CI.fatptr -> Z.t -> unit
    = "ml_lp_integer_construct_from_z"

  external get_z : _ CI.fatptr -> Z.t = "ml_lp_integer_get_z"

  let of_z z =
    let p =
      allocate_n ~finalise:Function.Integer.destruct ~count:1 Type.lp_integer_t
    in
    let (CI.CPointer pv) = p in
    construct_from_z pv z;
    p

  let to_z (CI.CPointer p) = get_z p
end

module Rational = struct
  open Ctypes_bindings

  type t = Type.lp_rational_t ptr

  external construct_from_div_z : _ CI.fatptr -> Z.t -> Z.t -> unit
    = "ml_lp_rational_construct_from_div_z"

  external get_num_z : _ CI.fatptr -> Z.t = "ml_lp_rational_get_num_z"
  external get_den_z : _ CI.fatptr -> Z.t = "ml_lp_rational_get_den_z"

  let of_q q =
    let p =
      allocate_n ~finalise:Function.Rational.destruct ~count:1
        Type.lp_rational_t
    in
    let (CI.CPointer pv) = p in
    construct_from_div_z pv (Q.num q) (Q.den q);
    p

  let to_q (CI.CPointer pv) = Q.make (get_num_z pv) (get_den_z pv)
end

module Dyadic_rational = struct
  open Ctypes_bindings

  type t = Type.lp_dyadic_rational_t ptr
end

module Value = struct
  open Ctypes_bindings

  type t = Type.lp_value_t ptr

  type view =
    | Integer of Integer.t
    | Dyadic_rational of Dyadic_rational.t
    | Rational of Rational.t
    | Algebraic of AlgebraicNumber.t
    | Plus_infinity
    | Minus_infinity

  let view v =
    match !@(v |-> Type.Value.type_) with
    | LP_VALUE_NONE ->
        (* broken invariant: we should not expose functions that return LP_VALUE_NONE *)
        assert false
    | LP_VALUE_INTEGER -> Integer (v |-> Type.Value.union |-> Type.Value.Union.z)
    | LP_VALUE_DYADIC_RATIONAL ->
        Dyadic_rational (v |-> Type.Value.union |-> Type.Value.Union.dy_q)
    | LP_VALUE_RATIONAL ->
        Rational (v |-> Type.Value.union |-> Type.Value.Union.q)
    | LP_VALUE_ALGEBRAIC ->
        Algebraic !@(v |-> Type.Value.union |-> Type.Value.Union.a)
    | LP_VALUE_PLUS_INFINITY -> Plus_infinity
    | LP_VALUE_MINUS_INFINITY -> Minus_infinity

  let is_integer = Function.Value.is_integer
  let is_rational = Function.Value.is_rational

  let get_rational v =
    let q =
      allocate_n ~finalise:Function.Rational.destruct ~count:1
        Type.lp_rational_t
    in
    Function.Rational.construct q;
    Function.Value.get_rational v q;
    q

  let to_rational_opt v = if is_rational v then Some (get_rational v) else None
  let to_q_opt v = Option.map Rational.to_q (to_rational_opt v)

  open Ctypes_bindings

  let[@inline] alloc () : t =
    allocate_n ~finalise:Function.Value.destruct ~count:1 Type.lp_value_t

  let[@inline] create () : t =
    let v = alloc () in
    Function.Value.construct_none v;
    v

  let[@inline] unop op v1 =
    let v = create () in
    op v v1;
    v

  let[@inline] binop op v1 v2 =
    let v = create () in
    op v v1 v2;
    v

  let of_int n =
    let v = alloc () in
    Function.Value.construct_int v (Signed.Long.of_int n);
    v

  let of_z z =
    let v = alloc () in
    Function.Value.construct v LP_VALUE_INTEGER (to_voidp (Integer.of_z z));
    v

  let of_rational q =
    let v = alloc () in
    Function.Value.construct v LP_VALUE_RATIONAL (to_voidp q);
    v

  let of_q q = of_rational (Rational.of_q q)
  let sgn = Function.Value.sgn
  let add v1 v2 = binop Function.Value.add v1 v2
  let sub v1 v2 = binop Function.Value.sub v1 v2
  let neg v = unop Function.Value.neg v
  let mul v1 v2 = binop Function.Value.mul v1 v2
  let inv v = unop Function.Value.inv v
  let div v1 v2 = binop Function.Value.div v1 v2
  let pow v n = binop Function.Value.pow v (Unsigned.UInt.of_int n)
  let to_string = Function.Value.to_string
  let compare v1 v2 = Function.Value.cmp v1 v2
end

module Variable = struct
  open Types_generated
  open Ctypes_bindings.Function

  type t = lp_variable_t
  type variable = t

  module Db = struct
    type t = lp_variable_db_struct ptr

    let create () =
      let db = lp_variable_db_new () in
      Gc.finalise lp_variable_db_detach db;
      db

    let new_variable = lp_variable_db_new_variable
    let add_variable = lp_variable_db_add_variable
    let get_name = lp_variable_db_get_name
  end

  module Order = struct
    type t = lp_variable_order_struct ptr

    let[@inline] to_string ~db order = lp_variable_order_to_string order db

    let create () =
      let order = lp_variable_order_new () in
      Gc.finalise lp_variable_order_detach order;
      order

    let compare_variables = lp_variable_order_cmp

    let[@inline] size order =
      Unsigned.Size_t.to_int (lp_variable_order_size order)

    let contains = lp_variable_order_contains
    let push = lp_variable_order_push
    let pop = lp_variable_order_pop
    let reverse = lp_variable_order_reverse
    let top = lp_variable_order_top
  end
end

module Assignment = struct
  open Ctypes_bindings
  open Ctypes_bindings.Type
  open Ctypes_bindings.Function

  type t = lp_assignment_t ptr

  let create db =
    let m = Assignment.new_ db in
    Gc.finalise Assignment.delete m;
    m

  let to_string = Assignment.to_string
  let add m x v = Assignment.set_value m x v
  let remove m x = Assignment.set_value m x (from_voidp (const lp_value_t) null)

  let[@inline] find m x =
    let v = Assignment.get_value m x in
    match !@(v |-> Type.Value.type_) with
    | LP_VALUE_NONE -> raise Not_found
    | _ -> v

  let[@inline] find_opt m x =
    let v = Assignment.get_value m x in
    match !@(v |-> Type.Value.type_) with LP_VALUE_NONE -> None | _ -> Some v
end

module Polynomial = struct
  open Ctypes_bindings

  type t = Type.lp_polynomial_t ptr

  module Context = struct
    type t = Type.lp_polynomial_context_struct ptr

    let create ?(ring = Ring.lp_Z) db order =
      let ctx = Function.lp_polynomial_context_new ring db order in
      Gc.finalise Function.lp_polynomial_context_detach ctx;
      ctx

    let equal = Function.lp_polynomial_context_equal
  end

  module Monomial = struct
    type t = Type.lp_monomial_t ptr

    let create ~ctx c xs =
      let m = addr (make Type.lp_monomial_t) in
      Function.Monomial.construct ctx m;
      Gc.finalise Function.Monomial.destruct m;
      Function.Monomial.set_coefficient ctx m c;
      List.iter
        (fun (x, d) -> Function.Monomial.push m x (Unsigned.Size_t.of_int d))
        xs;
      m

    let coefficient (m : t) = m |-> Type.Monomial.a
    let length m = Unsigned.Size_t.to_int !@(m |-> Type.Monomial.n)

    let fold_variables f m acc =
      let powers = m |-> Type.Monomial.p in
      let acc = ref acc in
      for i = 0 to length m - 1 do
        let power = !@(powers +@ i) in
        acc :=
          f
            !@(power |-> Type.Power.x)
            (Unsigned.Size_t.to_int !@(power |-> Type.Power.d))
            !acc
      done;
      !acc
  end

  open Ctypes_bindings.Type
  open Ctypes_bindings.Function
  open Ctypes_bindings.Function.Polynomial

  let create_simple ~ctx c x n =
    let p = alloc () in
    let _n = Unsigned.UInt.of_int n in
    construct_simple p ctx c x (Unsigned.UInt.of_int n);
    Gc.finalise delete p;
    set_external p;
    p

  let create ~ctx =
    let p = new_ ctx in
    Gc.finalise delete p;
    set_external p;
    p

  let to_string = Function.Polynomial.to_string
  let degree p = Unsigned.Size_t.to_int (degree p)

  let[@inline] unop op ~ctx v1 =
    let p = create ~ctx in
    op p v1;
    p

  let[@inline] binop op ~ctx v1 v2 =
    let p = create ~ctx in
    op p v1 v2;
    p

  let of_list ~ctx ms =
    let p = create ~ctx in
    List.iter (fun m -> Function.Polynomial.add_monomial p m) ms;
    ensure_order p;
    set_external p;
    p
  let compare = Function.Polynomial.cmp
  let add ~ctx p1 p2 = binop add ~ctx p1 p2
  let sub ~ctx p1 p2 = binop sub ~ctx p1 p2
  let neg ~ctx p = unop neg ~ctx p
  let mul ~ctx p1 p2 = binop mul ~ctx p1 p2
  let div ~ctx p1 p2 = binop div ~ctx p1 p2
  let resultant ~ctx p1 p2 = binop resultant ~ctx p1 p2
  let gcd ~ctx p1 p2 = binop gcd ~ctx p1 p2
  let sgn = sgn
  let evaluate = evaluate
  let reductum ~ctx p = unop reductum ~ctx p
  let derivative ~ctx p = unop derivative ~ctx p 
  let eq = eq 
  let top_variable = top_variable 
  let degree = degree 

  let get_coefficient ~ctx p k =   
    let q = create ~ctx in
    let i = Unsigned.Size_t.of_int k in
    get_coefficient q p i;
    q

  let fold f p acc =
    let acc = ref acc in
    traverse p (fun ctx mon _ -> acc := f ctx mon !acc) null;
    !acc

  let roots_isolate p m =
    let d = degree p in
    let tmp_roots = allocate_n lp_value_t ~count:d in
    let tmp_roots_size = allocate size_t Unsigned.Size_t.zero in
    roots_isolate p m tmp_roots tmp_roots_size;
    let tmp_roots_size = Unsigned.Size_t.to_int !@tmp_roots_size in
    let roots =
      Array.init tmp_roots_size (fun i ->
          let root = Value.new_ LP_VALUE_NONE null in
          Gc.finalise Value.destruct root;
          Value.swap root (tmp_roots +@ i);
          root)
    in
    for i = 0 to tmp_roots_size - 1 do
      Value.destruct (tmp_roots +@ i)
    done;
    roots

  external libpoly_utils_free : unit ptr -> unit
    = "libpoly_utils_free"

  let factor_square_free p =
    let tmp_factors = allocate (ptr (ptr (lp_polynomial_t))) (from_voidp (ptr (lp_polynomial_t)) null) in
    let tmp_multiplicities = allocate (ptr size_t) (from_voidp size_t null) in
    let tmp_size = allocate size_t Unsigned.Size_t.zero in
    factor_square_free p tmp_factors tmp_multiplicities tmp_size;
    let tmp_size = Unsigned.Size_t.to_int !@tmp_size in
    let r =
      Array.init tmp_size (fun i ->
        let p = !@(!@tmp_factors +@ i) in
        let m = Unsigned.Size_t.to_int !@(!@tmp_multiplicities +@ i) in
        (p, m))
    in
    libpoly_utils_free (to_voidp (!@tmp_factors));
    libpoly_utils_free (to_voidp (!@tmp_multiplicities));
    r
end
