open Ctypes

module Dyadic_rational : sig
  type t
end

module Ring : sig
  type t = [ `lp_int_ring_t ] Ctypes.structure

  val t : t typ
  val lp_Z : t ptr
end

module UPolynomial : sig
  type t = [ `lp_upolynomial_struct ] Ctypes.structure

  val t : t typ
  val degree : t ptr -> int
  val construct_from_int : Ring.t ptr -> int -> int ptr -> t ptr
  val construct_power : Ring.t ptr -> int -> Signed.Long.t -> t ptr
  val delete : t ptr -> unit
end

module Integer : sig
  type t

  val of_z : Z.t -> t
  val to_z : t -> Z.t
end

module Rational : sig
  type t

  val of_q : Q.t -> t
  val to_q : t -> Q.t
end

module AlgebraicNumber : sig
  type t = [ `lp_algebraic_number_struct ] structure

  val t : t typ
  val construct_zero : t ptr -> unit
end

module Value : sig
  type t

  type view =
    | Integer of Integer.t
    | Dyadic_rational of Dyadic_rational.t
    | Rational of Rational.t
    | Algebraic of AlgebraicNumber.t
    | Plus_infinity
    | Minus_infinity

  val view : t -> view
  val to_string : t -> string
  val compare : t -> t -> int
  val of_int : int -> t
  val of_z : Z.t -> t
  val of_q : Q.t -> t
  val of_rational : Rational.t -> t
  val sgn : t -> int
  val add : t -> t -> t
  val sub : t -> t -> t
  val neg : t -> t
  val mul : t -> t -> t
  val inv : t -> t
  val div : t -> t -> t
  val is_rational : t -> bool
  val to_rational_opt : t -> Rational.t option
  val to_q_opt : t -> Q.t option
  val is_integer : t -> bool

  (* exponent must be positive *)
  val pow : t -> int -> t
end

module Variable : sig
  type t
  type variable = t

  module Db : sig
    type t

    val create : unit -> t
    val new_variable : t -> string -> variable
    val add_variable : t -> variable -> string -> unit
    val get_name : t -> variable -> string
  end

  module Order : sig
    type t

    val to_string : db:Db.t -> t -> string
    val create : unit -> t
    val compare_variables : t -> variable -> variable -> int
    val size : t -> int
    val contains : t -> variable -> bool
    val push : t -> variable -> unit
    val pop : t -> unit
    val reverse : t -> unit
    val top : t -> variable
  end
end

module Assignment : sig
  type t

  val create : Variable.Db.t -> t
  val to_string : t -> string
  val add : t -> Variable.t -> Value.t -> unit
  val remove : t -> Variable.t -> unit

  val find : t -> Variable.t -> Value.t
  (** [find m x] returns the value of [x] in [m].

      @raise Not_found if [x] is not in [m]. *)

  val find_opt : t -> Variable.t -> Value.t option
  (** [find_opt m x] is [Some v] if [x] is assigned to [v] in [m], and [None] if
      [x] is not assigned in [m]. *)
end

module Polynomial : sig
  type t

  module Context : sig
    type t

    val create :
      ?ring:Ring.t Ctypes.ptr -> Variable.Db.t -> Variable.Order.t -> t

    val equal : t -> t -> bool
  end

  module Monomial : sig
    type t

    val create : ctx:Context.t -> Integer.t -> (Variable.t * int) list -> t
    (** {b Warning}: The provided variables must be pairwise distinct. *)

    val coefficient : t -> Integer.t
    val length : t -> int
    val fold_variables : (Variable.t -> int -> 'a -> 'a) -> t -> 'a -> 'a
  end

  val to_string : t -> string

  val create : ctx:Context.t -> t
  (** Create a zero polynomial. *)

  val create_simple : ctx:Context.t -> Integer.t -> Variable.t -> int -> t
  val resultant : ctx:Context.t -> t -> t -> t
  val gcd : ctx:Context.t -> t -> t -> t
  val evaluate : t -> Assignment.t -> Value.t
  val sgn : t -> Assignment.t -> int
  val fold : (Context.t -> Monomial.t -> 'a -> 'a) -> t -> 'a -> 'a
  val of_list : ctx:Context.t -> Monomial.t list -> t
  val add : ctx:Context.t -> t -> t -> t
  val sub : ctx:Context.t -> t -> t -> t
  val neg : ctx:Context.t -> t -> t
  val mul : ctx:Context.t -> t -> t -> t
  val div : ctx:Context.t -> t -> t -> t
  val roots_isolate : t -> Assignment.t -> Value.t array
  val factor_square_free : t -> (t * int) array
  val reductum : ctx:Context.t -> t -> t
  val eq : t -> t -> int 
  val top_variable : t -> Variable.t 
  val degree : t -> int 
  val get_coefficient : ctx:Context.t -> t -> int -> t 
  val compare : t -> t -> int 
end
