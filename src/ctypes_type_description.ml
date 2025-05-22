open Ctypes
open Ctypes_zarith
open Libpoly_structs

module DyadicRational = struct
  type s = lp_dyadic_rational_struct_0
  type t = s structure

  let t : t typ = lift_typ lp_dyadic_rational_struct_0

  let s =
    let field_0 =
      field (lift_typ lp_dyadic_rational_struct_0) "a" (lift_typ MPZ.t)
    in
    let field_1 = field (lift_typ lp_dyadic_rational_struct_0) "n" ulong in
    let () = seal (lift_typ lp_dyadic_rational_struct_0) in
    object
      method ctype = lp_dyadic_rational_struct_0

      method members =
        object
          method a = field_0
          method n = field_1
        end
    end
end

module Ring = struct
  type t = [ `lp_int_ring_t ] structure

  let s : t typ = structure ""

  let lp_int_ring_struct =
    let field_0 = field s "ref_count" size_t in
    let field_1 = field s "is_prime" int in
    let field_2 = field s "M" (lift_typ MPZ.t) in
    let field_3 = field s "lb" (lift_typ MPZ.t) in
    let field_4 = field s "ub" (lift_typ MPZ.t) in
    let () = seal s in
    object
      method ctype = s

      method members =
        object
          method ref_count = field_0
          method is_prime = field_1
          method _M = field_2
          method lb = field_3
          method ub = field_4
        end
    end

  let t = typedef s "lp_int_ring_t"
end

module Types (F : TYPE) = struct
  open F

  type lp_integer_t = MPZ.t abstract

  let lp_integer_t = lift_typ MPZ.t

  type lp_rational_t = MPQ.t abstract

  let lp_rational_t = lift_typ MPQ.t

  type lp_dyadic_rational_t = DyadicRational.t

  let lp_dyadic_rational_t = lift_typ DyadicRational.s#ctype

  module DyadicInterval = struct
    include DyadicInterval

    type s = t
    type t = s abstract
  end

  module UPolynomial = struct
    (* Anonymous lp_upolynomial_t *)

    type t = [ `lp_upolynomial_struct ] structure

    let s : t typ = structure ""
    let t = typedef s "lp_upolynomial_t"
  end

  type lp_upolynomial_t = UPolynomial.t

  let lp_upolynomial_t = UPolynomial.t

  module AlgebraicNumber = struct
    type t = [ `lp_algebraic_number_struct ] structure

    let s : t typ = structure "lp_algebraic_number_struct"

    let lp_algebraic_number_struct =
      let field_0 = field s "f" (ptr UPolynomial.t) in
      let field_1 = field s "I" (lift_typ DyadicInterval.t) in
      let field_2 = field s "sgn_at_a" int in
      let field_3 = field s "sgn_at_b" int in
      let () = seal s in
      object
        method ctype = s

        method members =
          object
            method f = field_0
            method _I = field_1
            method sgn_at_a = field_2
            method sgn_at_b = field_3
          end
      end

    let t = typedef s "lp_algebraic_number_t"
  end

  type lp_algebraic_number_t = AlgebraicNumber.t

  let lp_algebraic_number_t = AlgebraicNumber.s

  type lp_value_t = [ `lp_value_struct ] structure

  let lp_value_t : lp_value_t typ =
    typedef (structure "lp_value_struct") "lp_value_t"

  type lp_value_type_t =
    | LP_VALUE_NONE
    | LP_VALUE_INTEGER
    | LP_VALUE_DYADIC_RATIONAL
    | LP_VALUE_RATIONAL
    | LP_VALUE_ALGEBRAIC
    | LP_VALUE_PLUS_INFINITY
    | LP_VALUE_MINUS_INFINITY

  let lp_value_type_t =
    let lp_value_none = constant "LP_VALUE_NONE" int64_t in
    let lp_value_integer = constant "LP_VALUE_INTEGER" int64_t in
    let lp_value_dyadic_rational =
      constant "LP_VALUE_DYADIC_RATIONAL" int64_t
    in
    let lp_value_rational = constant "LP_VALUE_RATIONAL" int64_t in
    let lp_value_algebraic = constant "LP_VALUE_ALGEBRAIC" int64_t in
    let lp_value_plus_infinity = constant "LP_VALUE_PLUS_INFINITY" int64_t in
    let lp_value_minus_infinity = constant "LP_VALUE_MINUS_INFINITY" int64_t in
    enum ~typedef:true "lp_value_type_t"
      [
        (LP_VALUE_NONE, lp_value_none);
        (LP_VALUE_INTEGER, lp_value_integer);
        (LP_VALUE_DYADIC_RATIONAL, lp_value_dyadic_rational);
        (LP_VALUE_RATIONAL, lp_value_rational);
        (LP_VALUE_ALGEBRAIC, lp_value_algebraic);
        (LP_VALUE_PLUS_INFINITY, lp_value_plus_infinity);
        (LP_VALUE_MINUS_INFINITY, lp_value_minus_infinity);
      ]

  module Value = struct
    type union_t = [ `lp_value_union_t ] union

    let union_t : union_t typ = typedef (union "") "lp_value_union_t"

    module Union = struct
      let z = field union_t "z" lp_integer_t
      let q = field union_t "q" lp_rational_t
      let dy_q = field union_t "dy_q" lp_dyadic_rational_t
      let a = field union_t "a" lp_algebraic_number_t
      let () = seal union_t
    end

    let type_ = field lp_value_t "type" lp_value_type_t
    let union = field lp_value_t "value" union_t
    let () = seal lp_value_t
  end

  type lp_int_ring_struct = [ `lp_int_ring_struct ] structure

  let lp_int_ring_struct = lift_typ Ring.s
  let lp_int_ring_t = typedef lp_int_ring_struct "lp_int_ring_t"

  type lp_variable_t = Unsigned.size_t

  let lp_variable_t : lp_variable_t typ = size_t

  type lp_variable_db_struct = [ `lp_variable_db_struct ] structure

  let lp_variable_db_struct : lp_variable_db_struct typ =
    structure "lp_variable_db_struct"

  let lp_variable_db_t = typedef lp_variable_db_struct "lp_variable_db_t"

  type lp_variable_list_struct = [ `lp_variable_list_struct ] structure

  let lp_variable_list_struct : lp_variable_list_struct typ =
    structure "lp_variable_list_struct"

  let lp_variable_list_t = typedef lp_variable_list_struct "lp_variable_list_t"

  type lp_variable_order_struct = [ `lp_variable_order_struct ] structure

  let lp_variable_order_struct : lp_variable_order_struct typ =
    structure "lp_variable_order_struct"

  let lp_variable_order_t =
    typedef lp_variable_order_struct "lp_variable_order_t"

  type lp_polynomial_context_struct =
    [ `lp_polynomial_context_struct ] structure

  let lp_polynomial_context_struct : lp_polynomial_context_struct typ =
    structure "lp_polynomial_context_struct"

  let lp_polynomial_context_t =
    typedef lp_polynomial_context_struct "lp_polynomial_context_t"

  (* monomial.h *)

  type power_t = [ `power_struct ] structure

  let power_t : power_t typ = typedef (structure "power_struct") "power_t"

  module Power = struct
    let s = power_t
    let x = field s "x" lp_variable_t
    let d = field s "d" size_t
    let () = seal s
  end

  type lp_monomial_t = [ `lp_monomial_struct ] structure

  let lp_monomial_t : lp_monomial_t typ =
    typedef (structure "lp_monomial_struct") "lp_monomial_t"

  module Monomial = struct
    let s = lp_monomial_t
    let a = field s "a" lp_integer_t
    let n = field s "n" size_t
    let capacity = field s "capacity" size_t
    let p = field s "p" (ptr power_t)
    let () = seal s
  end

  (* interval.h *)

  type lp_interval_t = [ `lp_interval_struct ] structure

  let lp_interval_struct : lp_interval_t typ = structure "lp_interval_struct"
  let lp_interval_t = typedef lp_interval_struct "lp_interval_t"

  (* rational_interval.h *)

  type lp_rational_interval_t = [ `lp_rational_interval_struct ] structure

  let lp_rational_interval_struct : lp_rational_interval_t typ =
    structure "lp_rational_interval_struct"

  module Rational_interval = struct
    let s = lp_rational_interval_struct
    let a = field s "a" lp_rational_t
    let b = field s "b" lp_rational_t
    let () = seal s
  end

  let lp_rational_interval_t =
    typedef lp_rational_interval_struct "lp_rational_interval_t"

  (* assignment.h *)

  type lp_assignment_t = [ `lp_assignment_struct ] structure

  let lp_assignment_struct : lp_assignment_t typ =
    structure "lp_assignment_struct"

  let lp_assignment_t = typedef lp_assignment_struct "lp_polynomial_t"

  (* Interval assignment *)

  type lp_interval_assignment_t = [ `lp_interval_assignment_struct ] structure

  let lp_interval_assignment_struct : lp_interval_assignment_t typ =
    structure "lp_interval_assignment_struct"

  let lp_interval_assignment_t =
    typedef lp_interval_assignment_struct "lp_interval_assignment_t"

  (* polynomial.h *)

  type lp_polynomial_t = [ `lp_polynomial_struct ] structure

  let lp_polynomial_struct : lp_polynomial_t typ =
    structure "lp_polynomial_struct"

  let lp_polynomial_t = typedef lp_polynomial_struct "lp_polynomial_t"

  let lp_polynomial_traverse_f =
    ptr (const lp_polynomial_context_t)
    @-> ptr lp_monomial_t @-> ptr void @-> returning void

  (* polynomial_vector.h *)

  type lp_polynomial_vector_t = [ `lp_polynomial_vector_struct ] structure

  let lp_polynomial_vector_struct : lp_polynomial_vector_t typ =
    structure "lp_polynomial_vector_struct"

  let lp_polynomial_vector_t =
    typedef lp_polynomial_vector_struct "lp_polynomial_vector_t"

  (* feasibility_set.h *)

  type lp_feasibility_set_t = [ `lp_feasibility_set_struct ] structure

  let lp_feasibility_set_struct : lp_feasibility_set_t typ =
    structure "lp_feasibility_set_struct"

  let lp_feasibility_set_t =
    typedef lp_feasibility_set_struct "lp_feasibility_set_t"

  (* sign_condition.h *)

  type lp_sign_condition_t = Lt0 | Le0 | Eq0 | Ne0 | Gt0 | Ge0

  let lp_sign_condition_enum =
    let lp_sgn_lt_0 = constant "LP_SGN_LT_0" int64_t in
    let lp_sgn_le_0 = constant "LP_SGN_LE_0" int64_t in
    let lp_sgn_eq_0 = constant "LP_SGN_EQ_0" int64_t in
    let lp_sgn_ne_0 = constant "LP_SGN_NE_0" int64_t in
    let lp_sgn_gt_0 = constant "LP_SGN_GT_0" int64_t in
    let lp_sgn_ge_0 = constant "LP_SGN_GE_0" int64_t in
    enum "lp_sign_condition_enum"
      [
        (Lt0, lp_sgn_lt_0);
        (Le0, lp_sgn_le_0);
        (Eq0, lp_sgn_eq_0);
        (Ne0, lp_sgn_ne_0);
        (Gt0, lp_sgn_gt_0);
        (Ge0, lp_sgn_ge_0);
      ]

  let lp_sign_condition_t = typedef lp_sign_condition_enum "lp_sign_condition_t"
end
