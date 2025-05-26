open Ctypes

module Functions (F : FOREIGN) = struct
  open F

  module Ring = struct
    open Ctypes_type_description.Ring

    let lp_Z = foreign_value "lp_Z" (ptr t)
  end

  module UPolynomial = struct
    open Types_generated.UPolynomial

    let degree = foreign "lp_upolynomial_degree" (ptr t @-> returning size_t)

    let construct_from_int =
      foreign "lp_upolynomial_construct_from_int"
        (ptr Ctypes_type_description.Ring.t
        @-> size_t @-> ptr int
        @-> returning (ptr t))

    let construct_power =
      foreign "lp_upolynomial_construct_power"
        (ptr Ctypes_type_description.Ring.t
        @-> size_t @-> long
        @-> returning (ptr t))

    let delete = foreign "lp_upolynomial_delete" (ptr t @-> returning void)

    let to_string0 =
      foreign "lp_upolynomial_to_string" (ptr t @-> returning (ptr char))
  end

  module Integer = struct
    open Types_generated

    let construct =
      foreign "lp_integer_construct" (ptr lp_integer_t @-> returning void)

    let construct_from_rational =
      foreign "lp_integer_construct_from_rational"
        (ptr (const lp_int_ring_t)
        @-> ptr lp_integer_t
        @-> ptr (const lp_rational_t)
        @-> returning void)

    let construct_from_int =
      foreign "lp_integer_construct_from_int"
        (ptr (const lp_int_ring_t)
        @-> ptr lp_integer_t @-> long @-> returning void)

    let construct_from_string =
      foreign "lp_integer_construct_from_string"
        (ptr (const lp_int_ring_t)
        @-> ptr lp_integer_t @-> string @-> int @-> returning void)

    let construct_copy =
      foreign "lp_integer_construct_copy"
        (ptr (const lp_int_ring_t)
        @-> ptr lp_integer_t
        @-> ptr (const lp_integer_t)
        @-> returning void)

    let assign =
      foreign "lp_integer_assign"
        (ptr (const lp_int_ring_t)
        @-> ptr lp_integer_t
        @-> ptr (const lp_integer_t)
        @-> returning void)

    let assign_int =
      foreign "lp_integer_assign_int"
        (ptr (const lp_int_ring_t)
        @-> ptr lp_integer_t @-> long @-> returning void)

    let destruct =
      foreign "lp_integer_destruct" (ptr lp_integer_t @-> returning void)

    let bits =
      foreign "lp_integer_bits" (ptr (const lp_integer_t) @-> returning size_t)

    let to_string =
      foreign "lp_integer_to_string"
        (ptr (const lp_integer_t) @-> returning string)

    let to_int =
      foreign "lp_integer_to_int" (ptr (const lp_integer_t) @-> returning long)

    let to_double =
      foreign "lp_integer_to_double"
        (ptr (const lp_integer_t) @-> returning double)

    let is_prime =
      foreign "lp_integer_is_prime" (ptr (const lp_integer_t) @-> returning int)

    let is_zero =
      foreign "lp_integer_is_zero"
        (ptr (const lp_int_ring_t)
        @-> ptr (const lp_integer_t)
        @-> returning int)

    let in_ring =
      foreign "lp_integer_in_ring"
        (ptr (const lp_int_ring_t)
        @-> ptr (const lp_integer_t)
        @-> returning int)

    let sgn =
      foreign "lp_integer_sgn"
        (ptr (const lp_int_ring_t)
        @-> ptr (const lp_integer_t)
        @-> returning int)

    let cmp =
      foreign "lp_integer_cmp"
        (ptr (const lp_int_ring_t)
        @-> ptr (const lp_integer_t)
        @-> ptr (const lp_integer_t)
        @-> returning int)

    let cmp_int =
      foreign "lp_integer_cmp_int"
        (ptr (const lp_int_ring_t)
        @-> ptr (const lp_integer_t)
        @-> long @-> returning int)

    let divides =
      foreign "lp_integer_divides"
        (ptr (const lp_int_ring_t)
        @-> ptr (const lp_integer_t)
        @-> ptr (const lp_integer_t)
        @-> returning int)

    let swap =
      foreign "lp_integer_swap"
        (ptr lp_integer_t @-> ptr lp_integer_t @-> returning void)

    let inc =
      foreign "lp_integer_inc"
        (ptr (const lp_int_ring_t) @-> ptr lp_integer_t @-> returning void)

    let dec =
      foreign "lp_integer_dec"
        (ptr (const lp_int_ring_t) @-> ptr lp_integer_t @-> returning void)

    let add =
      foreign "lp_integer_add"
        (ptr (const lp_int_ring_t)
        @-> ptr lp_integer_t
        @-> ptr (const lp_integer_t)
        @-> ptr (const lp_integer_t)
        @-> returning void)

    let sub =
      foreign "lp_integer_sub"
        (ptr (const lp_int_ring_t)
        @-> ptr lp_integer_t
        @-> ptr (const lp_integer_t)
        @-> ptr (const lp_integer_t)
        @-> returning void)

    let neg =
      foreign "lp_integer_neg"
        (ptr (const lp_int_ring_t)
        @-> ptr lp_integer_t
        @-> ptr (const lp_integer_t)
        @-> returning void)

    let abs =
      foreign "lp_integer_abs"
        (ptr (const lp_int_ring_t)
        @-> ptr lp_integer_t
        @-> ptr (const lp_integer_t)
        @-> returning void)

    let inv =
      foreign "lp_integer_inv"
        (ptr (const lp_int_ring_t)
        @-> ptr lp_integer_t
        @-> ptr (const lp_integer_t)
        @-> returning void)

    let mul =
      foreign "lp_integer_mul"
        (ptr (const lp_int_ring_t)
        @-> ptr lp_integer_t
        @-> ptr (const lp_integer_t)
        @-> ptr (const lp_integer_t)
        @-> returning void)

    let mul_int =
      foreign "lp_integer_mul_int"
        (ptr (const lp_int_ring_t)
        @-> ptr lp_integer_t
        @-> ptr (const lp_integer_t)
        @-> long @-> returning void)

    let mul_pow2 =
      foreign "lp_integer_mul_pow2"
        (ptr (const lp_int_ring_t)
        @-> ptr lp_integer_t
        @-> ptr (const lp_integer_t)
        @-> uint @-> returning void)

    let pow =
      foreign "lp_integer_pow"
        (ptr (const lp_int_ring_t)
        @-> ptr lp_integer_t
        @-> ptr (const lp_integer_t)
        @-> uint @-> returning void)

    let sqrt_Z =
      foreign "lp_integer_sqrt_Z"
        (ptr lp_integer_t @-> ptr (const lp_integer_t) @-> returning void)

    let add_mul =
      foreign "lp_integer_add_mul"
        (ptr (const lp_int_ring_t)
        @-> ptr lp_integer_t
        @-> ptr (const lp_integer_t)
        @-> ptr (const lp_integer_t)
        @-> returning void)

    let add_mul_int =
      foreign "lp_integer_add_mul_int"
        (ptr (const lp_int_ring_t)
        @-> ptr lp_integer_t
        @-> ptr (const lp_integer_t)
        @-> int @-> returning void)

    let sub_mul =
      foreign "lp_integer_sub_mul"
        (ptr (const lp_int_ring_t)
        @-> ptr lp_integer_t
        @-> ptr (const lp_integer_t)
        @-> ptr (const lp_integer_t)
        @-> returning void)

    let div_exact =
      foreign "lp_integer_div_exact"
        (ptr (const lp_int_ring_t)
        @-> ptr lp_integer_t
        @-> ptr (const lp_integer_t)
        @-> ptr (const lp_integer_t)
        @-> returning void)

    let div_Z =
      foreign "lp_integer_div_Z"
        (ptr lp_integer_t
        @-> ptr (const lp_integer_t)
        @-> ptr (const lp_integer_t)
        @-> returning void)

    let rem_Z =
      foreign "lp_integer_rem_Z"
        (ptr lp_integer_t
        @-> ptr (const lp_integer_t)
        @-> ptr (const lp_integer_t)
        @-> returning void)

    let div_rem_Z =
      foreign "lp_integer_div_rem_Z"
        (ptr lp_integer_t @-> ptr lp_integer_t
        @-> ptr (const lp_integer_t)
        @-> ptr (const lp_integer_t)
        @-> returning void)

    let div_rem_pow2_Z =
      foreign "lp_integer_div_rem_pow2_Z"
        (ptr lp_integer_t @-> ptr lp_integer_t
        @-> ptr (const lp_integer_t)
        @-> uint @-> returning void)

    let gcd_Z =
      foreign "lp_integer_gcd_Z"
        (ptr lp_integer_t
        @-> ptr (const lp_integer_t)
        @-> ptr (const lp_integer_t)
        @-> returning void)

    let lcm_Z =
      foreign "lp_integer_lcm_Z"
        (ptr lp_integer_t
        @-> ptr (const lp_integer_t)
        @-> ptr (const lp_integer_t)
        @-> returning void)

    let hash =
      foreign "lp_integer_hash" (ptr (const lp_integer_t) @-> returning size_t)
  end

  module Rational = struct
    open Types_generated

    let construct =
      foreign "lp_rational_construct" (ptr lp_rational_t @-> returning void)

    let construct_from_div =
      foreign "lp_rational_construct_from_div"
        (ptr lp_rational_t
        @-> ptr (const lp_integer_t)
        @-> ptr (const lp_integer_t)
        @-> returning void)

    let construct_from_int =
      foreign "lp_rational_construct_from_int"
        (ptr lp_rational_t @-> long @-> ulong @-> returning void)

    let construct_from_integer =
      foreign "lp_rational_construct_from_integer"
        (ptr lp_rational_t @-> ptr (const lp_integer_t) @-> returning void)

    let construct_from_double =
      foreign "lp_rational_construct_from_double"
        (ptr lp_rational_t @-> double @-> returning void)

    let construct_from_dyadic =
      foreign "lp_rational_construct_from_dyadic"
        (ptr lp_rational_t
        @-> ptr (const lp_dyadic_rational_t)
        @-> returning void)

    let construct_copy =
      foreign "lp_rational_construct_copy"
        (ptr lp_rational_t @-> ptr (const lp_rational_t) @-> returning void)

    let assign =
      foreign "lp_rational_assign"
        (ptr lp_rational_t @-> ptr (const lp_rational_t) @-> returning void)

    let assign_int =
      foreign "lp_rational_assign_int"
        (ptr lp_rational_t @-> long @-> ulong @-> returning void)

    let destruct =
      foreign "lp_rational_destruct" (ptr lp_rational_t @-> returning void)

    let to_string =
      foreign "lp_rational_to_string"
        (ptr (const lp_rational_t) @-> returning string)

    let to_double =
      foreign "lp_rational_to_double"
        (ptr (const lp_rational_t) @-> returning double)

    let sgn =
      foreign "lp_rational_sgn" (ptr (const lp_rational_t) @-> returning int)

    let cmp =
      foreign "lp_rational_cmp"
        (ptr (const lp_rational_t)
        @-> ptr (const lp_rational_t)
        @-> returning int)

    let cmp_dyadic_rational =
      foreign "lp_rational_cmp_dyadic_rational"
        (ptr (const lp_rational_t)
        @-> ptr (const lp_dyadic_rational_t)
        @-> returning int)

    let cmp_integer =
      foreign "lp_rational_cmp_integer"
        (ptr (const lp_rational_t)
        @-> ptr (const lp_integer_t)
        @-> returning int)

    let swap =
      foreign "lp_rational_swap"
        (ptr lp_rational_t @-> ptr lp_rational_t @-> returning void)

    let add =
      foreign "lp_rational_add"
        (ptr lp_rational_t
        @-> ptr (const lp_rational_t)
        @-> ptr (const lp_rational_t)
        @-> returning void)

    let add_integer =
      foreign "lp_rational_add_integer"
        (ptr lp_rational_t
        @-> ptr (const lp_rational_t)
        @-> ptr (const lp_integer_t)
        @-> returning void)

    let sub =
      foreign "lp_rational_sub"
        (ptr lp_rational_t
        @-> ptr (const lp_rational_t)
        @-> ptr (const lp_rational_t)
        @-> returning void)

    let neg =
      foreign "lp_rational_neg"
        (ptr lp_rational_t @-> ptr (const lp_rational_t) @-> returning void)

    let inv =
      foreign "lp_rational_inv"
        (ptr lp_rational_t @-> ptr (const lp_rational_t) @-> returning void)

    let mul =
      foreign "lp_rational_mul"
        (ptr lp_rational_t
        @-> ptr (const lp_rational_t)
        @-> ptr (const lp_rational_t)
        @-> returning void)

    let mul_2exp =
      foreign "lp_rational_mul_2exp"
        (ptr lp_rational_t
        @-> ptr (const lp_rational_t)
        @-> uint @-> returning void)

    let pow =
      foreign "lp_rational_pow"
        (ptr lp_rational_t
        @-> ptr (const lp_rational_t)
        @-> uint @-> returning void)

    let div_ =
      foreign "lp_rational_div"
        (ptr lp_rational_t
        @-> ptr (const lp_rational_t)
        @-> ptr (const lp_rational_t)
        @-> returning void)

    let div_2exp =
      foreign "lp_rational_div_2exp"
        (ptr lp_rational_t
        @-> ptr (const lp_rational_t)
        @-> uint @-> returning void)

    let get_num =
      foreign "lp_rational_get_num"
        (ptr (const lp_rational_t) @-> ptr lp_integer_t @-> returning void)

    let get_den =
      foreign "lp_rational_get_den"
        (ptr (const lp_rational_t) @-> ptr lp_integer_t @-> returning void)

    let is_integer =
      foreign "lp_rational_is_integer"
        (ptr (const lp_rational_t) @-> returning int)

    let ceiling =
      foreign "lp_rational_ceiling"
        (ptr (const lp_rational_t) @-> ptr lp_integer_t @-> returning void)

    let floor =
      foreign "lp_rational_floor"
        (ptr (const lp_rational_t) @-> ptr lp_integer_t @-> returning void)

    let hash =
      foreign "lp_rational_hash" (ptr (const lp_rational_t) @-> returning size_t)

    let hash_approx =
      foreign "lp_rational_hash_approx"
        (ptr (const lp_rational_t) @-> uint @-> returning size_t)
  end

  module Dyadic_rational = struct
    open Types_generated

    type t = lp_dyadic_rational_t ptr
  end

  module AlgebraicNumber = struct
    open Types_generated.AlgebraicNumber

    let to_string0 =
      foreign "lp_algebraic_number_to_string" (ptr t @-> returning (ptr char))

    let construct_zero =
      foreign "lp_algebraic_number_construct_zero" (ptr t @-> returning void)

    let destruct =
      foreign "lp_algebraic_number_destruct" (ptr t @-> returning void)
  end

  module Value = struct
    open Types_generated

    let construct =
      foreign "lp_value_construct"
        (ptr lp_value_t @-> lp_value_type_t
        @-> ptr (const void)
        @-> returning void)

    let construct_zero =
      foreign "lp_value_construct_zero" (ptr lp_value_t @-> returning void)

    let construct_int =
      foreign "lp_value_construct_int"
        (ptr lp_value_t @-> long @-> returning void)

    let construct_none =
      foreign "lp_value_construct_none" (ptr lp_value_t @-> returning void)

    let construct_copy =
      foreign "lp_value_construct_copy"
        (ptr lp_value_t @-> ptr (const lp_value_t) @-> returning void)

    let none =
      foreign "lp_value_none" (void @-> returning (ptr (const lp_value_t)))

    let minus_infinity =
      foreign "lp_value_minus_infinity"
        (void @-> returning (ptr (const lp_value_t)))

    let plus_infinity =
      foreign "lp_value_plus_infinity"
        (void @-> returning (ptr (const lp_value_t)))

    let new_ =
      foreign "lp_value_new"
        (lp_value_type_t @-> ptr (const void) @-> returning (ptr lp_value_t))

    let new_copy =
      foreign "lp_value_new_copy"
        (ptr (const lp_value_t) @-> returning (ptr lp_value_t))

    let destruct =
      foreign "lp_value_destruct" (ptr lp_value_t @-> returning void)

    let delete = foreign "lp_value_delete" (ptr lp_value_t @-> returning void)

    let cmp =
      foreign "lp_value_cmp"
        (ptr (const lp_value_t) @-> ptr (const lp_value_t) @-> returning int)

    let cmp_rational =
      foreign "lp_value_cmp_rational"
        (ptr (const lp_value_t) @-> ptr (const lp_rational_t) @-> returning int)

    let to_string =
      foreign "lp_value_to_string" (ptr (const lp_value_t) @-> returning string)

    let sgn = foreign "lp_value_sgn" (ptr (const lp_value_t) @-> returning int)

    let is_rational =
      foreign "lp_value_is_rational" (ptr (const lp_value_t) @-> returning bool)

    let is_integer =
      foreign "lp_value_is_integer" (ptr (const lp_value_t) @-> returning bool)

    let is_infinity =
      foreign "lp_value_is_infinity" (ptr (const lp_value_t) @-> returning bool)

    let get_rational =
      foreign "lp_value_get_rational"
        (ptr (const lp_value_t) @-> ptr lp_rational_t @-> returning void)

    let get_den =
      foreign "lp_value_get_den"
        (ptr (const lp_value_t) @-> ptr lp_integer_t @-> returning void)

    let get_num =
      foreign "lp_value_get_num"
        (ptr (const lp_value_t) @-> ptr lp_integer_t @-> returning void)

    let swap =
      foreign "lp_value_swap"
        (ptr lp_value_t @-> ptr lp_value_t @-> returning void)

    let add =
      foreign "lp_value_add"
        (ptr lp_value_t
        @-> ptr (const lp_value_t)
        @-> ptr (const lp_value_t)
        @-> returning void)

    let sub =
      foreign "lp_value_sub"
        (ptr lp_value_t
        @-> ptr (const lp_value_t)
        @-> ptr (const lp_value_t)
        @-> returning void)

    let neg =
      foreign "lp_value_neg"
        (ptr lp_value_t @-> ptr (const lp_value_t) @-> returning void)

    let mul =
      foreign "lp_value_mul"
        (ptr lp_value_t
        @-> ptr (const lp_value_t)
        @-> ptr (const lp_value_t)
        @-> returning void)

    let inv =
      foreign "lp_value_inv"
        (ptr lp_value_t @-> ptr (const lp_value_t) @-> returning void)

    let div =
      foreign "lp_value_div"
        (ptr lp_value_t
        @-> ptr (const lp_value_t)
        @-> ptr (const lp_value_t)
        @-> returning void)

    let pow =
      foreign "lp_value_pow"
        (ptr lp_value_t @-> ptr (const lp_value_t) @-> uint @-> returning void)
  end

  open Types_generated

  (* Variables *)

  let lp_variable_db_new =
    foreign "lp_variable_db_new" (void @-> returning (ptr lp_variable_db_t))

  let lp_variable_db_attach =
    foreign "lp_variable_db_attach" (ptr lp_variable_db_t @-> returning void)

  let lp_variable_db_detach =
    foreign "lp_variable_db_detach" (ptr lp_variable_db_t @-> returning void)

  let lp_variable_db_new_variable =
    foreign "lp_variable_db_new_variable"
      (ptr lp_variable_db_t @-> string @-> returning lp_variable_t)

  let lp_variable_db_add_variable =
    foreign "lp_variable_db_add_variable"
      (ptr lp_variable_db_t @-> lp_variable_t @-> string @-> returning void)

  let lp_variable_db_get_name =
    foreign "lp_variable_db_get_name"
      (ptr (const lp_variable_db_t) @-> lp_variable_t @-> returning string)

  let lp_variable_order_new =
    foreign "lp_variable_order_new"
      (void @-> returning (ptr lp_variable_order_t))

  let lp_variable_order_attach =
    foreign "lp_variable_order_attach"
      (ptr lp_variable_order_t @-> returning void)

  let lp_variable_order_detach =
    foreign "lp_variable_order_detach"
      (ptr lp_variable_order_t @-> returning void)

  let lp_variable_order_cmp =
    foreign "lp_variable_order_cmp"
      (ptr (const lp_variable_order_t)
      @-> lp_variable_t @-> lp_variable_t @-> returning int)

  let lp_variable_order_size =
    foreign "lp_variable_order_size"
      (ptr (const lp_variable_order_t) @-> returning size_t)

  let lp_variable_order_clear =
    foreign "lp_variable_order_clear"
      (ptr lp_variable_order_t @-> returning void)

  let lp_variable_order_contains =
    foreign "lp_variable_order_contains"
      (ptr lp_variable_order_t @-> lp_variable_t @-> returning bool)

  let lp_variable_order_push =
    foreign "lp_variable_order_push"
      (ptr lp_variable_order_t @-> lp_variable_t @-> returning void)

  let lp_variable_order_pop =
    foreign "lp_variable_order_pop" (ptr lp_variable_order_t @-> returning void)

  let lp_variable_order_reverse =
    foreign "lp_variable_order_reverse"
      (ptr lp_variable_order_t @-> returning void)

  let lp_variable_order_top =
    foreign "lp_variable_order_top"
      (ptr (const lp_variable_order_t) @-> returning lp_variable_t)

  let lp_variable_order_to_string =
    foreign "lp_variable_order_to_string"
      (ptr (const lp_variable_order_t)
      @-> ptr (const lp_variable_db_t)
      @-> returning string)

  let lp_variable_order_get_list =
    foreign "lp_variable_order_get_list"
      (ptr (const lp_variable_order_t)
      @-> returning (ptr (const lp_variable_list_t)))

  let lp_polynomial_context_new =
    foreign "lp_polynomial_context_new"
      (ptr lp_int_ring_t @-> ptr lp_variable_db_t @-> ptr lp_variable_order_t
      @-> returning (ptr lp_polynomial_context_t))

  let lp_polynomial_context_attach =
    foreign "lp_polynomial_context_attach"
      (ptr lp_polynomial_context_t @-> returning void)

  let lp_polynomial_context_detach =
    foreign "lp_polynomial_context_detach"
      (ptr lp_polynomial_context_t @-> returning void)

  let lp_polynomial_context_equal =
    foreign "lp_polynomial_context_equal"
      (ptr lp_polynomial_context_t
      @-> ptr lp_polynomial_context_t
      @-> returning bool)

  module Rational_interval = struct end

  module Assignment = struct
    let construct =
      foreign "lp_assignment_construct"
        (ptr lp_assignment_t @-> ptr (const lp_variable_db_t) @-> returning void)

    let new_ =
      foreign "lp_assignment_new"
        (ptr (const lp_variable_db_t) @-> returning (ptr lp_assignment_t))

    let destruct =
      foreign "lp_assignment_destruct" (ptr lp_assignment_t @-> returning void)

    let delete =
      foreign "lp_assignment_delete" (ptr lp_assignment_t @-> returning void)

    let to_string =
      foreign "lp_assignment_to_string"
        (ptr (const lp_assignment_t) @-> returning string)

    let set_value =
      foreign "lp_assignment_set_value"
        (ptr lp_assignment_t @-> lp_variable_t
        @-> ptr (const lp_value_t)
        @-> returning void)

    let get_value =
      foreign "lp_assignment_get_value"
        (ptr (const lp_assignment_t)
        @-> lp_variable_t
        @-> returning (ptr (const lp_value_t)))

    let get_value_approx =
      foreign "lp_assignment_get_value_approx"
        (ptr (const lp_assignment_t)
        @-> lp_variable_t @-> ptr lp_rational_interval_t @-> returning void)

    let sgn =
      foreign "lp_assignment_sgn"
        (ptr (const lp_assignment_t)
        @-> ptr (const lp_polynomial_t)
        @-> returning int)
  end

  module Monomial = struct
    let construct =
      foreign "lp_monomial_construct"
        (ptr (const lp_polynomial_context_t)
        @-> ptr lp_monomial_t @-> returning void)

    let construct_copy =
      foreign "lp_monomial_construct_copy"
        (ptr (const lp_polynomial_context_t)
        @-> ptr lp_monomial_t
        @-> ptr (const lp_monomial_t)
        @-> int @-> returning void)

    let set_coefficient =
      foreign "lp_monomial_set_coefficient"
        (ptr (const lp_polynomial_context_t)
        @-> ptr lp_monomial_t
        @-> ptr (const lp_integer_t)
        @-> returning void)

    let destruct =
      foreign "lp_monomial_destruct" (ptr lp_monomial_t @-> returning void)

    let clear =
      foreign "lp_monomial_clear"
        (ptr (const lp_polynomial_context_t)
        @-> ptr lp_monomial_t @-> returning void)

    let assign =
      foreign "lp_monomial_assign"
        (ptr (const lp_polynomial_context_t)
        @-> ptr lp_monomial_t
        @-> ptr (const lp_monomial_t)
        @-> int @-> returning void)

    let push =
      foreign "lp_monomial_push"
        (ptr lp_monomial_t @-> lp_variable_t @-> size_t @-> returning void)

    let pop = foreign "lp_monomial_pop" (ptr lp_monomial_t @-> returning void)

    let gcd =
      foreign "lp_monomial_gcd"
        (ptr (const lp_polynomial_context_t)
        @-> ptr lp_monomial_t
        @-> ptr (const lp_monomial_t)
        @-> ptr (const lp_monomial_t)
        @-> returning void)
  end

  module Polynomial = struct
    let construct =
      foreign "lp_polynomial_construct"
        (ptr lp_polynomial_t
        @-> ptr (const lp_polynomial_context_t)
        @-> returning void)

    let construct_simple =
      foreign "lp_polynomial_construct_simple"
        (ptr lp_polynomial_t
        @-> ptr (const lp_polynomial_context_t)
        @-> ptr (const lp_integer_t)
        @-> lp_variable_t @-> uint @-> returning void)

    let construct_copy =
      foreign "lp_polynomial_construct_copy"
        (ptr lp_polynomial_t @-> ptr lp_polynomial_t @-> returning void)

    let destruct =
      foreign "lp_polynomial_destruct" (ptr lp_polynomial_t @-> returning void)

    let delete =
      foreign "lp_polynomial_delete" (ptr lp_polynomial_t @-> returning void)

    let alloc =
      foreign "lp_polynomial_alloc" (void @-> returning (ptr lp_polynomial_t))

    let new_ =
      foreign "lp_polynomial_new"
        (ptr (const lp_polynomial_context_t) @-> returning (ptr lp_polynomial_t))

    let new_copy =
      foreign "lp_polynomial_new_copy"
        (ptr (const lp_polynomial_t) @-> returning (ptr lp_polynomial_t))

    let set_external =
      foreign "lp_polynomial_set_external"
        (ptr lp_polynomial_t @-> returning void)

    let check_order =
      foreign "lp_polynomial_check_order"
        (ptr (const lp_polynomial_t) @-> returning int)

    let ensure_order =
      foreign "lp_polynomial_ensure_order"
        (ptr lp_polynomial_t @-> returning void)

    let swap =
      foreign "lp_polynomial_swap"
        (ptr lp_polynomial_t @-> ptr lp_polynomial_t @-> returning void)

    let assign =
      foreign "lp_polynomial_assign"
        (ptr lp_polynomial_t @-> ptr (const lp_polynomial_t) @-> returning void)

    let degree =
      foreign "lp_polynomial_degree"
        (ptr (const lp_polynomial_t) @-> returning size_t)

    let top_variable =
      foreign "lp_polynomial_top_variable"
        (ptr (const lp_polynomial_t) @-> returning lp_variable_t)

    let is_linear =
      foreign "lp_polynomial_is_linear"
        (ptr (const lp_polynomial_t) @-> returning int)

    let lc_is_constant =
      foreign "lp_polynomial_lc_is_constant"
        (ptr (const lp_polynomial_t) @-> returning int)

    let lc_sgn =
      foreign "lp_polynomial_lc_sgn"
        (ptr (const lp_polynomial_t) @-> returning int)

    let get_context =
      foreign "lp_polynomial_get_context"
        (ptr (const lp_polynomial_t)
        @-> returning (ptr (const lp_polynomial_context_t)))

    let get_variables =
      foreign "lp_polynomial_get_variables"
        (ptr (const lp_polynomial_t)
        @-> ptr lp_variable_list_t @-> returning void)

    let get_coefficient =
      foreign "lp_polynomial_get_coefficient"
        (ptr lp_polynomial_t
        @-> ptr (const lp_polynomial_t)
        @-> size_t @-> returning void)

    let reductum =
      foreign "lp_polynomial_reductum"
        (ptr lp_polynomial_t @-> ptr (const lp_polynomial_t) @-> returning void)

    let reductum_m =
      foreign "lp_polynomial_reductum_m"
        (ptr lp_polynomial_t
        @-> ptr (const lp_polynomial_t)
        @-> ptr (const lp_assignment_t)
        @-> returning void)

    let is_constant =
      foreign "lp_polynomial_is_constant"
        (ptr (const lp_polynomial_t) @-> returning int)

    let is_zero =
      foreign "lp_polynomial_is_zero"
        (ptr (const lp_polynomial_t) @-> returning int)

    let is_univariate =
      foreign "lp_polynomial_is_univariate"
        (ptr (const lp_polynomial_t) @-> returning int)

    let is_univariate_m =
      foreign "lp_polynomial_is_univariate_m"
        (ptr (const lp_polynomial_t)
        @-> ptr (const lp_assignment_t)
        @-> returning int)

    let to_univariate =
      foreign "lp_polynomial_to_univariate"
        (ptr (const lp_polynomial_t) @-> returning (ptr lp_upolynomial_t))

    let is_assigned =
      foreign "lp_polynomial_is_assigned"
        (ptr (const lp_polynomial_t)
        @-> ptr (const lp_assignment_t)
        @-> returning int)

    let sgn =
      foreign "lp_polynomial_sgn"
        (ptr (const lp_polynomial_t)
        @-> ptr (const lp_assignment_t)
        @-> returning int)

    let interval_value =
      foreign "lp_polynomial_interval_value"
        (ptr (const lp_polynomial_t)
        @-> ptr (const lp_interval_assignment_t)
        @-> ptr lp_interval_t @-> returning void)

    
    let evaluate =
      foreign "lp_polynomial_evaluate"
        (ptr (const lp_polynomial_t)
        @-> ptr (const lp_assignment_t)
        @-> returning (ptr lp_value_t))

    let cmp =
      foreign "lp_polynomial_cmp"
        (ptr (const lp_polynomial_t)
        @-> ptr (const lp_polynomial_t)
        @-> returning int)

    let cmp_type =
      foreign "lp_polynomial_cmp_type"
        (ptr (const lp_polynomial_t)
        @-> ptr (const lp_polynomial_t)
        @-> returning int)

    let hash =
      foreign "lp_polynomial_hash"
        (ptr (const lp_polynomial_t) @-> returning size_t)

    let eq =
      foreign "lp_polynomial_eq"
        (ptr (const lp_polynomial_t)
        @-> ptr (const lp_polynomial_t)
        @-> returning int)

    let divides =
      foreign "lp_polynomial_divides"
        (ptr (const lp_polynomial_t)
        @-> ptr (const lp_polynomial_t)
        @-> returning int)

    let to_string =
      foreign "lp_polynomial_to_string"
        (ptr (const lp_polynomial_t) @-> returning string)

    let add =
      foreign "lp_polynomial_add"
        (ptr lp_polynomial_t
        @-> ptr (const lp_polynomial_t)
        @-> ptr (const lp_polynomial_t)
        @-> returning void)

    let add_monomial =
      foreign "lp_polynomial_add_monomial"
        (ptr lp_polynomial_t @-> ptr (const lp_monomial_t) @-> returning void)

    let sub =
      foreign "lp_polynomial_sub"
        (ptr lp_polynomial_t
        @-> ptr (const lp_polynomial_t)
        @-> ptr (const lp_polynomial_t)
        @-> returning void)

    let neg =
      foreign "lp_polynomial_neg"
        (ptr lp_polynomial_t @-> ptr (const lp_polynomial_t) @-> returning void)

    let mul =
      foreign "lp_polynomial_mul"
        (ptr lp_polynomial_t
        @-> ptr (const lp_polynomial_t)
        @-> ptr (const lp_polynomial_t)
        @-> returning void)

    let mul_integer =
      foreign "lp_polynomial_mul_integer"
        (ptr lp_polynomial_t
        @-> ptr (const lp_polynomial_t)
        @-> ptr (const lp_integer_t)
        @-> returning void)

    let shl =
      foreign "lp_polynomial_shl"
        (ptr lp_polynomial_t
        @-> ptr (const lp_polynomial_t)
        @-> uint @-> returning void)

    let pow =
      foreign "lp_polynomial_pow"
        (ptr lp_polynomial_t
        @-> ptr (const lp_polynomial_t)
        @-> uint @-> returning void)

    let add_mul =
      foreign "lp_polynomial_add_mul"
        (ptr lp_polynomial_t
        @-> ptr (const lp_polynomial_t)
        @-> ptr (const lp_polynomial_t)
        @-> returning void)

    let sub_mul =
      foreign "lp_polynomial_sub_mul"
        (ptr lp_polynomial_t
        @-> ptr (const lp_polynomial_t)
        @-> ptr (const lp_polynomial_t)
        @-> returning void)

    let reduce =
      foreign "lp_polynomial_reduce"
        (ptr (const lp_polynomial_t)
        @-> ptr (const lp_polynomial_t)
        @-> ptr lp_polynomial_t @-> ptr lp_polynomial_t @-> ptr lp_polynomial_t
        @-> returning void)

    let div =
      foreign "lp_polynomial_div"
        (ptr lp_polynomial_t
        @-> ptr (const lp_polynomial_t)
        @-> ptr (const lp_polynomial_t)
        @-> returning void)

    let rem =
      foreign "lp_polynomial_rem"
        (ptr lp_polynomial_t
        @-> ptr (const lp_polynomial_t)
        @-> ptr (const lp_polynomial_t)
        @-> returning void)

    let prem =
      foreign "lp_polynomial_prem"
        (ptr lp_polynomial_t
        @-> ptr (const lp_polynomial_t)
        @-> ptr (const lp_polynomial_t)
        @-> returning void)

    let sprem =
      foreign "lp_polynomial_sprem"
        (ptr lp_polynomial_t
        @-> ptr (const lp_polynomial_t)
        @-> ptr (const lp_polynomial_t)
        @-> returning void)

    let divrem =
      foreign "lp_polynomial_divrem"
        (ptr lp_polynomial_t @-> ptr lp_polynomial_t
        @-> ptr (const lp_polynomial_t)
        @-> ptr (const lp_polynomial_t)
        @-> returning void)

    let derivative =
      foreign "lp_polynomial_derivative"
        (ptr lp_polynomial_t @-> ptr (const lp_polynomial_t) @-> returning void)

    let gcd =
      foreign "lp_polynomial_gcd"
        (ptr lp_polynomial_t
        @-> ptr (const lp_polynomial_t)
        @-> ptr (const lp_polynomial_t)
        @-> returning void)

    let lcm =
      foreign "lp_polynomial_lcm"
        (ptr lp_polynomial_t
        @-> ptr (const lp_polynomial_t)
        @-> ptr (const lp_polynomial_t)
        @-> returning void)

    let cont =
      foreign "lp_polynomial_cont"
        (ptr lp_polynomial_t @-> ptr (const lp_polynomial_t) @-> returning void)

    let pp =
      foreign "lp_polynomial_pp"
        (ptr lp_polynomial_t @-> ptr (const lp_polynomial_t) @-> returning void)

    let pp_cont =
      foreign "lp_polynomial_pp_cont"
        (ptr lp_polynomial_t @-> ptr lp_polynomial_t
        @-> ptr (const lp_polynomial_t)
        @-> returning void)

    let resultant =
      foreign "lp_polynomial_resultant"
        (ptr lp_polynomial_t
        @-> ptr (const lp_polynomial_t)
        @-> ptr (const lp_polynomial_t)
        @-> returning void)

    let psc =
      foreign "lp_polynomial_psc"
        (ptr (ptr lp_polynomial_t)
        @-> ptr (const lp_polynomial_t)
        @-> ptr (const lp_polynomial_t)
        @-> returning void)

    let mgcd =
      foreign "lp_polynomial_mgcd"
        (ptr (const lp_polynomial_t)
        @-> ptr (const lp_polynomial_t)
        @-> ptr (const lp_assignment_t)
        @-> returning (ptr lp_polynomial_vector_t))

    let factor_square_free =
      foreign "lp_polynomial_factor_square_free"
        (ptr (const lp_polynomial_t)
        @-> ptr (ptr (ptr lp_polynomial_t))
        @-> ptr (ptr size_t)
        @-> ptr size_t @-> returning void)

    let factor_content_free =
      foreign "lp_polynomial_factor_content_free"
        (ptr (const lp_polynomial_t)
        @-> ptr (ptr (ptr lp_polynomial_t))
        @-> ptr (ptr size_t)
        @-> ptr size_t @-> returning void)

    let roots_isolate =
      foreign "lp_polynomial_roots_isolate"
        (ptr (const lp_polynomial_t)
        @-> ptr (const lp_assignment_t)
        @-> ptr lp_value_t @-> ptr size_t @-> returning void)

    let constraint_get_feasible_set =
      foreign "lp_polynomial_constraint_get_feasible_set"
        (ptr (const lp_polynomial_t)
        @-> lp_sign_condition_t @-> int
        @-> ptr (const lp_assignment_t)
        @-> returning (ptr lp_feasibility_set_t))

    let constraint_infer_bounds =
      foreign "lp_polynomial_constraint_infer_bounds"
        (ptr (const lp_polynomial_t)
        @-> lp_sign_condition_t @-> int
        @-> ptr lp_interval_assignment_t
        @-> returning int)

    let constraint_explain_infer_bounds =
      foreign "lp_polynomial_constraint_explain_infer_bounds"
        (ptr (const lp_polynomial_t)
        @-> lp_sign_condition_t @-> int @-> lp_variable_t
        @-> returning (ptr lp_polynomial_t))

    let constraint_evaluate =
      foreign "lp_polynomial_constraint_evaluate"
        (ptr (const lp_polynomial_t)
        @-> lp_sign_condition_t
        @-> ptr (const lp_assignment_t)
        @-> returning int)

    let root_constraint_get_feasible_set =
      foreign "lp_polynomial_root_constraint_get_feasible_set"
        (ptr (const lp_polynomial_t)
        @-> size_t @-> lp_sign_condition_t @-> int
        @-> ptr (const lp_assignment_t)
        @-> returning (ptr lp_feasibility_set_t))

    let root_constraint_evaluate =
      foreign "lp_polynomial_root_constraint_evaluate"
        (ptr (const lp_polynomial_t)
        @-> size_t @-> lp_sign_condition_t
        @-> ptr (const lp_assignment_t)
        @-> returning int)

    let traverse =
      foreign "lp_polynomial_traverse"
        (ptr (const lp_polynomial_t)
        @-> Foreign.funptr lp_polynomial_traverse_f
        @-> ptr void @-> returning void)

    let check_integrity =
      foreign "lp_polynomial_check_integrity"
        (ptr (const lp_polynomial_t) @-> returning int)

    let constraint_resolve_fm =
      foreign "lp_polynomial_constraint_resolve_fm"
        (ptr (const lp_polynomial_t)
        @-> lp_sign_condition_t
        @-> ptr (const lp_polynomial_t)
        @-> lp_sign_condition_t
        @-> ptr (const lp_assignment_t)
        @-> ptr lp_polynomial_t @-> ptr lp_sign_condition_t
        @-> ptr lp_polynomial_vector_t @-> returning int)
  end
end
