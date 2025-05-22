#include <gmp.h>
#include <zarith.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

#include <poly/integer.h>
#include <poly/rational.h>

#include <ctypes_cstubs_internals.h>

CAMLprim value ml_lp_integer_construct_from_z(value p_value, value z) {
    lp_integer_t *p = CTYPES_ADDR_OF_FATPTR(p_value);
    lp_integer_construct(p);
    ml_z_mpz_set_z(p, z);
    return Val_unit;
}

CAMLprim value ml_lp_integer_get_z(value p_value) {
    lp_integer_t *p = CTYPES_ADDR_OF_FATPTR(p_value);
    return ml_z_from_mpz(p);
}

CAMLprim value ml_lp_rational_construct_from_div_z(value mpz_p_value, value num, value den)
{
    lp_rational_t *p = CTYPES_ADDR_OF_FATPTR(mpz_p_value);
    ml_z_mpz_set_z(&p->_mp_num, num);
    ml_z_mpz_set_z(&p->_mp_den, den);
    mpq_canonicalize(p);
    return Val_unit;
}

CAMLprim value ml_lp_rational_get_num_z(value p_value)
{
    lp_rational_t *p = CTYPES_ADDR_OF_FATPTR(p_value);
    return ml_z_from_mpz(&p->_mp_num);
}

CAMLprim value ml_lp_rational_get_den_z(value p_value)
{
    lp_rational_t *p = CTYPES_ADDR_OF_FATPTR(p_value);
    return ml_z_from_mpz(&p->_mp_den);
}
