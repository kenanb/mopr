#include "datum.h"

// Wrap internal includes.
#include "wrap/usd/box/datum.h"

#define MOPR_APPLY_TYPE( T_name, T_text, T_type, T_dims, T_prim_h )                      \
    MOPR_DEFINE_RAII_FUNCTIONS( MoprDatum_##T_text, datum_##T_text )                     \
    _Bool mopr_datum_##T_text##_is_empty_p( MoprDatum_##T_text##_ch this_ch )            \
    {                                                                                    \
        return false;                                                                    \
    }                                                                                    \
    void mopr_datum_##T_text##_ctor( MoprDatum_##T_text##_h this_h )                     \
    {                                                                                    \
        this_h->d = { };                                                                 \
    }                                                                                    \
    T_prim_h mopr_datum_##T_text##_row_major_aref( MoprDatum_##T_text##_h this_h,        \
                                                   int num )                             \
    {                                                                                    \
        return this_h->row_major_aref( num );                                            \
    }                                                                                    \
    int mopr_datum_##T_text##_get_dim( )                                                 \
    {                                                                                    \
        return MoprDatum_##T_text::get_dim( );                                           \
    }                                                                                    \
    int mopr_datum_##T_text##_get_primitive_sizeof( )                                    \
    {                                                                                    \
        return MoprDatum_##T_text::get_primitive_sizeof( );                              \
    }                                                                                    \
    int mopr_datum_##T_text##_get_primitive_alignof( )                                   \
    {                                                                                    \
        return MoprDatum_##T_text::get_primitive_alignof( );                             \
    }
MOPR_SCALAR_VALUE_TYPES
MOPR_VECTOR_VALUE_TYPES
#undef MOPR_APPLY_TYPE
