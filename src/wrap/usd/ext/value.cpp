#include "value.h"

// Wrap internal includes.
#include "wrap/usd/box/array.h"
#include "wrap/usd/box/datum.h"
#include "wrap/usd/box/value.h"

MOPR_DEFINE_RAII_FUNCTIONS( MoprValue, value )

/* Constructor */

void
 mopr_value_ctor( MoprValue_h this_h )
{
    this_h->d = pxr::VtValue( );
}

/* Query */

_Bool
 mopr_value_is_empty_p( MoprValue_ch this_ch )
{
    return this_ch->d.IsEmpty( );
}

#define MOPR_APPLY_TYPE( T_name, T_text, T_type, T_dims, T_prim_h )                      \
    void mopr_value_assign_datum_##T_text( MoprValue_h this_h,                           \
                                           MoprDatum_##T_text##_ch datum_ch )            \
    {                                                                                    \
        this_h->d = pxr::VtValue( datum_ch->d );                                         \
    }
MOPR_SCALAR_VALUE_TYPES
MOPR_VECTOR_VALUE_TYPES
#undef MOPR_APPLY_TYPE

#define MOPR_APPLY_TYPE( T_name, T_text, T_type, T_dims, T_prim_h )                      \
    void mopr_value_assign_array_##T_text( MoprValue_h this_h,                           \
                                           MoprArray_##T_text##_ch array_ch )            \
    {                                                                                    \
        this_h->d = pxr::VtValue( array_ch->d );                                         \
    }
MOPR_SCALAR_VALUE_TYPES
MOPR_VECTOR_VALUE_TYPES
#undef MOPR_APPLY_TYPE
