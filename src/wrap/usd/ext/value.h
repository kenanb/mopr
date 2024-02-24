#ifndef MOPR_WRAP_USD_EXT_VALUE_H
#define MOPR_WRAP_USD_EXT_VALUE_H

// Wrap util includes.
#include "wrap/_base/ext/common.h"
#include "wrap/_base/ext/prolog.h"
#include "wrap/_base/ext/types.h"

#include "wrap/usd/ext/array.h"
#include "wrap/usd/ext/datum.h"

// Generic includes.
#include "base/api.h"

#ifdef __cplusplus
extern "C"
{
#endif

    //
    // VT VALUE WRAPPER
    //

    MOPR_DECLARE_HANDLE( MoprValue )

    MOPR_DECLARE_RAII_FUNCTIONS( MoprValue, value )

    /* Query */

    MOPR_API
    _Bool
     mopr_value_is_empty_p( MoprValue_ch this_ch );

    //
    // API
    //

    /* Constructor */

    MOPR_API
    void
     mopr_value_ctor( MoprValue_h this_h );

#define MOPR_APPLY_TYPE( T_name, T_text, T_type, T_dims, T_prim_h )                      \
    MOPR_API void mopr_value_assign_datum_##T_text( MoprValue_h this_h,                  \
                                                    MoprDatum_##T_text##_ch datum_ch );
    MOPR_SCALAR_VALUE_TYPES
    MOPR_VECTOR_VALUE_TYPES
#undef MOPR_APPLY_TYPE

#define MOPR_APPLY_TYPE( T_name, T_text, T_type, T_dims, T_prim_h )                      \
    MOPR_API void mopr_value_assign_array_##T_text( MoprValue_h this_h,                  \
                                                    MoprArray_##T_text##_ch array_ch );
    MOPR_SCALAR_VALUE_TYPES
    MOPR_VECTOR_VALUE_TYPES
#undef MOPR_APPLY_TYPE

#ifdef __cplusplus
}
#endif

#endif   // MOPR_WRAP_USD_EXT_VALUE_H
