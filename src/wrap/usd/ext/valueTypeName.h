#ifndef MOPR_WRAP_USD_EXT_VALUE_TYPE_NAME_H
#define MOPR_WRAP_USD_EXT_VALUE_TYPE_NAME_H

#include "token.h"

// Wrap external std includes.
#include "wrap/std/ext/string.h"

// Wrap util includes.
#include "wrap/_base/ext/common.h"
#include "wrap/_base/ext/prolog.h"

// Generic includes.
#include "base/api.h"

#ifdef __cplusplus
extern "C"
{
#endif

    //
    // SDF VALUE TYPE NAME WRAPPER
    //

    MOPR_DECLARE_HANDLE( MoprValueTypeName )

    MOPR_DECLARE_RAII_FUNCTIONS( MoprValueTypeName, value_type_name )

    /* Query */

    MOPR_API MOPR_BOOL
     mopr_value_type_name_is_empty_p( MoprValueTypeName_ch this_ch );

    //
    // API
    //

    /* Constructor */

    MOPR_API void
     mopr_value_type_name_ctor( MoprValueTypeName_h this_h );

    MOPR_API void
     mopr_value_type_name_find_cstr( MoprValueTypeName_h this_h,
                                     char const * type_name_cstr );

    MOPR_API MOPR_BOOL
     mopr_value_type_name_is_scalar_p( MoprValueTypeName_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_value_type_name_is_array_p( MoprValueTypeName_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_value_type_name_get_scalar_type( MoprValueTypeName_h other_h,
                                           MoprValueTypeName_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_value_type_name_get_array_type( MoprValueTypeName_h other_h,
                                          MoprValueTypeName_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_value_type_name_get_role( MoprToken_h token_h, MoprValueTypeName_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_value_type_name_get_as_token( MoprToken_h token_h,
                                        MoprValueTypeName_ch this_ch );

    MOPR_API unsigned char
     mopr_value_type_name_get_dimension( MoprValueTypeName_ch this_ch, int index );

#ifdef __cplusplus
}
#endif

#endif   // MOPR_WRAP_USD_EXT_VALUE_TYPE_NAME_H
