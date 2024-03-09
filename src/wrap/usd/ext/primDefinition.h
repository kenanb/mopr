#ifndef MOPR_WRAP_USD_EXT_PRIM_DEFINITION_H
#define MOPR_WRAP_USD_EXT_PRIM_DEFINITION_H

// Wrap external includes.
#include "propertyDefinition.h"
#include "token.h"

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
    // USD PRIM DEFINITION WRAPPER
    //

    MOPR_DECLARE_HANDLE( MoprPrimDefinition )

    MOPR_DECLARE_RAII_FUNCTIONS( MoprPrimDefinition, prim_definition )

    /* Query */

    MOPR_API MOPR_BOOL
     mopr_prim_definition_is_empty_p( MoprPrimDefinition_ch this_ch );

    //
    // API
    //

    /* Constructor */

    MOPR_API void
     mopr_prim_definition_ctor_api( MoprPrimDefinition_h this_h, MoprToken_ch token_ch );

    MOPR_API void
     mopr_prim_definition_ctor_isa( MoprPrimDefinition_h this_h, MoprToken_ch token_ch );

    MOPR_API int
     mopr_prim_definition_get_property_count( MoprPrimDefinition_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_prim_definition_get_property_name( MoprToken_h token_h,
                                             MoprPrimDefinition_ch this_ch,
                                             int i );

    MOPR_API MOPR_BOOL
     mopr_prim_definition_get_property( MoprPropertyDefinition_h property_h,
                                        MoprPrimDefinition_ch this_ch,
                                        MoprToken_ch token_ch );

#ifdef __cplusplus
}
#endif

#endif   // MOPR_WRAP_USD_EXT_PRIM_DEFINITION_H
