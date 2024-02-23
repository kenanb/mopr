#ifndef MOPR_WRAP_USD_EXT_PRIM_H
#define MOPR_WRAP_USD_EXT_PRIM_H

// Wrap external std includes.
#include "wrap/std/ext/string.h"

// Wrap external includes.
#include "attribute.h"
#include "path.h"
#include "token.h"
#include "valueTypeName.h"

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
    // USD PRIM WRAPPER
    //

    MOPR_DECLARE_HANDLE( MoprPrim )

    MOPR_DECLARE_RAII_FUNCTIONS( MoprPrim, prim )

    /* Query */

    MOPR_API
    _Bool
     mopr_prim_is_empty_p( MoprPrim_ch this_ch );

    //
    // API
    //

    /* Constructor */

    MOPR_API
    void
     mopr_prim_ctor( MoprPrim_h this_h );

    /* Query */

    MOPR_API
    void
     mopr_prim_set_type_name( MoprPrim_h this_h, MoprToken_ch token_ch );

    MOPR_API
    _Bool
     mopr_prim_create_attribute( MoprAttribute_h attribute_h,
                                 MoprPrim_ch this_ch,
                                 MoprToken_ch name_token_ch,
                                 MoprValueTypeName_ch value_type_name_ch,
                                 _Bool custom,
                                 MoprPropertyVariability variability );

#ifdef __cplusplus
}
#endif

#endif   // MOPR_WRAP_USD_EXT_PRIM_H
