#ifndef MOPR_WRAP_USD_EXT_TOKEN_H
#define MOPR_WRAP_USD_EXT_TOKEN_H

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
    // TF TOKEN WRAPPER
    //

    MOPR_DECLARE_HANDLE( MoprToken )

    MOPR_DECLARE_RAII_FUNCTIONS( MoprToken, token )

    /* Query */

    MOPR_API
    _Bool
     mopr_token_is_empty_p( MoprToken_ch this_ch );

    MOPR_API
    char const *
     mopr_token_cstr( MoprToken_ch this_ch );

    //
    // API
    //

    /* Constructor */

    MOPR_API
    void
     mopr_token_ctor( MoprToken_h this_h );

    MOPR_API
    void
     mopr_token_ctor_cstr( MoprToken_h this_h, char const * token_cstr );

    MOPR_API
    void
     mopr_token_ctor_string( MoprToken_h this_h, MoprString_ch string_ch );

#ifdef __cplusplus
}
#endif

#endif   // MOPR_WRAP_USD_EXT_TOKEN_H
