#ifndef MOPR_WRAP_STD_EXT_STRING_H
#define MOPR_WRAP_STD_EXT_STRING_H

// Wrap external includes.

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
    // TF STRING WRAPPER
    //

    MOPR_DECLARE_HANDLE( MoprString )

    MOPR_DECLARE_RAII_FUNCTIONS( MoprString, string )

    /* Query */

    MOPR_API MOPR_BOOL
     mopr_string_is_empty_p( MoprString_ch this_ch );

    //
    // API
    //

    /* Constructor */

    MOPR_API void
     mopr_string_ctor( MoprString_h this_h );

    MOPR_API void
     mopr_string_ctor_cstr( MoprString_h this_h, char const * cstr );

    /* Query */

    MOPR_API char const *
     mopr_string_cstr( MoprString_ch this_ch );

#ifdef __cplusplus
}
#endif

#endif   // MOPR_WRAP_STD_EXT_STRING_H
