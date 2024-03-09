#ifndef MOPR_WRAP_USD_EXT_LAYER_H
#define MOPR_WRAP_USD_EXT_LAYER_H

// Wrap external std includes.
#include "wrap/std/ext/string.h"

// Wrap external includes.
#include "path.h"

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
    // SDF LAYER POINTER WRAPPER
    //

    MOPR_DECLARE_HANDLE( MoprLayer )

    MOPR_DECLARE_RAII_FUNCTIONS( MoprLayer, layer )

    /* Query */

    MOPR_API MOPR_BOOL
     mopr_layer_is_empty_p( MoprLayer_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_layer_try_upgrade( MoprLayer_h this_h );

    MOPR_API void
     mopr_layer_downgrade( MoprLayer_h this_h );

    //
    // API
    //

    /* Constructor */

    MOPR_API MOPR_BOOL
     mopr_layer_create_new( MoprLayer_h this_h, char const * id_cstr );

    MOPR_API MOPR_BOOL
     mopr_layer_create_anonymous( MoprLayer_h this_h, char const * id_cstr );

    /* TODO: External references */

    /* Query */

    MOPR_API MOPR_BOOL
     mopr_layer_get_identifier( MoprString_h id_string_h, MoprLayer_ch this_ch );

    /* Export */

    MOPR_API MOPR_BOOL
     mopr_layer_export_to_string( MoprString_h o_string_h, MoprLayer_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_layer_save( MoprLayer_ch this_ch );

#ifdef __cplusplus
}
#endif

#endif   // MOPR_WRAP_USD_EXT_LAYER_H
