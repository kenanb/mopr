#ifndef MOPR_WRAP_USD_EXT_STAGE_H
#define MOPR_WRAP_USD_EXT_STAGE_H

// Wrap external includes.
#include "layer.h"
#include "path.h"
#include "prim.h"

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
    // USD STAGE REF POINTER WRAPPER
    //

    MOPR_DECLARE_HANDLE( MoprStage )

    MOPR_DECLARE_RAII_FUNCTIONS( MoprStage, stage )

    /* Query */

    MOPR_API MOPR_BOOL
     mopr_stage_is_empty_p( MoprStage_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_stage_try_upgrade( MoprStage_h this_h );

    MOPR_API void
     mopr_stage_downgrade( MoprStage_h this_h );

    //
    // API
    //

    /* Constructor */

    MOPR_API MOPR_BOOL
     mopr_stage_create_new( MoprStage_h this_h, char const * id_cstr );

    MOPR_API MOPR_BOOL
     mopr_stage_open_layer( MoprStage_h this_h, MoprLayer_h layer_h );

    /* Query */

    MOPR_API MOPR_BOOL
     mopr_stage_get_root_layer_w( MoprLayer_h layer_h, MoprStage_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_stage_define_prim( MoprPrim_h prim_h, MoprStage_h this_h, MoprPath_ch path_ch );

    MOPR_API MOPR_BOOL
     mopr_stage_override_prim( MoprPrim_h prim_h,
                               MoprStage_h this_h,
                               MoprPath_ch path_ch );

    MOPR_API MOPR_BOOL
     mopr_stage_create_class_prim( MoprPrim_h prim_h,
                                   MoprStage_h this_h,
                                   MoprPath_ch path_ch );

    MOPR_API MOPR_BOOL
     mopr_stage_get_prim_at_path( MoprPrim_h prim_h,
                                  MoprStage_ch this_ch,
                                  MoprPath_ch path_ch );

#ifdef __cplusplus
}
#endif

#endif   // MOPR_WRAP_USD_EXT_STAGE_H
