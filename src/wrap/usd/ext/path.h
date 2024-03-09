#ifndef MOPR_WRAP_USD_EXT_PATH_H
#define MOPR_WRAP_USD_EXT_PATH_H

// Wrap external std includes.
#include "wrap/std/ext/string.h"

// Wrap external includes.
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
    // SDF PATH WRAPPER
    //

    MOPR_DECLARE_HANDLE( MoprPath )

    MOPR_DECLARE_RAII_FUNCTIONS( MoprPath, path )

    /* Query */

    MOPR_API MOPR_BOOL
     mopr_path_is_empty_p( MoprPath_ch this_ch );

    //
    // API
    //

    /* Constructor */

    MOPR_API void
     mopr_path_ctor( MoprPath_h this_h );

    MOPR_API void
     mopr_path_ctor_cstr( MoprPath_h this_h, char const * path_cstr );

    /* Query */

    MOPR_API size_t
     mopr_path_get_path_element_count( MoprPath_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_path_is_absolute_path_p( MoprPath_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_path_is_absolute_root_path_p( MoprPath_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_path_is_prim_path_p( MoprPath_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_path_is_absolute_root_or_prim_path_p( MoprPath_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_path_is_root_prim_path_p( MoprPath_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_path_is_property_path_p( MoprPath_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_path_is_prim_property_path_p( MoprPath_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_path_is_namespaced_property_path_p( MoprPath_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_path_is_prim_variant_selection_path_p( MoprPath_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_path_is_prim_or_prim_variant_selection_path_p( MoprPath_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_path_contains_prim_variant_selection_p( MoprPath_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_path_contains_property_elements_p( MoprPath_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_path_contains_target_path_p( MoprPath_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_path_is_relational_attribute_path_p( MoprPath_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_path_is_target_path_p( MoprPath_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_path_is_mapper_path_p( MoprPath_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_path_is_mapper_arg_path_p( MoprPath_ch this_ch );

    MOPR_API MOPR_BOOL
     mopr_path_is_expression_path_p( MoprPath_ch this_ch );

    MOPR_API void
     mopr_path_get_as_string( MoprString_h string_h, MoprPath_ch this_ch );

    MOPR_API void
     mopr_path_get_as_token( MoprToken_h token_h, MoprPath_ch this_ch );

#ifdef __cplusplus
}
#endif

#endif   // MOPR_WRAP_USD_EXT_PATH_H
