#ifndef MOPR_WRAP_USD_EXT_ARRAY_H
#define MOPR_WRAP_USD_EXT_ARRAY_H

// Wrap util includes.
#include "wrap/_base/ext/common.h"
#include "wrap/_base/ext/prolog.h"
#include "wrap/_base/ext/types.h"
#include "wrap/usd/ext/timecode.h"
#include "wrap/usd/ext/token.h"

// Generic includes.
#include "base/api.h"

#ifdef __cplusplus
extern "C"
{
#endif

    //
    // VT ARRAY WRAPPER
    //

#define MOPR_APPLY_TYPE( T_name, T_text, T_type, T_dims, T_prim_h )                      \
    MOPR_DECLARE_HANDLE( MoprArray_##T_text )                                            \
    MOPR_DECLARE_RAII_FUNCTIONS( MoprArray_##T_text, array_##T_text )                    \
    MOPR_API _Bool mopr_array_##T_text##_is_empty_p( MoprArray_##T_text##_ch this_ch );  \
    MOPR_API void mopr_array_##T_text##_ctor( MoprArray_##T_text##_h this_h );           \
    MOPR_API void mopr_array_##T_text##_reserve( MoprArray_##T_text##_h this_h,          \
                                                 int num );                              \
    MOPR_API void mopr_array_##T_text##_resize( MoprArray_##T_text##_h this_h,           \
                                                int num );                               \
    MOPR_API T_prim_h mopr_array_##T_text##_aref( MoprArray_##T_text##_h this_h,         \
                                                  int num );                             \
    MOPR_API T_prim_h mopr_array_##T_text##_row_major_aref(                              \
     MoprArray_##T_text##_h this_h, int num );                                           \
    MOPR_API int mopr_array_##T_text##_get_dim( );                                       \
    MOPR_API int mopr_array_##T_text##_get_primitive_sizeof( );                          \
    MOPR_API int mopr_array_##T_text##_get_primitive_alignof( );                         \
    MOPR_API int mopr_array_##T_text##_get_array_type_sizeof( );                         \
    MOPR_API int mopr_array_##T_text##_get_array_type_alignof( );
    MOPR_SCALAR_VALUE_TYPES
    MOPR_VECTOR_VALUE_TYPES
#undef MOPR_APPLY_TYPE

#ifdef __cplusplus
}
#endif

#endif   // MOPR_WRAP_USD_EXT_ARRAY_H
