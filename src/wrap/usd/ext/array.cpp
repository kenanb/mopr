#include "array.h"

// Wrap internal includes.
#include "wrap/usd/box/array.h"

#define MOPR_APPLY_TYPE( T_name, T_text, T_type, T_dims, T_prim_h )                      \
    MOPR_DEFINE_RAII_FUNCTIONS( MoprArray_##T_text, array_##T_text )                     \
    MOPR_BOOL mopr_array_##T_text##_is_empty_p( MoprArray_##T_text##_ch this_ch )        \
    {                                                                                    \
        return this_ch->d.empty( );                                                      \
    }                                                                                    \
    void mopr_array_##T_text##_ctor( MoprArray_##T_text##_h this_h )                     \
    {                                                                                    \
        this_h->d = pxr::VtArray< T_type >( );                                           \
    }                                                                                    \
    void mopr_array_##T_text##_reserve( MoprArray_##T_text##_h this_h, int num )         \
    {                                                                                    \
        this_h->d.reserve( num );                                                        \
    }                                                                                    \
    void mopr_array_##T_text##_resize( MoprArray_##T_text##_h this_h, int num )          \
    {                                                                                    \
        this_h->d.resize( num );                                                         \
    }                                                                                    \
    T_prim_h mopr_array_##T_text##_aref( MoprArray_##T_text##_h this_h, int num )        \
    {                                                                                    \
        return this_h->aref( num );                                                      \
    }                                                                                    \
    T_prim_h mopr_array_##T_text##_row_major_aref( MoprArray_##T_text##_h this_h,        \
                                                   int num )                             \
    {                                                                                    \
        return this_h->row_major_aref( num );                                            \
    }                                                                                    \
    int mopr_array_##T_text##_get_dim( )                                                 \
    {                                                                                    \
        return MoprArray_##T_text::get_dim( );                                           \
    }                                                                                    \
    int mopr_array_##T_text##_get_primitive_sizeof( )                                    \
    {                                                                                    \
        return MoprArray_##T_text::get_primitive_sizeof( );                              \
    }                                                                                    \
    int mopr_array_##T_text##_get_primitive_alignof( )                                   \
    {                                                                                    \
        return MoprArray_##T_text::get_primitive_alignof( );                             \
    }                                                                                    \
    int mopr_array_##T_text##_get_array_type_sizeof( )                                   \
    {                                                                                    \
        return MoprArray_##T_text::get_array_type_sizeof( );                             \
    }                                                                                    \
    int mopr_array_##T_text##_get_array_type_alignof( )                                  \
    {                                                                                    \
        return MoprArray_##T_text::get_array_type_alignof( );                            \
    }
MOPR_SCALAR_VALUE_TYPES
MOPR_VECTOR_VALUE_TYPES
#undef MOPR_APPLY_TYPE
