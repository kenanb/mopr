#ifndef MOPR_WRAP_USD_BOX_ARRAY_H
#define MOPR_WRAP_USD_BOX_ARRAY_H

#include "wrap/usd/box/datumBase.h"

#include "wrap/_base/ext/types.h"

#include "pxr/base/vt/array.h"

template < typename T >
struct MOPR_API MoprArray
    : public MoprDatumGeneric< T >
{
    pxr::VtArray< T > d;
    typedef MoprDatumBase< T > base;
    typedef MoprDatumGeneric< T > gen;
    typedef typename gen::PrimitiveAccess primitive_access;
    using MoprDatumBase< T >::dim;

    static constexpr int
     get_array_type_sizeof( )
    {
        return sizeof( T );
    }

    static constexpr int
     get_array_type_alignof( )
    {
        return alignof( T );
    }

    typename base::target_primitive_type *
     aref( int idx )
    {
        T & ref = d[ idx ];
        primitive_access * in = reinterpret_cast< primitive_access * >( &ref );
        return &in->t[ 0 ];
    }

    typename base::target_primitive_type *
     row_major_aref( int i )
    {
        int idx = i / dim;
        int col = i % dim;
        T & ref = d[ idx ];
        primitive_access * in = reinterpret_cast< primitive_access * >( &ref );
        return &in->t[ col ];
    }
};

#define MOPR_APPLY_TYPE( T_name, T_text, T_type, T_dims, T_prim_h )                      \
    struct MOPR_API MoprArray_##T_text : public MoprArray< T_type >                      \
    {                                                                                    \
    };
MOPR_SCALAR_VALUE_TYPES
MOPR_VECTOR_VALUE_TYPES
#undef MOPR_APPLY_TYPE

#endif   // MOPR_WRAP_USD_BOX_ARRAY_H
