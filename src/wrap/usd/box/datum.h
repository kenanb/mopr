#ifndef MOPR_WRAP_USD_BOX_DATUM_H
#define MOPR_WRAP_USD_BOX_DATUM_H

#include "wrap/usd/box/datumBase.h"

#include "wrap/_base/ext/types.h"

template < typename T >
struct MOPR_API MoprDatum
    : public MoprDatumGeneric< T >
{
    T d;
    typedef MoprDatumBase< T > base;
    typedef MoprDatumGeneric< T > gen;
    typedef typename gen::PrimitiveAccess primitive_access;
    using MoprDatumBase< T >::dim;

    typename base::target_primitive_type *
     row_major_aref( int i )
    {
        T & ref = d;
        primitive_access * in = reinterpret_cast< primitive_access * >( &ref );
        return &in->t[ i ];
    }
};

#define MOPR_APPLY_TYPE( T_name, T_text, T_type, T_dims, T_prim_h )                      \
    struct MOPR_API MoprDatum_##T_text : public MoprDatum< T_type >                      \
    {                                                                                    \
    };
MOPR_SCALAR_VALUE_TYPES
MOPR_VECTOR_VALUE_TYPES
#undef MOPR_APPLY_TYPE

#endif   // MOPR_WRAP_USD_BOX_DATUM_H
