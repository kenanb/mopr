#ifndef MOPR_WRAP_USD_BOX_DATUM_BASE_H
#define MOPR_WRAP_USD_BOX_DATUM_BASE_H

#include "wrap/_base/box/generic.h"

#include "wrap/usd/box/timecode.h"
#include "wrap/usd/box/token.h"

#include "wrap/_base/ext/types.h"

#include "pxr/base/gf/half.h"
#include "pxr/base/gf/matrix2d.h"
#include "pxr/base/gf/matrix3d.h"
#include "pxr/base/gf/matrix4d.h"
#include "pxr/base/gf/quatd.h"
#include "pxr/base/gf/quatf.h"
#include "pxr/base/gf/quath.h"
#include "pxr/base/gf/vec2d.h"
#include "pxr/base/gf/vec2f.h"
#include "pxr/base/gf/vec2h.h"
#include "pxr/base/gf/vec2i.h"
#include "pxr/base/gf/vec3d.h"
#include "pxr/base/gf/vec3f.h"
#include "pxr/base/gf/vec3h.h"
#include "pxr/base/gf/vec3i.h"
#include "pxr/base/gf/vec4d.h"
#include "pxr/base/gf/vec4f.h"
#include "pxr/base/gf/vec4h.h"
#include "pxr/base/gf/vec4i.h"
#include "pxr/base/tf/token.h"
#include "pxr/usd/sdf/assetPath.h"
#include "pxr/usd/sdf/opaqueValue.h"
#include "pxr/usd/sdf/pathExpression.h"
#include "pxr/usd/sdf/timeCode.h"

#include <string>

template < typename T, typename Enable = void >
struct MOPR_API MoprDatumBase : public MoprGeneric
{
    typedef T source_primitive_type;
    typedef T target_primitive_type;
    static constexpr const size_t dim = 1;
};

template <>
struct MOPR_API MoprDatumBase< pxr::TfToken > : public MoprGeneric
{
    typedef pxr::TfToken source_primitive_type;
    typedef struct MoprToken target_primitive_type;
    static constexpr const size_t dim = 1;
};

template <>
struct MOPR_API MoprDatumBase< pxr::SdfTimeCode > : public MoprGeneric
{
    typedef pxr::SdfTimeCode source_primitive_type;
    typedef struct MoprTimecode target_primitive_type;
    static constexpr const size_t dim = 1;
};

template < typename T >
struct MOPR_API
 MoprDatumBase< T, typename std::enable_if< pxr::GfIsGfVec< T >::value >::type >
    : public MoprGeneric
{
    typedef typename T::ScalarType source_primitive_type;
    typedef typename T::ScalarType target_primitive_type;
    static constexpr const size_t dim = T::dimension;
};

template < typename T >
struct MOPR_API
 MoprDatumBase< T, typename std::enable_if< pxr::GfIsGfMatrix< T >::value >::type >
    : public MoprGeneric
{
    typedef typename T::ScalarType source_primitive_type;
    typedef typename T::ScalarType target_primitive_type;
    static constexpr const size_t col = T::numColumns;
    static constexpr const size_t row = T::numRows;
    static constexpr const size_t dim = col * row;
};

template < typename T >
struct MOPR_API
 MoprDatumBase< T, typename std::enable_if< pxr::GfIsGfQuat< T >::value >::type >
    : public MoprGeneric
{
    typedef typename T::ScalarType source_primitive_type;
    typedef typename T::ScalarType target_primitive_type;
    static constexpr const size_t dim = 4;
};

template < typename T >
struct MOPR_API MoprDatumGeneric : public MoprDatumBase< T >
{
    typedef MoprDatumBase< T > base;

    union PrimitiveAccess
    {
        T p;
        typename base::source_primitive_type s[ base::dim ];
        typename base::target_primitive_type t[ base::dim ];
    };

    static constexpr int
     get_dim( )
    {
        return base::dim;
    }

    static constexpr int
     get_primitive_sizeof( )
    {
        return sizeof( typename base::source_primitive_type );
    }

    static constexpr int
     get_primitive_alignof( )
    {
        return alignof( typename base::source_primitive_type );
    }
};

#endif   // MOPR_WRAP_USD_BOX_DATUM_BASE_H
