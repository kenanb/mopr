#ifndef MOPR_WRAP__BASE_EXT_TYPES_H
#define MOPR_WRAP__BASE_EXT_TYPES_H

#include "wrap/_base/ext/prolog.h"

/*
#define MOPR_SCALAR_VALUE_TYPES                                                          \
    MOPR_APPLY_TYPE( Bool, bool, bool, ( ) )                                             \
    MOPR_APPLY_TYPE( UChar, uchar, unsigned char, ( ) )                                  \
    MOPR_APPLY_TYPE( Int, int, int, ( ) )                                                \
    MOPR_APPLY_TYPE( UInt, uint, unsigned int, ( ) )                                     \
    MOPR_APPLY_TYPE( Int64, int64, int64_t, ( ) )                                        \
    MOPR_APPLY_TYPE( UInt64, uint64, uint64_t, ( ) )                                     \
    MOPR_APPLY_TYPE( Half, half, pxr::GfHalf, ( ) )                                      \
    MOPR_APPLY_TYPE( Float, float, float, ( ) )                                          \
    MOPR_APPLY_TYPE( Double, double, double, ( ) )                                       \
    MOPR_APPLY_TYPE( TimeCode, timecode, pxr::SdfTimeCode, ( ) )                         \
    MOPR_APPLY_TYPE( String, string, std::string, ( ) )                                  \
    MOPR_APPLY_TYPE( Token, token, pxr::TfToken, ( ) )                                   \
    MOPR_APPLY_TYPE( Asset, asset, pxr::SdfAssetPath, ( ) )                              \
    MOPR_APPLY_TYPE( Opaque, opaque, pxr::SdfOpaqueValue, ( ) )                          \
    MOPR_APPLY_TYPE( PathExpression, pathExpression, pxr::SdfPathExpression, ( ) )

#define MOPR_VECTOR_VALUE_TYPES                                                          \
    MOPR_APPLY_TYPE( Matrix2d, matrix2d, pxr::GfMatrix2d, ( 2, 2 ) )                     \
    MOPR_APPLY_TYPE( Matrix3d, matrix3d, pxr::GfMatrix3d, ( 3, 3 ) )                     \
    MOPR_APPLY_TYPE( Matrix4d, matrix4d, pxr::GfMatrix4d, ( 4, 4 ) )                     \
    MOPR_APPLY_TYPE( Quath, quath, pxr::GfQuath, ( 4 ) )                                 \
    MOPR_APPLY_TYPE( Quatf, quatf, pxr::GfQuatf, ( 4 ) )                                 \
    MOPR_APPLY_TYPE( Quatd, quatd, pxr::GfQuatd, ( 4 ) )                                 \
    MOPR_APPLY_TYPE( Int2, int2, pxr::GfVec2i, ( 2 ) )                                   \
    MOPR_APPLY_TYPE( Half2, half2, pxr::GfVec2h, ( 2 ) )                                 \
    MOPR_APPLY_TYPE( Float2, float2, pxr::GfVec2f, ( 2 ) )                               \
    MOPR_APPLY_TYPE( Double2, double2, pxr::GfVec2d, ( 2 ) )                             \
    MOPR_APPLY_TYPE( Int3, int3, pxr::GfVec3i, ( 3 ) )                                   \
    MOPR_APPLY_TYPE( Half3, half3, pxr::GfVec3h, ( 3 ) )                                 \
    MOPR_APPLY_TYPE( Float3, float3, pxr::GfVec3f, ( 3 ) )                               \
    MOPR_APPLY_TYPE( Double3, double3, pxr::GfVec3d, ( 3 ) )                             \
    MOPR_APPLY_TYPE( Int4, int4, pxr::GfVec4i, ( 4 ) )                                   \
    MOPR_APPLY_TYPE( Half4, half4, pxr::GfVec4h, ( 4 ) )                                 \
    MOPR_APPLY_TYPE( Float4, float4, pxr::GfVec4f, ( 4 ) )                               \
    MOPR_APPLY_TYPE( Double4, double4, pxr::GfVec4d, ( 4 ) )
*/

// TEMPORARILY LIMIT TYPES TO THE ONES BEING NEEDED EARLY.
// TODO : Enable other types.

#define MOPR_SCALAR_VALUE_TYPES                                                          \
    MOPR_APPLY_TYPE( Bool, bool, bool, ( ), MOPR_BOOL * )                                \
    MOPR_APPLY_TYPE( UChar, uchar, unsigned char, ( ), unsigned char * )                 \
    MOPR_APPLY_TYPE( Int, int, int, ( ), int * )                                         \
    MOPR_APPLY_TYPE( UInt, uint, unsigned int, ( ), unsigned int * )                     \
    MOPR_APPLY_TYPE( Int64, int64, int64_t, ( ), long * )                                \
    MOPR_APPLY_TYPE( UInt64, uint64, uint64_t, ( ), unsigned long * )                    \
    MOPR_APPLY_TYPE( Float, float, float, ( ), float * )                                 \
    MOPR_APPLY_TYPE( Double, double, double, ( ), double * )                             \
    MOPR_APPLY_TYPE( TimeCode, timecode, pxr::SdfTimeCode, ( ), double * )               \
    MOPR_APPLY_TYPE( Token, token, pxr::TfToken, ( ), MoprToken_h )

#define MOPR_VECTOR_VALUE_TYPES                                                          \
    MOPR_APPLY_TYPE( Matrix2d, matrix2d, pxr::GfMatrix2d, ( 2, 2 ), double * )           \
    MOPR_APPLY_TYPE( Matrix3d, matrix3d, pxr::GfMatrix3d, ( 3, 3 ), double * )           \
    MOPR_APPLY_TYPE( Matrix4d, matrix4d, pxr::GfMatrix4d, ( 4, 4 ), double * )           \
    MOPR_APPLY_TYPE( Quatf, quatf, pxr::GfQuatf, ( 4 ), float * )                        \
    MOPR_APPLY_TYPE( Quatd, quatd, pxr::GfQuatd, ( 4 ), double * )                       \
    MOPR_APPLY_TYPE( Int2, int2, pxr::GfVec2i, ( 2 ), int * )                            \
    MOPR_APPLY_TYPE( Float2, float2, pxr::GfVec2f, ( 2 ), float * )                      \
    MOPR_APPLY_TYPE( Double2, double2, pxr::GfVec2d, ( 2 ), double * )                   \
    MOPR_APPLY_TYPE( Int3, int3, pxr::GfVec3i, ( 3 ), int * )                            \
    MOPR_APPLY_TYPE( Float3, float3, pxr::GfVec3f, ( 3 ), float * )                      \
    MOPR_APPLY_TYPE( Double3, double3, pxr::GfVec3d, ( 3 ), double * )                   \
    MOPR_APPLY_TYPE( Int4, int4, pxr::GfVec4i, ( 4 ), int * )                            \
    MOPR_APPLY_TYPE( Float4, float4, pxr::GfVec4f, ( 4 ), float * )                      \
    MOPR_APPLY_TYPE( Double4, double4, pxr::GfVec4d, ( 4 ), double * )

#endif   // MOPR_WRAP__BASE_EXT_TYPES_H
