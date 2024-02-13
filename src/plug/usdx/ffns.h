#ifndef MOPR_FFNS_H
#define MOPR_FFNS_H

#include "pxr/base/arch/export.h"
#include "pxr/base/tf/declarePtrs.h"

#define MOPR_DECLARE_WEAK_AND_REF_PTRS( type )                                           \
    typedef PXR_NS::TfDeclarePtrs< class type >::Ptr type##Ptr;                          \
    typedef PXR_NS::TfDeclarePtrs< class type >::ConstPtr type##ConstPtr;                \
    typedef PXR_NS::TfDeclarePtrs< class type >::PtrVector type##PtrVector;              \
    typedef PXR_NS::TfDeclarePtrs< class type >::ConstPtrVector type##ConstPtrVector;    \
    typedef PXR_NS::TfDeclarePtrs< class type >::RefPtr type##RefPtr;                    \
    typedef PXR_NS::TfDeclarePtrs< class type >::ConstRefPtr type##ConstRefPtr;          \
    typedef PXR_NS::TfDeclarePtrs< class type >::RefPtrVector type##RefPtrVector;        \
    typedef PXR_NS::TfDeclarePtrs< class type >::ConstRefPtrVector type##ConstRefPtrVector

#define MOPR_SDF_FILE_FORMAT_FACTORY_ACCESS                                              \
    template < typename T >                                                              \
    friend class PXR_INTERNAL_NS::Sdf_FileFormatFactory

#endif
