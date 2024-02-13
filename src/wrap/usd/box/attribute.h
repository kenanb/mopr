#ifndef MOPR_WRAP_USD_BOX_ATTRIBUTE_H
#define MOPR_WRAP_USD_BOX_ATTRIBUTE_H

#include "wrap/_base/box/generic.h"

#include "pxr/usd/usd/attribute.h"

struct MOPR_API MoprAttribute : public MoprGeneric
{
    pxr::UsdAttribute d;
};

#endif   // MOPR_WRAP_USD_BOX_ATTRIBUTE_H
