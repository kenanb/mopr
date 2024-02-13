#ifndef MOPR_WRAP_USD_BOX_VALUE_TYPE_NAME_H
#define MOPR_WRAP_USD_BOX_VALUE_TYPE_NAME_H

#include "wrap/_base/box/generic.h"

#include "pxr/usd/sdf/valueTypeName.h"

struct MOPR_API MoprValueTypeName : public MoprGeneric
{
    pxr::SdfValueTypeName d;
};

#endif   // MOPR_WRAP_USD_BOX_VALUE_TYPE_NAME_H
