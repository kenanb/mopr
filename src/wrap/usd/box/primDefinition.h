#ifndef MOPR_WRAP_USD_BOX_PRIM_DEFINITION_H
#define MOPR_WRAP_USD_BOX_PRIM_DEFINITION_H

#include "wrap/_base/box/generic.h"

#include "pxr/usd/usd/primDefinition.h"

struct MOPR_API MoprPrimDefinition : public MoprGeneric
{
    const pxr::UsdPrimDefinition * p = nullptr;
};

#endif   // MOPR_WRAP_USD_BOX_PRIM_DEFINITION_H
