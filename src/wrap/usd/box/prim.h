#ifndef MOPR_WRAP_USD_BOX_PRIM_H
#define MOPR_WRAP_USD_BOX_PRIM_H

#include "wrap/_base/box/generic.h"

#include "pxr/usd/usd/prim.h"

struct MOPR_API MoprPrim : public MoprGeneric
{
    pxr::UsdPrim d;
};

#endif   // MOPR_WRAP_USD_BOX_PRIM_H
