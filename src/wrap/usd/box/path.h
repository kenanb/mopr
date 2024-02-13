#ifndef MOPR_WRAP_USD_BOX_PATH_H
#define MOPR_WRAP_USD_BOX_PATH_H

#include "wrap/_base/box/generic.h"

#include "pxr/usd/sdf/path.h"

struct MOPR_API MoprPath : public MoprGeneric
{
    pxr::SdfPath d;
};

#endif   // MOPR_WRAP_USD_BOX_PATH_H
