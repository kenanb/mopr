#ifndef MOPR_WRAP_USD_BOX_VALUE_H
#define MOPR_WRAP_USD_BOX_VALUE_H

#include "wrap/_base/box/generic.h"

#include "pxr/base/vt/value.h"

struct MOPR_API MoprValue : public MoprGeneric
{
    pxr::VtValue d;
};

#endif   // MOPR_WRAP_USD_BOX_VALUE_H
