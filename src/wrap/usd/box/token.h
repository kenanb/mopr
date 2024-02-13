#ifndef MOPR_WRAP_USD_BOX_TOKEN_H
#define MOPR_WRAP_USD_BOX_TOKEN_H

#include "wrap/_base/box/generic.h"

#include "pxr/base/tf/token.h"

struct MOPR_API MoprToken : public MoprGeneric
{
    pxr::TfToken d;
};

#endif   // MOPR_WRAP_USD_BOX_TOKEN_H
