#ifndef MOPR_WRAP_USD_BOX_STAGE_H
#define MOPR_WRAP_USD_BOX_STAGE_H

#include "wrap/usd/box/_smart.h"

#include "pxr/usd/usd/stage.h"

#include <iostream>

struct MOPR_API MoprStage
    : public MoprPairedSmartPtr< pxr::UsdStage >
{
};

#endif   // MOPR_WRAP_USD_BOX_STAGE_H
