#ifndef MOPR_WRAP_USD_BOX_LAYER_H
#define MOPR_WRAP_USD_BOX_LAYER_H

#include "wrap/usd/box/_smart.h"

#include "pxr/usd/sdf/layer.h"

#include <iostream>

struct MOPR_API MoprLayer : public MoprPairedSmartPtr< pxr::SdfLayer >
{
};

#endif   // MOPR_WRAP_USD_BOX_LAYER_H
