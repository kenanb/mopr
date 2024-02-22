#ifndef MOPR_WRAP_USD_BOX_PROPERTY_DEFINITION_H
#define MOPR_WRAP_USD_BOX_PROPERTY_DEFINITION_H

#include "wrap/_base/box/generic.h"

#include "pxr/usd/usd/primDefinition.h"

struct MOPR_API MoprPropertyDefinition : public MoprGeneric
{
    pxr::UsdPrimDefinition::Property d;
};

#endif   // MOPR_WRAP_USD_BOX_PROPERTY_DEFINITION_H
