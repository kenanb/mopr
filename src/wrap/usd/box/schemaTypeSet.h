#ifndef MOPR_WRAP_USD_BOX_SCHEMA_TYPE_SET_H
#define MOPR_WRAP_USD_BOX_SCHEMA_TYPE_SET_H

#include "wrap/_base/box/generic.h"

#include "pxr/base/tf/type.h"

#include <vector>

struct MOPR_API MoprSchemaTypeSet : public MoprGeneric
{
    std::vector< pxr::TfType > d;
};

#endif   // MOPR_WRAP_USD_BOX_SCHEMA_TYPE_SET_H
