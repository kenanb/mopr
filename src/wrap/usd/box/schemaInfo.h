#ifndef MOPR_WRAP_USD_BOX_SCHEMA_INFO_H
#define MOPR_WRAP_USD_BOX_SCHEMA_INFO_H

#include "wrap/_base/box/generic.h"

#include "pxr/usd/usd/schemaRegistry.h"

struct MOPR_API MoprSchemaInfo : public MoprGeneric
{
    const pxr::UsdSchemaRegistry::SchemaInfo * p = nullptr;
};

#endif   // MOPR_WRAP_USD_BOX_SCHEMA_INFO_H
