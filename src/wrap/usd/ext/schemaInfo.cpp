#include "schemaInfo.h"

// Wrap internal includes.
#include "wrap/usd/box/schemaInfo.h"
#include "wrap/usd/box/token.h"

MOPR_DEFINE_RAII_FUNCTIONS( MoprSchemaInfo, schema_info )

/* Constructor */

void
 mopr_schema_info_ctor( MoprSchemaInfo_h this_h )
{
    this_h->p = nullptr;
}

/* Query */

enum MoprSchemaKind
 mopr_schema_info_get_kind( MoprSchemaInfo_ch this_ch )
{
    switch ( this_ch->p->kind )
    {
        case pxr::UsdSchemaKind::AbstractBase:
            return MoprSchemaKindAbstractBase;
        case pxr::UsdSchemaKind::AbstractTyped:
            return MoprSchemaKindAbstractTyped;
        case pxr::UsdSchemaKind::ConcreteTyped:
            return MoprSchemaKindConcreteTyped;
        case pxr::UsdSchemaKind::NonAppliedAPI:
            return MoprSchemaKindNonAppliedAPI;
        case pxr::UsdSchemaKind::SingleApplyAPI:
            return MoprSchemaKindSingleApplyAPI;
        case pxr::UsdSchemaKind::MultipleApplyAPI:
            return MoprSchemaKindMultipleApplyAPI;
        case pxr::UsdSchemaKind::Invalid:
        default:
            return MoprSchemaKindInvalid;
    }
}

unsigned int
 mopr_schema_info_get_version( MoprSchemaInfo_ch this_ch )
{
    return this_ch->p->version;
}

void
 mopr_schema_info_get_identifier( MoprToken_h id_h, MoprSchemaInfo_ch this_ch )
{
    id_h->d = this_ch->p->identifier;
}

void
 mopr_schema_info_get_family( MoprToken_h family_h, MoprSchemaInfo_ch this_ch )
{
    family_h->d = this_ch->p->family;
}

MOPR_BOOL
 mopr_schema_info_is_empty_p( MoprSchemaInfo_ch this_ch )
{
    return !this_ch->p;
}
