#include "schemaTypeSet.h"

// Wrap internal includes.
#include "wrap/usd/box/schemaInfo.h"
#include "wrap/usd/box/schemaTypeSet.h"

#include "pxr/base/plug/plugin.h"
#include "pxr/base/plug/registry.h"
#include "pxr/usd/usd/apiSchemaBase.h"
#include "pxr/usd/usd/schemaRegistry.h"
#include "pxr/usd/usd/typed.h"

#include <set>

MOPR_DEFINE_RAII_FUNCTIONS( MoprSchemaTypeSet, schema_type_set )

/* Constructor */

void
 mopr_schema_type_set_ctor( MoprSchemaTypeSet_h this_h )
{
    this_h->d.clear( );
}

void
 mopr_schema_type_set_ctor_isa_derived( MoprSchemaTypeSet_h this_h )
{
    const auto & preg = pxr::PlugRegistry::GetInstance( );
    std::set< pxr::TfType > types;
    preg.GetAllDerivedTypes< pxr::UsdTyped >( &types );
    this_h->d = { types.cbegin( ), types.cend( ) };
}

void
 mopr_schema_type_set_ctor_api_derived( MoprSchemaTypeSet_h this_h )
{
    const auto & preg = pxr::PlugRegistry::GetInstance( );
    std::set< pxr::TfType > types;
    preg.GetAllDerivedTypes< pxr::UsdAPISchemaBase >( &types );
    this_h->d = { types.cbegin( ), types.cend( ) };
}

/* Query */

int
 mopr_schema_type_set_get_type_count( MoprSchemaTypeSet_ch this_ch )
{
    return this_ch->d.size( );
}

_Bool
 mopr_schema_type_set_get_schema_info( MoprSchemaInfo_h info_h,
                                       MoprSchemaTypeSet_ch this_ch,
                                       int index )
{
    const pxr::UsdSchemaRegistry & reg = pxr::UsdSchemaRegistry::GetInstance( );
    info_h->p = reg.FindSchemaInfo( this_ch->d[ index ] );
    return info_h->p;
}

_Bool
 mopr_schema_type_set_is_empty_p( MoprSchemaTypeSet_ch this_ch )
{
    return this_ch->d.empty( );
}
