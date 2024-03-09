#include "primDefinition.h"

// Wrap internal includes.
#include "wrap/usd/box/primDefinition.h"
#include "wrap/usd/box/propertyDefinition.h"
#include "wrap/usd/box/token.h"

#include "pxr/usd/usd/primDefinition.h"
#include "pxr/usd/usd/schemaRegistry.h"

MOPR_DEFINE_RAII_FUNCTIONS( MoprPrimDefinition, prim_definition )

/* Constructor */

void
 mopr_prim_definition_ctor_api( MoprPrimDefinition_h this_h, MoprToken_ch token_ch )
{
    const pxr::UsdSchemaRegistry & reg = pxr::UsdSchemaRegistry::GetInstance( );
    this_h->p = reg.FindAppliedAPIPrimDefinition( token_ch->d );
}

void
 mopr_prim_definition_ctor_isa( MoprPrimDefinition_h this_h, MoprToken_ch token_ch )
{
    const pxr::UsdSchemaRegistry & reg = pxr::UsdSchemaRegistry::GetInstance( );
    this_h->p = reg.FindConcretePrimDefinition( token_ch->d );
}

int
 mopr_prim_definition_get_property_count( MoprPrimDefinition_ch this_ch )
{
    const auto & pNames = this_ch->p->GetPropertyNames( );
    return pNames.size( );
}

MOPR_BOOL
 mopr_prim_definition_get_property_name( MoprToken_h token_h,
                                         MoprPrimDefinition_ch this_ch,
                                         int i )
{
    const auto & pNames = this_ch->p->GetPropertyNames( );
    if ( pNames.size( ) > static_cast< size_t >( i ) )
    {
        token_h->d = pNames[ i ];
        return true;
    }
    else
    {
        return false;
    }
}

MOPR_BOOL
 mopr_prim_definition_get_property( MoprPropertyDefinition_h property_h,
                                    MoprPrimDefinition_ch this_ch,
                                    MoprToken_ch token_ch )
{
    property_h->d = this_ch->p->GetPropertyDefinition( token_ch->d );
    return !mopr_property_definition_is_empty_p( property_h );
}

/* Query */

MOPR_BOOL
 mopr_prim_definition_is_empty_p( MoprPrimDefinition_ch this_ch )
{
    return !this_ch->p;
}
