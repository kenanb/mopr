#include "propertyDefinition.h"

// Wrap internal includes.
#include "wrap/usd/box/propertyDefinition.h"
#include "wrap/usd/box/token.h"
#include "wrap/usd/box/valueTypeName.h"

#include "pxr/usd/usd/primDefinition.h"
#include "pxr/usd/usd/schemaRegistry.h"

MOPR_DEFINE_RAII_FUNCTIONS( MoprPropertyDefinition, property_definition )

/* Constructor */

void
 mopr_property_definition_ctor( MoprPropertyDefinition_h this_h )
{
    this_h->d = { };
}

_Bool
 mopr_property_definition_get_name( MoprToken_h token_h,
                                    MoprPropertyDefinition_ch this_ch )
{
    if ( this_ch->d )
    {
        token_h->d = this_ch->d.GetName( );
        return true;
    }
    else
    {
        return false;
    }
}

_Bool
 mopr_property_definition_is_attribute_p( MoprPropertyDefinition_ch this_ch )
{
    return this_ch->d.IsAttribute( );
}

_Bool
 mopr_property_definition_is_relationship_p( MoprPropertyDefinition_ch this_ch )
{
    return this_ch->d.IsRelationship( );
}

_Bool
 mopr_property_definition_attribute_get_type_name( MoprValueTypeName_h value_tyne_name_h,
                                                   MoprPropertyDefinition_ch this_ch )
{
    if ( this_ch->d && this_ch->d.IsAttribute( ) )
    {
        pxr::UsdPrimDefinition::Attribute attr = this_ch->d;
        value_tyne_name_h->d = attr.GetTypeName( );
        return true;
    }

    return false;
}

_Bool
 mopr_property_definition_attribute_get_type_name_token(
  MoprToken_h token_h, MoprPropertyDefinition_ch this_ch )
{
    if ( this_ch->d && this_ch->d.IsAttribute( ) )
    {
        pxr::UsdPrimDefinition::Attribute attr = this_ch->d;
        token_h->d = attr.GetTypeNameToken( );
        return true;
    }

    return false;
}

/* Query */

_Bool
 mopr_property_definition_is_empty_p( MoprPropertyDefinition_ch this_ch )
{
    return !this_ch->d;
}

enum MoprAttributeVariability
 mopr_property_definition_get_variability( MoprPropertyDefinition_ch this_ch )
{
    switch ( this_ch->d.GetVariability( ) )
    {
        case pxr::SdfVariabilityUniform:
            return MoprAttributeVariabilityUniform;

        case pxr::SdfVariabilityVarying:
        default:
            return MoprAttributeVariabilityVarying;
    }
}
