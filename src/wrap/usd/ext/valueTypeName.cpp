#include "valueTypeName.h"

// Wrap std internal includes.
#include "wrap/std/box/string.h"

// Wrap internal includes.
#include "wrap/usd/box/token.h"
#include "wrap/usd/box/valueTypeName.h"

#include "pxr/usd/sdf/schema.h"

MOPR_DEFINE_RAII_FUNCTIONS( MoprValueTypeName, value_type_name )

/* Constructor */

void
 mopr_value_type_name_ctor( MoprValueTypeName_h this_h )
{
    this_h->d = pxr::SdfValueTypeName( );
}

void
 mopr_value_type_name_find_cstr( MoprValueTypeName_h this_h, char const * type_name_cstr )
{
    const std::string & type_name{ type_name_cstr };
    this_h->d = pxr::SdfSchema::GetInstance( ).FindType( type_name );
}

/* Query */

MOPR_BOOL
 mopr_value_type_name_is_empty_p( MoprValueTypeName_ch this_ch )
{
    return !this_ch->d;
}

MOPR_BOOL
 mopr_value_type_name_is_scalar_p( MoprValueTypeName_ch this_ch )
{
    return this_ch->d.IsScalar( );
}

MOPR_BOOL
 mopr_value_type_name_is_array_p( MoprValueTypeName_ch this_ch )
{
    return this_ch->d.IsArray( );
}

MOPR_BOOL
 mopr_value_type_name_get_scalar_type( MoprValueTypeName_h other_h,
                                       MoprValueTypeName_ch this_ch )
{
    if ( mopr_value_type_name_is_empty_p( this_ch ) )
    {
        return false;
    }

    other_h->d = this_ch->d.GetScalarType( );
    return true;
}

MOPR_BOOL
 mopr_value_type_name_get_array_type( MoprValueTypeName_h other_h,
                                      MoprValueTypeName_ch this_ch )
{
    if ( mopr_value_type_name_is_empty_p( this_ch ) )
    {
        return false;
    }

    other_h->d = this_ch->d.GetArrayType( );
    return true;
}

MOPR_BOOL
 mopr_value_type_name_get_role( MoprToken_h token_h, MoprValueTypeName_ch this_ch )
{
    if ( mopr_value_type_name_is_empty_p( this_ch ) )
    {
        return false;
    }

    token_h->d = this_ch->d.GetRole( );
    return true;
}

MOPR_BOOL
 mopr_value_type_name_get_as_token( MoprToken_h token_h, MoprValueTypeName_ch this_ch )
{
    if ( mopr_value_type_name_is_empty_p( this_ch ) )
    {
        return false;
    }

    token_h->d = this_ch->d.GetAsToken( );
    return true;
}

unsigned char
 mopr_value_type_name_get_dimension( MoprValueTypeName_ch this_ch, int index )
{
    if ( mopr_value_type_name_is_empty_p( this_ch ) )
    {
        return 0;
    }

    return this_ch->d.GetDimensions( ).d[ index ];
}
