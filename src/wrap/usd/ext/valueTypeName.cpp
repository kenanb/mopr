#include "valueTypeName.h"

// Wrap std internal includes.
#include "wrap/std/box/string.h"

// Wrap internal includes.
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

_Bool
 mopr_value_type_name_is_empty_p( MoprValueTypeName_ch this_ch )
{
    return bool( this_ch->d );
}
