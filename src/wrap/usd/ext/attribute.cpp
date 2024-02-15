#include "attribute.h"

// Wrap internal includes.
#include "wrap/usd/box/attribute.h"
#include "wrap/usd/box/timecode.h"
#include "wrap/usd/box/value.h"

MOPR_DEFINE_RAII_FUNCTIONS( MoprAttribute, attribute )

/* Constructor */

void
 mopr_attribute_ctor( MoprAttribute_h this_h )
{
    this_h->d = pxr::UsdAttribute( );
}

/* Query */

_Bool
 mopr_attribute_is_empty_p( MoprAttribute_ch this_ch )
{
    return this_ch->d.IsValid( );
}

_Bool
 mopr_attribute_set_value( MoprAttribute_ch this_ch,
                           MoprValue_ch value_ch,
                           MoprTimecode_ch timecode_ch )
{
    this_ch->d.Set( value_ch->d, timecode_ch->d );
    return !mopr_attribute_is_empty_p( this_ch );
}
