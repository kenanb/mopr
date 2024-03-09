#include "prim.h"

// Wrap internal includes.
#include "wrap/usd/box/attribute.h"
#include "wrap/usd/box/prim.h"
#include "wrap/usd/box/token.h"
#include "wrap/usd/box/valueTypeName.h"

MOPR_DEFINE_RAII_FUNCTIONS( MoprPrim, prim )

/* Constructor */

void
 mopr_prim_ctor( MoprPrim_h this_h )
{
    this_h->d = pxr::UsdPrim( );
}

/* Query */

MOPR_BOOL
 mopr_prim_is_empty_p( MoprPrim_ch this_ch )
{
    return this_ch->d.IsValid( );
}

void
 mopr_prim_set_type_name( MoprPrim_h this_h, MoprToken_ch token_ch )
{
    this_h->d.SetTypeName( token_ch->d );
}

MOPR_BOOL
 mopr_prim_create_attribute( MoprAttribute_h attribute_h,
                             MoprPrim_ch this_ch,
                             MoprToken_ch name_token_ch,
                             MoprValueTypeName_ch value_type_name_ch,
                             MOPR_BOOL custom,
                             MoprPropertyVariability variability )
{
    pxr::SdfVariability vr;

    switch ( variability )
    {
        case MoprPropertyVariabilityUniform:
            vr = pxr::SdfVariabilityUniform;
            break;

        case MoprPropertyVariabilityVarying:
        default:
            vr = pxr::SdfVariabilityVarying;
            break;
    }

    attribute_h->d =
     this_ch->d.CreateAttribute( name_token_ch->d, value_type_name_ch->d, custom, vr );

    return !mopr_attribute_is_empty_p( attribute_h );
}
