#include "layer.h"

// Wrap std internal includes.
#include "wrap/std/box/string.h"

// Wrap internal includes.
#include "wrap/usd/box/layer.h"
#include "wrap/usd/box/path.h"

MOPR_DEFINE_RAII_FUNCTIONS( MoprLayer, layer )

MOPR_BOOL mopr_layer_is_empty_p( MoprLayer_ch this_ch )
{
    return this_ch->IsEmpty( );
}

MOPR_BOOL
 mopr_layer_try_upgrade( MoprLayer_h this_h )
{
    return this_h->TryUpgrade( );
}

void
 mopr_layer_downgrade( MoprLayer_h this_h )
{
    this_h->Downgrade( );
}

MOPR_BOOL
 mopr_layer_create_new( MoprLayer_h this_h,
                        char const * id_cstr   // ,
                        /* const FileFormatArguments &args=FileFormatArguments() */ )
{
    this_h->SetRefPtr( pxr::SdfLayer::CreateNew( id_cstr ) );
    return this_h->rp;
}

MOPR_BOOL
 mopr_layer_create_anonymous(
  MoprLayer_h this_h,
  char const * id_cstr   // ,
  /* const FileFormatArguments &args=FileFormatArguments() */ )
{
    this_h->SetRefPtr( pxr::SdfLayer::CreateAnonymous( id_cstr ) );
    return this_h->rp;
}

MOPR_BOOL
 mopr_layer_export_to_string( MoprString_h o_string_h, MoprLayer_ch this_ch )
{
    if ( !this_ch->AssertCallReady( ) ) return false;

    this_ch->rp->ExportToString( &o_string_h->d );

    return true;
}

MOPR_BOOL
 mopr_layer_get_identifier( MoprString_h id_string_h, MoprLayer_ch this_ch )
{
    if ( !this_ch->AssertCallReady( ) ) return false;

    id_string_h->d = this_ch->rp->GetIdentifier( );

    return true;
}

MOPR_BOOL
 mopr_layer_save( MoprLayer_ch this_ch )
{
    if ( !this_ch->AssertCallReady( ) ) return false;

    return this_ch->rp->Save( );
}
