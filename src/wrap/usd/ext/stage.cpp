#include "stage.h"

// Wrap internal includes.
#include "wrap/usd/box/layer.h"
#include "wrap/usd/box/path.h"
#include "wrap/usd/box/prim.h"
#include "wrap/usd/box/stage.h"

// OpenUSD includes.
#include "pxr/usd/usdGeom/cube.h"
#include "pxr/usd/usdGeom/mesh.h"

MOPR_DEFINE_RAII_FUNCTIONS( MoprStage, stage )

_Bool
 mopr_stage_is_empty_p( MoprStage_ch this_ch )
{
    return this_ch->IsEmpty( );
}

_Bool
 mopr_stage_try_upgrade( MoprStage_h this_h )
{
    return this_h->TryUpgrade( );
}

void
 mopr_stage_downgrade( MoprStage_h this_h )
{
    this_h->Downgrade( );
}

_Bool
 mopr_stage_create_new( MoprStage_h this_h,
                        char const * id_cstr   // ,
                        /* InitialLoadSet load=LoadAll */ )
{
    this_h->SetRefPtr( pxr::UsdStage::CreateNew( id_cstr ) );
    return this_h->rp;
}

_Bool
 mopr_stage_open_layer( MoprStage_h this_h, MoprLayer_h layer_h )
{
    if ( !layer_h->AssertCallReady( ) ) return false;

    this_h->SetRefPtr( pxr::UsdStage::Open( layer_h->rp ) );
    return this_h->rp;
}

_Bool
 mopr_stage_get_root_layer_w( MoprLayer_h layer_h, MoprStage_ch this_ch )
{
    if ( !this_ch->AssertCallReady( ) ) return false;

    layer_h->SetWeakPtr( this_ch->rp->GetRootLayer( ) );
    return !mopr_layer_is_empty_p( layer_h );
}

_Bool
 mopr_stage_define_prim( MoprPrim_h prim_h, MoprStage_h this_h, MoprPath_ch path_ch )
{
    if ( !this_h->AssertCallReady( ) ) return false;

    prim_h->d = this_h->rp->DefinePrim( path_ch->d );

    return !mopr_prim_is_empty_p( prim_h );
}

_Bool
 mopr_stage_override_prim( MoprPrim_h prim_h, MoprStage_h this_h, MoprPath_ch path_ch )
{
    if ( !this_h->AssertCallReady( ) ) return false;

    prim_h->d = this_h->rp->OverridePrim( path_ch->d );

    return !mopr_prim_is_empty_p( prim_h );
}

_Bool
 mopr_stage_create_class_prim( MoprPrim_h prim_h,
                               MoprStage_h this_h,
                               MoprPath_ch path_ch )
{
    if ( !this_h->AssertCallReady( ) ) return false;

    prim_h->d = this_h->rp->CreateClassPrim( path_ch->d );

    return !mopr_prim_is_empty_p( prim_h );
}

_Bool
 mopr_stage_get_prim_at_path( MoprPrim_h prim_h,
                              MoprStage_ch this_ch,
                              MoprPath_ch path_ch )
{
    if ( !this_ch->AssertCallReady( ) ) return false;

    prim_h->d = this_ch->rp->GetPrimAtPath( path_ch->d );

    return !mopr_prim_is_empty_p( prim_h );
}
